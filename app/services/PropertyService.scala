/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services

import cats.data.EitherT
import cats.syntax.either._
import connectors.IntegrationFrameworkConnector
import models.ITPEnvelope.ITPEnvelope
import models.common._
import models.domain.JourneyAnswers
import models.errors._
import models.request._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba.{EsbaInUpstream, EsbaInfo, EsbaInfoToSave}
import models.responses._
import models.{ExpensesStoreAnswers, ITPEnvelope, PropertyPeriodicSubmissionResponse, RentalAllowancesStoreAnswers}
import play.api.libs.Files.logger
import play.api.libs.json.{JsValue, Json, Writes}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Period
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PropertyService @Inject() (connector: IntegrationFrameworkConnector, repository: MongoJourneyAnswersRepository)(
  implicit ec: ExecutionContext
) {

  def saveIncome(journeyContext: JourneyContext, nino: Nino, incomeToSave: Income, saveIncome: SaveIncome)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {

      currentPeriodicSubmission <- getCurrentPeriodicSubmission(
                                     journeyContext.taxYear.endYear,
                                     nino.value,
                                     journeyContext.incomeSourceId.value
                                   )
      ppsr <- ITPEnvelope.liftEither(
                PropertyPeriodicSubmissionRequest.fromUkOtherPropertyIncome(currentPeriodicSubmission, saveIncome)
              )

      r <- currentPeriodicSubmission match {
             case None =>
               createPeriodicSubmission(
                 nino.value,
                 journeyContext.incomeSourceId.value,
                 journeyContext.taxYear.endYear,
                 ppsr
               )
             case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _, _, _)) =>
               updatePeriodicSubmission(
                 journeyContext.mtditid.value,
                 journeyContext.incomeSourceId.value,
                 journeyContext.taxYear.endYear,
                 submissionId.submissionId,
                 ppsr
               ).map(_ => Some(submissionId))
             case _ =>
               ITPEnvelope.liftEither(InternalError("No submission id fetched").asLeft[Option[PeriodicSubmissionId]])
           }
      _ <- persistAnswers(journeyContext, incomeToSave).map(isPersistSuccess =>
             if (!isPersistSuccess) {
               logger.error("Could not persist")
             } else {
               logger.info("Persist successful")
             }
           )
    } yield r

  import models.repository.Merger._

  def getFetchedPropertyDataMerged(
    ctx: JourneyContext,
    nino: Nino,
    incomeSourceId: String
  )(implicit
    ec: ExecutionContext,
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, FetchedPropertyData] = {
    val result = getPropertyAnnualSubmission(ctx.taxYear.endYear, nino.toString, incomeSourceId)

    for {
      resultFromDownstream <- result
      resultFromRepository <- fetchAllJourneyDataFromRepository(ctx) // ToDo, make a proper repo error?
    } yield mergeAll(resultFromDownstream, resultFromRepository)

  }

  private def fetchAllJourneyDataFromRepository(ctx: JourneyContext): ITPEnvelope[Map[String, JourneyAnswers]] = {
    val result: Future[Either[ServiceError, Map[String, JourneyAnswers]]] = if (ctx.journey == JourneyName.NoJourney) {
      Future.successful(
        InternalError(s"Journey Repo 'should' not be accessed, journey name: ${ctx.journey.entryName}")
          .asLeft[Map[String, JourneyAnswers]]
      )
    } else {
      repository
        .fetchAllJourneys(ctx)
        .map(ja => getValidJourneysPerJourneyName(ja.toList))
    }
    ITPEnvelope.liftFuture(result)
  }

  private def getValidJourneysPerJourneyName(
    journeyAnswers: List[JourneyAnswers]
  ): Either[ServiceError, Map[String, JourneyAnswers]] = {
    val journeyAnswersGrouped = journeyAnswers.toList.groupBy(j => j.journey.entryName)
    journeyAnswersGrouped.foldLeft(Map[String, JourneyAnswers]().asRight[ServiceError]) { (acc, kv) =>
      acc match {
        case Right(ja) =>
          val (k, v) = kv
          val r: Either[ServiceError, Map[String, JourneyAnswers]] = if (v.size == 1) {
            (ja + (k -> v(0))).asRight[ServiceError]
          } else {
            RepositoryError.asLeft[Map[String, JourneyAnswers]]
          }
          r
        case left => left
      }
    }

  }

  private def mergeAll(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromRepository: Map[String, JourneyAnswers]
  ): FetchedPropertyData = {
    val esbaInfoMaybe = mergeEsbaInfo(resultFromDownstream, resultFromRepository.get(JourneyName.RentalESBA.entryName))
    val propertyAboutMaybe = mergePropertyAbout(resultFromRepository.get(JourneyName.About.entryName))
    val adjustmentsMaybe =
      mergeAdjustments(resultFromDownstream, resultFromRepository.get(JourneyName.RentalAdjustments.entryName))
    FetchedPropertyData(None, propertyAboutMaybe, adjustmentsMaybe, esbaInfoMaybe)
  }

  private def mergePropertyAbout(resultFromRepository: Option[JourneyAnswers]): Option[PropertyAbout] =
    resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[PropertyAbout])
      case None                 => None
    }

  private def mergeAdjustments(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromRepository: Option[JourneyAnswers]
  ): Option[PropertyRentalAdjustments] = {

    // Todo: What to do with None None (In case we do not receive any info from db and downstream?
    val adjustments: Option[UkOtherAdjustments] = for {
      uop   <- resultFromDownstream.ukOtherProperty
      uopaa <- uop.ukOtherPropertyAnnualAdjustments
    } yield uopaa

    val adjustmentStoreAnswers: Option[AdjustmentStoreAnswers] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[AdjustmentStoreAnswers])
      case None                 => None
    }
    adjustmentStoreAnswers.merge(adjustments)
  }

  private def mergeEsbaInfo(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromRepository: Option[JourneyAnswers]
  ): Option[EsbaInfo] = {
    val esbasMaybe: Option[List[Esba]] = for {
      ukop   <- resultFromDownstream.ukOtherProperty
      ukopaa <- ukop.ukOtherPropertyAnnualAllowances
      esba   <- ukopaa.enhancedStructuredBuildingAllowance
    } yield esba.toList

    val esbasInRequestMaybe = esbasMaybe.map(
      _.map(e =>
        EsbaInUpstream(
          // Todo: Remove .get's, but again, they are mandatory on frontend.
          // Todo: What to do if None comes from downstream?
          e.firstYear.get.qualifyingDate,
          e.firstYear.get.qualifyingAmountExpenditure,
          e.amount,
          Address(
            BuildingName(e.building.name.get),
            BuildingNumber(e.building.number.get),
            Postcode(e.building.postCode)
          )
        )
      )
    )

    // Todo: When giving the data back, what to do in case qualifying date and amounts not present?
    // Todo: They are required on the frontend

    val esbaInfoSavedInRepositoryMaybe: Option[EsbaInfoToSave] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[EsbaInfoToSave])
      case None                 => None
    }

    esbaInfoSavedInRepositoryMaybe.merge(esbasInRequestMaybe)
  }

  def getPropertyPeriodicSubmissions(taxYear: Int, taxableEntityId: String, incomeSourceId: String)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[PropertyPeriodicSubmissionResponse] = {

    val result: ITPEnvelope[List[PropertyPeriodicSubmission]] =
      for {
        periodicSubmissionIds <- EitherT(connector.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId))
                                   .leftMap(error => ApiServiceError(error.status))
        propertyPeriodicSubmissions <-
          getPropertySubmissions(taxYear, taxableEntityId, incomeSourceId, periodicSubmissionIds)
      } yield propertyPeriodicSubmissions

    result.subflatMap(propertyPeriodicSubmissionList => transformToResponse(propertyPeriodicSubmissionList))
  }

  def saveExpenses(ctx: JourneyContext, nino: Nino, expenses: Expenses)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {
      maybePeriodicSubmission <- getCurrentPeriodicSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value)
      periodicSubmissionRequest <-
        ITPEnvelope.liftEither(PropertyPeriodicSubmissionRequest.fromExpenses(maybePeriodicSubmission, expenses))
      submissionResponse <- maybePeriodicSubmission match {
                              case None =>
                                createPeriodicSubmission(
                                  nino.value,
                                  ctx.incomeSourceId.value,
                                  ctx.taxYear.endYear,
                                  periodicSubmissionRequest
                                )
                              case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _, _, _)) =>
                                updatePeriodicSubmission(
                                  nino.value,
                                  ctx.incomeSourceId.value,
                                  ctx.taxYear.endYear,
                                  submissionId.submissionId,
                                  periodicSubmissionRequest
                                ).map(_ => Some(submissionId))
                              case _ =>
                                ITPEnvelope.liftEither(
                                  InternalError("No submission id fetched").asLeft[Option[PeriodicSubmissionId]]
                                )
                            }
      _ <- expenses.consolidatedExpenses match {
             case Some(consolidatedExpenses) =>
               persistAnswers(ctx, ExpensesStoreAnswers(consolidatedExpenses.consolidatedExpensesYesOrNo))
             case None =>
               ITPEnvelope.liftPure(None)
           }
    } yield submissionResponse

  def getCurrentPeriodicSubmission(taxYear: Int, taxableEntityId: String, incomeSourceId: String)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Option[PropertyPeriodicSubmission]] =
    getPropertyPeriodicSubmissions(taxYear, taxableEntityId, incomeSourceId)
      .map(_.periodicSubmissions.headOption)
      .flatMap {
        case Some(newest) => ITPEnvelope.liftPure(Some(newest))
        case None         => ITPEnvelope.liftPure(None)
      }

  def getPropertyAnnualSubmission(taxYear: Int, taxableEntityId: String, incomeSourceId: String)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[PropertyAnnualSubmission] =
    EitherT(connector.getPropertyAnnualSubmission(taxYear, taxableEntityId, incomeSourceId))
      .leftMap(error => ApiServiceError(error.status))
      .subflatMap { annualSubmission =>
        annualSubmission.fold[Either[ServiceError, PropertyAnnualSubmission]](
          DataNotFoundError.asLeft[PropertyAnnualSubmission]
        )(_.asRight[ServiceError])
      }

  def deletePropertyAnnualSubmission(incomeSourceId: String, taxableEntityId: String, taxYear: Int)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Unit] =
    EitherT(connector.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear))
      .bimap(error => ApiServiceError(error.status), result => result)

  def createPeriodicSubmission(
    nino: String,
    incomeSourceId: String,
    taxYear: Int,
    body: PropertyPeriodicSubmissionRequest
  )(implicit hc: HeaderCarrier): ITPEnvelope[Option[PeriodicSubmissionId]] =
    EitherT(connector.createPeriodicSubmission(taxYear, nino, incomeSourceId, body)).leftMap(e =>
      ApiServiceError(e.status)
    )

  def updatePeriodicSubmission(
    nino: String,
    incomeSourceId: String,
    taxYear: Int,
    submissionId: String,
    propertyPeriodicSubmissionRequest: PropertyPeriodicSubmissionRequest
  )(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[String] =
    EitherT(
      connector.updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, propertyPeriodicSubmissionRequest)
    )
      .bimap(error => ApiServiceError(error.status), _ => "")

  @Deprecated
  def createOrUpdateAnnualSubmission(nino: String, incomeSourceId: String, taxYear: Int, body: Option[JsValue])(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Unit] =
    EitherT(connector.createOrUpdateAnnualSubmission(taxYear, nino, incomeSourceId, body.get))
      .bimap(error => ApiServiceError(error.status), _ => ())
  def createOrUpdateAnnualSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino,
    body: PropertyAnnualSubmission
  )(implicit hc: HeaderCarrier): ITPEnvelope[Unit] =
    EitherT(
      connector.createOrUpdateAnnualSubmission(taxYear, incomeSourceId, nino, body)
    ).leftMap(e => ApiServiceError(e.status))

  private def getPropertySubmissions(
    taxYear: Int,
    taxableEntityId: String,
    incomeSourceId: String,
    periodicSubmissionIds: List[PeriodicSubmissionIdModel]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): ITPEnvelope[List[PropertyPeriodicSubmission]] = {
    val propertyPeriodicSubmissions = periodicSubmissionIds
      .filter(submissionId => Period.between(submissionId.fromDate, submissionId.toDate).getYears >= 1)
      .map { submissionId =>
        connector
          .getPropertyPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId, submissionId.submissionId)
          .map {
            case Right(Some(submission)) =>
              Some(
                submission.copy(submissionId =
                  Some(
                    PeriodicSubmissionId(submissionId.submissionId)
                  )
                )
              ).asRight[ApiError]
            case Right(None) => None.asRight[ApiError]
            case Left(e)     => e.asLeft[Option[PropertyPeriodicSubmission]]
          }
      }
    val all: Future[List[Either[ApiError, Option[PropertyPeriodicSubmission]]]] =
      Future.sequence(propertyPeriodicSubmissions) // .map(_.flatten)

    EitherT(all.map { list =>
      list.foldLeft[Either[ApiError, List[Option[PropertyPeriodicSubmission]]]](
        List[Option[PropertyPeriodicSubmission]]().asRight[ApiError]
      )((acc, a) =>
        a match {
          case Left(e)  => e.asLeft[List[Option[PropertyPeriodicSubmission]]]
          case Right(r) => acc.map(l => r :: l)
        }
      )
    }).bimap(l => ApiServiceError(l.status), _.flatten)
  }

  private def transformToResponse(
    submissions: List[PropertyPeriodicSubmission]
  ): Either[ServiceError, PropertyPeriodicSubmissionResponse] =
    Right(PropertyPeriodicSubmissionResponse(submissions))

  def persistAnswers[A](ctx: JourneyContext, answers: A)(implicit
    writes: Writes[A]
  ): EitherT[Future, ServiceError, Boolean] =
    EitherT(
      repository.upsertAnswers(ctx, Json.toJson(answers)).map {
        case false => RepositoryError.asLeft[Boolean]
        case true  => true.asRight[ServiceError]
      }
    )

  def savePropertyRentalAdjustments(
    contextWithNino: JourneyContextWithNino,
    propertyRentalAdjustment: PropertyRentalAdjustments
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {

    val adjustmentStoreAnswers = AdjustmentStoreAnswers(
      propertyRentalAdjustment.balancingCharge.balancingChargeYesNo,
      propertyRentalAdjustment.renovationAllowanceBalancingCharge.renovationAllowanceBalancingChargeYesNo
    )

    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None, None, None)

    for {
      propertyAnnualSubmissionFromDownstream <-
        this
          .getPropertyAnnualSubmission(
            contextWithNino.taxYear.endYear,
            contextWithNino.nino.value,
            contextWithNino.incomeSourceId.value
          )
          .leftFlatMap(e =>
            e match {
              case DataNotFoundError => ITPEnvelope.liftPure(emptyPropertyAnnualSubmission)
              case _                 => ITPEnvelope.liftEither(e.asLeft[PropertyAnnualSubmission])
            }
          )
      _ <- createOrUpdateAnnualSubmission(
             contextWithNino.taxYear,
             contextWithNino.incomeSourceId,
             contextWithNino.nino,
             PropertyAnnualSubmission
               .fromPropertyRentalAdjustments(propertyRentalAdjustment, propertyAnnualSubmissionFromDownstream)
           )
      res <- persistAnswers(contextWithNino.toJourneyContext(JourneyName.RentalAdjustments), adjustmentStoreAnswers)
    } yield res

  }

  def saveAllowances(ctx: JourneyContextWithNino, answers: Allowances, journeyName: JourneyName)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Boolean] = {
    val storeAnswers = RentalAllowancesStoreAnswers.fromJourneyAnswers(answers)
    val submission: PropertyAnnualSubmission = getPropertySubmission(answers)
    for {
      _ <- createOrUpdateAnnualSubmission(ctx.taxYear, ctx.incomeSourceId, ctx.nino, submission)
      res <- persistAnswers(ctx.toJourneyContext(journeyName),
               storeAnswers.capitalAllowancesForACar.fold(storeAnswers)( _ => storeAnswers.copy(otherCapitalAllowance = None)))
    } yield res
  }

  private def getPropertySubmission(answers: Allowances) = {
    val allowances = UkOtherAllowances(
      answers.annualInvestmentAllowance,
      answers.zeroEmissionGoodsVehicleAllowance,
      answers.businessPremisesRenovationAllowance,
      answers.otherCapitalAllowance,
      answers.replacementOfDomesticGoodsAllowance,
      answers.electricChargePointAllowance.flatMap(_.electricChargePointAllowanceAmount),
      None,
      None,
      answers.zeroEmissionCarAllowance,
      None
    )
    val annualUkOtherProperty = AnnualUkOtherProperty(None, Some(allowances))
    PropertyAnnualSubmission(None, None, None, None, Some(annualUkOtherProperty))
  }
}
