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
import models.repository.Extractor.GeneralExtractor
import models.repository.Merger._
import models.request._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba.EsbaInfoExtensions._
import models.request.esba.{EsbaInUpstream, EsbaInfo, EsbaInfoToSave}
import models.request.sba.SbaInfoExtensions.SbaExtensions
import models.request.sba.{SbaInfo, SbaInfoToSave}
import models.responses._
import models.{ExpensesStoreAnswers, ITPEnvelope, PropertyPeriodicSubmissionResponse, RentalAllowancesStoreAnswers}
import play.api.libs.Files.logger
import play.api.libs.json.{JsValue, Json, Writes}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Period
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

final case class ClaimExpensesOrRRRYesNo(claimExpensesOrRRR: Boolean)

object ClaimExpensesOrRRRYesNo {
  implicit val format = Json.format[ClaimExpensesOrRRRYesNo]
}

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
      createPropertyPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          CreatePropertyPeriodicSubmissionRequest.fromUkOtherPropertyIncome(currentPeriodicSubmission, saveIncome)
        )
      updatePropertyPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          UpdatePropertyPeriodicSubmissionRequest.fromUkOtherPropertyIncome(currentPeriodicSubmission, saveIncome)
        )
      r <- currentPeriodicSubmission match {
             case None =>
               createPeriodicSubmission(
                 nino.value,
                 journeyContext.incomeSourceId.value,
                 journeyContext.taxYear.endYear,
                 createPropertyPeriodicSubmissionRequest
               )
             case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _, _, _)) =>
               updatePeriodicSubmission(
                 journeyContext.mtditid.value,
                 journeyContext.incomeSourceId.value,
                 journeyContext.taxYear.endYear,
                 submissionId.submissionId,
                 updatePropertyPeriodicSubmissionRequest
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

  def getFetchedPropertyDataMerged(
    ctx: JourneyContext,
    nino: Nino,
    incomeSourceId: String
  )(implicit
    ec: ExecutionContext,
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, FetchedPropertyData] = {
    val resultAnnual = getPropertyAnnualSubmission(ctx.taxYear.endYear, nino.toString, incomeSourceId)

    val resultPeriodic = getCurrentPeriodicSubmission(ctx.taxYear.endYear, nino.toString, incomeSourceId)

    for {
      resultFromDownstreamAnnual        <- resultAnnual
      resultFromDownstreamPeriodicMaybe <- resultPeriodic
      resultFromRepository              <- fetchAllJourneyDataFromRepository(ctx) // ToDo, make a proper repo error?
    } yield mergeAll(resultFromDownstreamAnnual, resultFromDownstreamPeriodicMaybe, resultFromRepository)
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
    resultFromAnnualDownstream: PropertyAnnualSubmission,
    resultFromPeriodicDownstreamMaybe: Option[PropertyPeriodicSubmission],
    resultFromRepository: Map[String, JourneyAnswers]
  ): FetchedPropertyData = {
    val esbaInfoMaybe =
      mergeEsbaInfo(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalESBA.entryName))
    val sbaInfoMaybe =
      mergeSbaInfo(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalSBA.entryName))
    val propertyAboutMaybe = mergePropertyAbout(resultFromRepository.get(JourneyName.About.entryName))
    val adjustmentsMaybe =
      mergeAdjustments(
        resultFromAnnualDownstream,
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalAdjustments.entryName)
      )

    val allowancesMaybe =
      mergeAllowances(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalAllowances.entryName))

    FetchedPropertyData(None, propertyAboutMaybe, adjustmentsMaybe, allowancesMaybe, esbaInfoMaybe, sbaInfoMaybe)
  }

  def mergeAllowances(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromRepository: Option[JourneyAnswers]
  ): Option[Allowances] = {
    val allowancesStoreAnswers: Option[RentalAllowancesStoreAnswers] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[RentalAllowancesStoreAnswers])
      case None                 => None
    }
    val ukOtherPropertyAnnualAllowances: Option[UkOtherAllowances] =
      resultFromDownstream.ukOtherProperty.flatMap(_.ukOtherPropertyAnnualAllowances)
    allowancesStoreAnswers.merge(ukOtherPropertyAnnualAllowances)
  }
  private def mergePropertyAbout(resultFromRepository: Option[JourneyAnswers]): Option[PropertyAbout] =
    resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[PropertyAbout])
      case None                 => None
    }

  private def mergeAdjustments(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromPeriodicDownstreamMaybe: Option[PropertyPeriodicSubmission],
    resultFromRepository: Option[JourneyAnswers]
  ): Option[PropertyRentalAdjustments] = {

    val adjustmentsAndPeriodicExpenses: Option[(UkOtherAdjustments, UkOtherPropertyExpenses)] = for {
      uopAnnual                    <- resultFromDownstream.ukOtherProperty
      resultFromDownstreamPeriodic <- resultFromPeriodicDownstreamMaybe // revisit. None Periodic what to do
      uopPeriodic                  <- resultFromDownstreamPeriodic.ukOtherProperty
      expensesPeriodic             <- uopPeriodic.expenses
      uopaa                        <- uopAnnual.ukOtherPropertyAnnualAdjustments
    } yield (uopaa, expensesPeriodic)

    val adjustmentStoreAnswers: Option[AdjustmentStoreAnswers] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[AdjustmentStoreAnswers])
      case None                 => None
    }

    adjustmentStoreAnswers.merge(adjustmentsAndPeriodicExpenses)
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

    val esbasInRequestMaybe: Option[List[EsbaInUpstream]] = esbasMaybe.flatMap(
      EsbaInUpstream.fromEsbasToEsbasInUpstream(_)
    )

    val esbaInfoSavedInRepositoryMaybe: Option[EsbaInfoToSave] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[EsbaInfoToSave])
      case None                 => None
    }

    esbaInfoSavedInRepositoryMaybe.merge(esbasInRequestMaybe)
  }

  private def mergeSbaInfo(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromRepository: Option[JourneyAnswers]
  ): Option[SbaInfo] = {
    val sbasMaybe: Option[List[StructuredBuildingAllowance]] = for {
      ukop   <- resultFromDownstream.ukOtherProperty
      ukopaa <- ukop.ukOtherPropertyAnnualAllowances
      sba    <- ukopaa.structuredBuildingAllowance
    } yield sba.toList

    val sbaInfoSavedInRepositoryMaybe: Option[SbaInfoToSave] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[SbaInfoToSave])
      case None                 => None
    }

    sbaInfoSavedInRepositoryMaybe.merge(sbasMaybe)
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

  def saveEsbas(
    ctx: JourneyContext,
    nino: Nino,
    esbaInfo: EsbaInfo
  )(implicit hc: HeaderCarrier): ITPEnvelope[Unit] = {
    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None, None, None)

    for {
      annualSubmission <-
        getPropertyAnnualSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value).leftFlatMap(e =>
          e match {
            case DataNotFoundError => ITPEnvelope.liftPure(emptyPropertyAnnualSubmission)
            case _                 => ITPEnvelope.liftEither(e.asLeft[PropertyAnnualSubmission])
          }
        )
      r <- this.createOrUpdateAnnualSubmission(
             ctx.taxYear,
             ctx.incomeSourceId,
             nino,
             PropertyAnnualSubmission.fromEsbas(annualSubmission, esbaInfo.toEsba)
           )
      _ <- this
             .persistAnswers(ctx, esbaInfo.extractToSavePart())
             .map(isPersistSuccess =>
               if (!isPersistSuccess) {
                 logger.error("Could not persist")
               } else {
                 logger.info("Persist successful")
               }
             )
    } yield r
  }

  def saveSbas(
    ctx: JourneyContext,
    nino: Nino,
    sbaInfo: SbaInfo
  )(implicit hc: HeaderCarrier): ITPEnvelope[Unit] = {

    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None, None, None)

    for {
      annualSubmission <-
        getPropertyAnnualSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value).leftFlatMap(e =>
          e match {
            case DataNotFoundError => ITPEnvelope.liftPure(emptyPropertyAnnualSubmission)
            case _                 => ITPEnvelope.liftEither(e.asLeft[PropertyAnnualSubmission])
          }
        )
      r <- this.createOrUpdateAnnualSubmission(
             ctx.taxYear,
             ctx.incomeSourceId,
             nino,
             PropertyAnnualSubmission.fromSbas(annualSubmission, sbaInfo.toSba.toList)
           )
      _ <- this
             .persistAnswers(ctx, sbaInfo.toSbaToSave)
             .map(isPersistSuccess =>
               if (!isPersistSuccess) {
                 logger.error("SBA Persist failed")
               } else {
                 logger.info("SBA Persist successful")
               }
             )
    } yield r
  }
  def saveRaRAbout(ctx: JourneyContext, nino: Nino, rarAbout: RaRAbout)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Boolean] = {
    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None, None, None)
    for {
      maybePeriodicSubmission <- getCurrentPeriodicSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value)
      annualSubmission <-
        getPropertyAnnualSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value).leftFlatMap(e =>
          e match {
            case DataNotFoundError => ITPEnvelope.liftPure(emptyPropertyAnnualSubmission)
            case _                 => ITPEnvelope.liftEither(e.asLeft[PropertyAnnualSubmission])
          }
        )
      createPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          CreatePropertyPeriodicSubmissionRequest.fromUkRaRAbout(maybePeriodicSubmission, rarAbout)
        )
      updatePeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          UpdatePropertyPeriodicSubmissionRequest.fromUkRaRAbout(maybePeriodicSubmission, rarAbout)
        )

      annualSubmissionRequest <-
        ITPEnvelope.liftPure(PropertyAnnualSubmission.fromUkRentARoomAbout(rarAbout, annualSubmission))
      _ <- maybePeriodicSubmission match {
             case None =>
               createPeriodicSubmission(
                 nino.value,
                 ctx.incomeSourceId.value,
                 ctx.taxYear.endYear,
                 createPeriodicSubmissionRequest
               ).map(_ => ())
             case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _, _, _)) =>
               updatePeriodicSubmission(
                 nino.value,
                 ctx.incomeSourceId.value,
                 ctx.taxYear.endYear,
                 submissionId.submissionId,
                 updatePeriodicSubmissionRequest
               ).map(_ => ())
             case _ =>
               ITPEnvelope.liftEither(
                 InternalError("No submission id fetched").asLeft[Unit]
               )
           }
      _ <- createOrUpdateAnnualSubmission(
             ctx.taxYear,
             ctx.incomeSourceId,
             nino,
             annualSubmissionRequest
           )
      res <- persistAnswers(
               ctx,
               ClaimExpensesOrRRRYesNo(rarAbout.claimExpensesOrRRR.claimExpensesOrRRR)
             )

    } yield res
  }

  def saveExpenses(ctx: JourneyContext, nino: Nino, expenses: Expenses)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {
      maybePeriodicSubmission <- getCurrentPeriodicSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value)
      updatePeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(UpdatePropertyPeriodicSubmissionRequest.fromExpenses(maybePeriodicSubmission, expenses))
      createPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(CreatePropertyPeriodicSubmissionRequest.fromExpenses(maybePeriodicSubmission, expenses))
      submissionResponse <- maybePeriodicSubmission match {
                              case None =>
                                createPeriodicSubmission(
                                  nino.value,
                                  ctx.incomeSourceId.value,
                                  ctx.taxYear.endYear,
                                  createPeriodicSubmissionRequest
                                )
                              case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _, _, _)) =>
                                updatePeriodicSubmission(
                                  nino.value,
                                  ctx.incomeSourceId.value,
                                  ctx.taxYear.endYear,
                                  submissionId.submissionId,
                                  updatePeriodicSubmissionRequest
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
    body: CreatePropertyPeriodicSubmissionRequest
  )(implicit hc: HeaderCarrier): ITPEnvelope[Option[PeriodicSubmissionId]] =
    EitherT(connector.createPeriodicSubmission(taxYear, nino, incomeSourceId, body)).leftMap(e =>
      ApiServiceError(e.status)
    )

  def updatePeriodicSubmission(
    nino: String,
    incomeSourceId: String,
    taxYear: Int,
    submissionId: String,
    updatePropertyPeriodicSubmissionRequest: UpdatePropertyPeriodicSubmissionRequest
  )(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[String] =
    EitherT(
      connector
        .updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, updatePropertyPeriodicSubmissionRequest)
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
