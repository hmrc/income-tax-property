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
import models.domain.{ForeignFetchedPropertyData, JourneyAnswers}
import models.errors._
import models.request.foreign._
import models.request.foreign.expenses.ForeignPropertyExpenses
import models.request.{CreatePropertyPeriodicSubmissionRequest, UpdatePropertyPeriodicSubmissionRequest}
import models.responses._
import models.{ITPEnvelope, PropertyPeriodicSubmissionResponse}
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ForeignPropertyService @Inject() (
  mergeService: ForeignMergeService,
  connector: IntegrationFrameworkConnector,
  repository: MongoJourneyAnswersRepository
)(implicit ec: ExecutionContext)
    extends Logging {

  def getFetchedPropertyDataMerged(
                                    ctx: JourneyContext,
                                    nino: Nino,
                                    incomeSourceId: IncomeSourceId
                                  )(implicit
                                    hc: HeaderCarrier
                                  ): EitherT[Future, ServiceError, ForeignFetchedPropertyData] = {
    val resultAnnual = getPropertyAnnualSubmission(ctx.taxYear, nino, incomeSourceId)

    val resultPeriodic = getCurrentPeriodicSubmission(ctx.taxYear, nino, incomeSourceId)

    for {
      resultFromDownstreamAnnual        <- resultAnnual
      resultFromDownstreamPeriodicMaybe <- resultPeriodic
      foreignResultFromRepository       <- fetchAllForeignJourneyDataFromRepository(ctx)
    } yield mergeService.mergeAll(resultFromDownstreamAnnual, resultFromDownstreamPeriodicMaybe, foreignResultFromRepository)

  }

  private def fetchAllForeignJourneyDataFromRepository(ctx: JourneyContext): ITPEnvelope[Map[String, Map[String, JourneyAnswers]]] = {
    val result: Future[Either[ServiceError, Map[String, Map[String, JourneyAnswers]]]] = if (ctx.journey == JourneyName.NoJourney) {
      Future.successful(
        InternalError(s"Journey Repo could not be accessed, journey name: ${ctx.journey.entryName}")
          .asLeft[Map[String, Map[String, JourneyAnswers]]]
      )
    } else {
      repository
        .fetchAllJourneys(ctx)
        .map(ja => getForeignJourneysPerJourneyName(ja.toList))
    }
    ITPEnvelope.liftFuture(result)
  }

  private def getForeignJourneysPerJourneyName(journeyAnswers: List[JourneyAnswers]): Either[ServiceError, Map[String, Map[String, JourneyAnswers]]] = {
    val journeyAnswersGrouped = journeyAnswers.groupBy(j => j.journey.entryName)
    journeyAnswersGrouped.foldLeft(Map[String, Map[String, JourneyAnswers]]().asRight[ServiceError]) { (acc, kv) =>
      acc match {
        case Right(validJourneys) =>
          val (journeyName, journeys) = kv
          val foreignJourneyMap: Either[ServiceError, Map[String, JourneyAnswers]] =
            journeys.groupBy(_.countryCode).foldLeft(Map[String, JourneyAnswers]().asRight[ServiceError]) { (innerAcc, innerKV) =>
              innerAcc match {
                case Right(innerValidJourneys) =>
                  innerKV match {
                    case (Some(countryCode), List(foreignJourneyAnswers)) => (innerValidJourneys + (countryCode -> foreignJourneyAnswers)).asRight[ServiceError]
                    case _ => innerValidJourneys.asRight[ServiceError]
                  }
                case left => left
              }
            }
          foreignJourneyMap match {
            case Right(fjm) => (validJourneys + (journeyName -> fjm)).asRight[ServiceError]
            case _ => validJourneys.asRight[ServiceError]
          }
        case left => left
      }
    }
  }

  def getPropertyAnnualSubmission(taxYear: TaxYear, taxableEntityId: Nino, incomeSourceId: IncomeSourceId)(implicit
                                                                                                           hc: HeaderCarrier
  ): ITPEnvelope[PropertyAnnualSubmission] =
    EitherT(connector.getPropertyAnnualSubmission(taxYear, taxableEntityId, incomeSourceId))
      .leftMap(error => ApiServiceError(error.status))
      .subflatMap { annualSubmission =>
        annualSubmission.fold[Either[ServiceError, PropertyAnnualSubmission]](
          DataNotFoundError.asLeft[PropertyAnnualSubmission]
        )(_.asRight[ServiceError])
      }

  def persistAnswers[A](ctx: JourneyContext, answers: A)(implicit
    writes: Writes[A]
  ): EitherT[Future, ServiceError, Boolean] =
    EitherT(
      repository.upsertAnswers(ctx, Json.toJson(answers)).map {
        case false => RepositoryError.asLeft[Boolean]
        case true  => true.asRight[ServiceError]
      }
    )
  def persistForeignAnswers[A](ctx: JourneyContext, answers: A, countryCode: String)(implicit
    writes: Writes[A]
  ): EitherT[Future, ServiceError, Boolean] =
    EitherT(
      repository.foreignUpsertAnswers(ctx, Json.toJson(answers), countryCode).map {
        case false => RepositoryError.asLeft[Boolean]
        case true  => true.asRight[ServiceError]
      }
    )

  def saveForeignPropertySelectCountry(
    ctx: JourneyContext,
    foreignPropertySelectCountry: ForeignPropertySelectCountry
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] =
    persistAnswers(
      ctx,
      foreignPropertySelectCountry
    )

  def saveForeignPropertyExpenses(
    ctx: JourneyContext,
    foreignPropertyExpenses: ForeignPropertyExpenses
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] =
    persistForeignAnswers(ctx, foreignPropertyExpenses, foreignPropertyExpenses.countryCode)

  def saveForeignPropertyTax(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertyTaxWithCountryCode: ForeignPropertyTaxWithCountryCode
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {
      currentPeriodicSubmission <- getCurrentPeriodicSubmission(
                                     journeyContext.taxYear,
                                     nino,
                                     journeyContext.incomeSourceId
                                   )

      submissionResponse <- savePeriodicSubmission(
                              journeyContext.toJourneyContextWithNino(nino),
                              currentPeriodicSubmission,
                              foreignPropertyTaxWithCountryCode
                            )
      _ <- persistForeignAnswers(
             journeyContext,
             ForeignPropertyTaxStoreAnswers(
               foreignIncomeTaxYesNo = foreignPropertyTaxWithCountryCode.foreignIncomeTax.map(_.foreignIncomeTaxYesNo)
             ),
             foreignPropertyTaxWithCountryCode.countryCode
           ).map(isPersistSuccess =>
             if (!isPersistSuccess) {
               logger.error("Could not persist")
             } else {
               logger.info("Persist successful")
             }
           )
    } yield submissionResponse

  def getCurrentPeriodicSubmission(taxYear: TaxYear, nino: Nino, incomeSourceId: IncomeSourceId)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Option[PropertyPeriodicSubmission]] =
    getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId)
      .map(_.periodicSubmissions.headOption)
      .flatMap {
        case Some(newest) => ITPEnvelope.liftPure(Some(newest))
        case None         => ITPEnvelope.liftPure(None)
      }

  private def savePeriodicSubmission[T](
    contextWithNino: JourneyContextWithNino,
    maybePeriodicSubmission: Option[PropertyPeriodicSubmission],
    entity: T
  )(implicit hc: HeaderCarrier): ITPEnvelope[Option[PeriodicSubmissionId]] =
    for {
      updatePeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          UpdatePropertyPeriodicSubmissionRequest
            .fromEntity(maybePeriodicSubmission, entity)
        )
      createPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          CreatePropertyPeriodicSubmissionRequest
            .fromEntity(contextWithNino.taxYear, maybePeriodicSubmission, entity)
        )
      submissionResponse <- maybePeriodicSubmission match {
                              case None =>
                                createPeriodicSubmission(
                                  contextWithNino.nino,
                                  contextWithNino.incomeSourceId,
                                  contextWithNino.taxYear,
                                  createPeriodicSubmissionRequest
                                )
                              case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _)) =>
                                updatePeriodicSubmission(
                                  contextWithNino.nino,
                                  contextWithNino.incomeSourceId,
                                  contextWithNino.taxYear,
                                  submissionId.submissionId,
                                  updatePeriodicSubmissionRequest
                                ).map(_ => Some(submissionId))
                              case _ =>
                                ITPEnvelope.liftEither(
                                  InternalError("No submission id fetched").asLeft[Option[PeriodicSubmissionId]]
                                )
                            }
    } yield {
      logger.debug(s"Save periodic submission details: $submissionResponse")
      submissionResponse
    }

  def createOrUpdateAnnualSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino,
    body: PropertyAnnualSubmission
  )(implicit hc: HeaderCarrier): ITPEnvelope[Unit] =
    body match {
      case PropertyAnnualSubmission(None, None, None) =>
        ITPEnvelope.liftPure(())
      case _ =>
        EitherT(
          connector.createOrUpdateAnnualSubmission(taxYear, incomeSourceId, nino, body)
        ).leftMap(e => ApiServiceError(e.status))
    }

  def createPeriodicSubmission(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
    body: CreatePropertyPeriodicSubmissionRequest
  )(implicit hc: HeaderCarrier): ITPEnvelope[Option[PeriodicSubmissionId]] =
    EitherT(connector.createPeriodicSubmission(taxYear, nino, incomeSourceId, body)).leftMap(e =>
      ApiServiceError(e.status)
    )

  def updatePeriodicSubmission(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
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

  def getPropertyPeriodicSubmissions(taxYear: TaxYear, nino: Nino, incomeSourceId: IncomeSourceId)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[PropertyPeriodicSubmissionResponse] = {

    val result: ITPEnvelope[List[PropertyPeriodicSubmission]] =
      for {
        periodicSubmissionIds <- EitherT(connector.getAllPeriodicSubmission(taxYear, nino, incomeSourceId))
                                   .leftMap(error => ApiServiceError(error.status))
        propertyPeriodicSubmissions <-
          getPropertySubmissions(taxYear, nino, incomeSourceId, periodicSubmissionIds)
      } yield {
        logger.debug(s"Filtered periodic submission details: $propertyPeriodicSubmissions")
        propertyPeriodicSubmissions
      }

    result.subflatMap(propertyPeriodicSubmissionList => transformToResponse(propertyPeriodicSubmissionList))
  }

  private def getPropertySubmissions(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    periodicSubmissionIds: List[PeriodicSubmissionIdModel]
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): ITPEnvelope[List[PropertyPeriodicSubmission]] = {
    val propertyPeriodicSubmissions = periodicSubmissionIds
      .filter(submissionId =>
        submissionId.fromDate.equals(TaxYear.startDate(taxYear.endYear)) && submissionId.toDate
          .equals(TaxYear.endDate(taxYear.endYear))
      )
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

  def saveForeignIncome(
    ctx: JourneyContext,
    foreignPropertyIncome: ForeignIncome
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] =
    persistForeignAnswers(
      ctx,
      foreignPropertyIncome,
      foreignPropertyIncome.countryCode
    )

}
