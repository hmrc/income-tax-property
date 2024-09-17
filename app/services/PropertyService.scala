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
import models._
import models.common._
import models.domain.JourneyAnswers
import models.errors._
import models.repository.Extractor.GeneralExtractor
import models.request._
import models.request.esba.EsbaInfo
import models.request.esba.EsbaInfoExtensions._
import models.request.sba.SbaInfo
import models.request.sba.SbaInfoExtensions.SbaExtensions
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Period
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PropertyService @Inject() (
  mergeService: MergeService,
  connector: IntegrationFrameworkConnector,
  repository: MongoJourneyAnswersRepository
)(implicit ec: ExecutionContext)
    extends Logging {

  def saveIncome(journeyContext: JourneyContext, nino: Nino, propertyRentalsIncome: PropertyRentalsIncome)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {

      currentPeriodicSubmission <- getCurrentPeriodicSubmission(
                                     journeyContext.taxYear.endYear,
                                     nino.value,
                                     journeyContext.incomeSourceId.value
                                   )
      submissionResponse <- savePeriodicSubmission(
                              journeyContext.toJourneyContextWithNino(nino),
                              currentPeriodicSubmission,
                              propertyRentalsIncome
                            )
      _ <- persistAnswers(journeyContext, StoredIncome.fromRentalsIncome(propertyRentalsIncome)).map(isPersistSuccess =>
             if (!isPersistSuccess) {
               logger.error("Could not persist")
             } else {
               logger.info("Persist successful")
             }
           )
    } yield submissionResponse

  def saveRentalsAndRaRIncome(journeyContext: JourneyContext, nino: Nino, rentalsAndRaRIncome: RentalsAndRaRIncome)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {

      currentPeriodicSubmission <- getCurrentPeriodicSubmission(
                                     journeyContext.taxYear.endYear,
                                     nino.value,
                                     journeyContext.incomeSourceId.value
                                   )
      submissionResponse <- savePeriodicSubmission(
                              journeyContext.toJourneyContextWithNino(nino),
                              currentPeriodicSubmission,
                              rentalsAndRaRIncome
                            )
      _ <- persistAnswers(journeyContext, StoredIncome.fromRentalsAndRaRIncome(rentalsAndRaRIncome)).map(
             isPersistSuccess =>
               if (!isPersistSuccess) {
                 logger.error("Could not persist")
               } else {
                 logger.info("Persist successful")
               }
           )
    } yield submissionResponse

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
    } yield mergeService.mergeAll(resultFromDownstreamAnnual, resultFromDownstreamPeriodicMaybe, resultFromRepository)

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
            (ja + (k -> v.head)).asRight[ServiceError]
          } else {
            RepositoryError.asLeft[Map[String, JourneyAnswers]]
          }
          r
        case left => left
      }
    }

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
  )(implicit hc: HeaderCarrier): ITPEnvelope[Unit] =
    for {
      annualSubmission <- getAnnualSubmission(ctx, nino)
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

  def saveSBA(
    ctx: JourneyContext,
    nino: Nino,
    sbaInfo: SbaInfo
  )(implicit hc: HeaderCarrier): ITPEnvelope[Unit] =
    for {
      annualSubmission <- getAnnualSubmission(ctx, nino)
      result <- this.createOrUpdateAnnualSubmission(
                  ctx.taxYear,
                  ctx.incomeSourceId,
                  nino,
                  PropertyAnnualSubmission.fromSbas(annualSubmission, sbaInfo.toSba)
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
    } yield result

  private def getAnnualSubmission(ctx: JourneyContext, nino: Nino)(implicit hc: HeaderCarrier) = {
    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)
    getPropertyAnnualSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value).leftFlatMap {
      case DataNotFoundError => ITPEnvelope.liftPure(emptyPropertyAnnualSubmission)
      case error             => ITPEnvelope.liftEither(error.asLeft[PropertyAnnualSubmission])
    }
  }

  def saveRaRAbout(ctx: JourneyContext, nino: Nino, rarAbout: RaRAbout)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Boolean] =
    for {
      annualSubmission <- getAnnualSubmission(ctx, nino)
      annualSubmissionRequest <-
        ITPEnvelope.liftPure(PropertyAnnualSubmission.fromUkRentARoomAbout(rarAbout, annualSubmission))
      maybePeriodicSubmission <- getCurrentPeriodicSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value)
      submissionResponse <- savePeriodicSubmission(
                              ctx.toJourneyContextWithNino(nino),
                              maybePeriodicSubmission,
                              rarAbout
                            )
      _ <- createOrUpdateAnnualSubmission(
             ctx.taxYear,
             ctx.incomeSourceId,
             nino,
             annualSubmissionRequest
           )
      res <- persistAnswers(
               ctx,
               ClaimExpensesOrRRRYesNo(rarAbout.claimExpensesOrRelief.claimExpensesOrReliefYesNo)
             )

    } yield res

  def saveRentalsAndRaRAbout(ctx: JourneyContext, nino: Nino, rentalsAndRaRAbout: RentalsAndRaRAbout)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Boolean] =
    for {
      annualSubmission <- getAnnualSubmission(ctx, nino)
      annualSubmissionRequest <-
        ITPEnvelope.liftPure(
          PropertyAnnualSubmission.fromRentalsAndRentARoomAbout(rentalsAndRaRAbout, annualSubmission)
        )
      maybePeriodicSubmission <- getCurrentPeriodicSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value)
      submissionResponse <- savePeriodicSubmission(
                              ctx.toJourneyContextWithNino(nino),
                              maybePeriodicSubmission,
                              rentalsAndRaRAbout
                            )
      _ <- createOrUpdateAnnualSubmission(
             ctx.taxYear,
             ctx.incomeSourceId,
             nino,
             annualSubmissionRequest
           )
      _ <- persistAnswers(
             ctx,
             ClaimExpensesOrRRRYesNo(rentalsAndRaRAbout.claimExpensesOrRelief.claimExpensesOrReliefYesNo)
           )
      res <- persistAnswers(
               ctx,
               ClaimPropertyIncomeAllowanceYesOrNo(rentalsAndRaRAbout.claimPropertyIncomeAllowanceYesOrNo)
             )

    } yield res

  def saveExpenses(ctx: JourneyContext, nino: Nino, expenses: Expenses)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {
      maybePeriodicSubmission <- getCurrentPeriodicSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value)
      updatePeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(UpdatePropertyPeriodicSubmissionRequest.fromEntity(maybePeriodicSubmission, expenses))
      createPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          CreatePropertyPeriodicSubmissionRequest.fromEntity(ctx.taxYear, maybePeriodicSubmission, expenses)
        )
      submissionResponse <- maybePeriodicSubmission match {
                              case None =>
                                createPeriodicSubmission(
                                  nino.value,
                                  ctx.incomeSourceId.value,
                                  ctx.taxYear.endYear,
                                  createPeriodicSubmissionRequest
                                )
                              case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _)) =>
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

  def saveRaRExpenses(ctx: JourneyContext, nino: Nino, raRExpenses: RentARoomExpenses)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {
      maybePeriodicSubmission <- getCurrentPeriodicSubmission(ctx.taxYear.endYear, nino.value, ctx.incomeSourceId.value)
      updatePeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          UpdatePropertyPeriodicSubmissionRequest.fromEntity(maybePeriodicSubmission, raRExpenses)
        )
      createPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          CreatePropertyPeriodicSubmissionRequest.fromEntity(ctx.taxYear, maybePeriodicSubmission, raRExpenses)
        )
      submissionResponse <- maybePeriodicSubmission match {
                              case None =>
                                createPeriodicSubmission(
                                  nino.value,
                                  ctx.incomeSourceId.value,
                                  ctx.taxYear.endYear,
                                  createPeriodicSubmissionRequest
                                )
                              case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _)) =>
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
      _ <- raRExpenses.consolidatedExpenses match {
             case Some(consolidatedExpenses) =>
               persistAnswers(ctx, RentARoomExpensesStoreAnswers(consolidatedExpenses.consolidatedExpensesYesOrNo))
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
    context: JourneyContext,
    nino: Nino,
    propertyRentalAdjustment: PropertyRentalAdjustments
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {

    val adjustmentStoreAnswers = AdjustmentStoreAnswers(
      propertyRentalAdjustment.balancingCharge.balancingChargeYesNo,
      propertyRentalAdjustment.renovationAllowanceBalancingCharge.renovationAllowanceBalancingChargeYesNo
    )
    for {
      maybePeriodicSubmission <- getCurrentPeriodicSubmission(
                                   context.taxYear.endYear,
                                   nino.value,
                                   context.incomeSourceId.value
                                 )
      _ <- savePeriodicSubmission(
             context.toJourneyContextWithNino(nino),
             maybePeriodicSubmission,
             propertyRentalAdjustment
           )
      propertyAnnualSubmissionFromDownstream <- getAnnualSubmission(context, nino)
      _ <- createOrUpdateAnnualSubmission(
             context.taxYear,
             context.incomeSourceId,
             nino,
             PropertyAnnualSubmission
               .fromPropertyRentalAdjustments(propertyRentalAdjustment, propertyAnnualSubmissionFromDownstream)
           )
      res <- persistAnswers(context, adjustmentStoreAnswers)
    } yield res

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
                                  contextWithNino.nino.value,
                                  contextWithNino.incomeSourceId.value,
                                  contextWithNino.taxYear.endYear,
                                  createPeriodicSubmissionRequest
                                )
                              case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _)) =>
                                updatePeriodicSubmission(
                                  contextWithNino.nino.value,
                                  contextWithNino.incomeSourceId.value,
                                  contextWithNino.taxYear.endYear,
                                  submissionId.submissionId,
                                  updatePeriodicSubmissionRequest
                                ).map(_ => Some(submissionId))
                              case _ =>
                                ITPEnvelope.liftEither(
                                  InternalError("No submission id fetched").asLeft[Option[PeriodicSubmissionId]]
                                )
                            }
    } yield submissionResponse

  def saveRaRAdjustments(ctx: JourneyContext, nino: Nino, raRAdjustments: RaRAdjustments)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Boolean] = {

    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)

    for {
      propertyAnnualSubmissionFromDownstream <-
        this
          .getPropertyAnnualSubmission(
            ctx.taxYear.endYear,
            nino.value,
            ctx.incomeSourceId.value
          )
          .leftFlatMap {
            case DataNotFoundError => ITPEnvelope.liftPure(emptyPropertyAnnualSubmission)
            case e                 => ITPEnvelope.liftEither(e.asLeft[PropertyAnnualSubmission])
          }
      _ <- createOrUpdateAnnualSubmission(
             ctx.taxYear,
             ctx.incomeSourceId,
             nino,
             PropertyAnnualSubmission
               .fromRaRAdjustments(propertyAnnualSubmissionFromDownstream, raRAdjustments)
           )
      res <- {
        raRAdjustments.balancingCharge match {
          case Some(BalancingCharge(raRbalancingChargeYesNo, _)) =>
            persistAnswers(ctx, RaRBalancingChargeYesNo(raRbalancingChargeYesNo))
          case _ => ITPEnvelope.liftPure(true)
        }
      }
    } yield res
  }

  def savePropertyRentalAllowances(
    ctx: JourneyContextWithNino,
    rentalAllowances: RentalAllowances
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {

    val rentalAllowancesStoreAnswers = RentalAllowancesStoreAnswers.fromJourneyAnswers(rentalAllowances)

    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)

    for {
      propertyAnnualSubmissionFromDownstream <-
        this
          .getPropertyAnnualSubmission(
            ctx.taxYear.endYear,
            ctx.nino.value,
            ctx.incomeSourceId.value
          )
          .leftFlatMap(e =>
            e match {
              case DataNotFoundError => ITPEnvelope.liftPure(emptyPropertyAnnualSubmission)
              case _                 => ITPEnvelope.liftEither(e.asLeft[PropertyAnnualSubmission])
            }
          )
      _ <- createOrUpdateAnnualSubmission(
             ctx.taxYear,
             ctx.incomeSourceId,
             ctx.nino,
             PropertyAnnualSubmission
               .fromRentalAllowances(propertyAnnualSubmissionFromDownstream, rentalAllowances)
           )
      res <- persistAnswers(ctx.toJourneyContext(JourneyName.RentARoomAllowances), rentalAllowancesStoreAnswers)
    } yield res
  }

  def saveRentARoomAllowances(
    ctx: JourneyContextWithNino,
    rentARoomAllowances: RentARoomAllowances
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {
    val rentARoomAllowancesStoreAnswers = RentARoomAllowancesStoreAnswers(
      rentARoomAllowances.capitalAllowancesForACar.map(_.capitalAllowancesForACarYesNo)
    )

    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)

    for {
      propertyAnnualSubmissionFromDownstream <-
        this
          .getPropertyAnnualSubmission(
            ctx.taxYear.endYear,
            ctx.nino.value,
            ctx.incomeSourceId.value
          )
          .leftFlatMap {
            case DataNotFoundError => ITPEnvelope.liftPure(emptyPropertyAnnualSubmission)
            case e                 => ITPEnvelope.liftEither(e.asLeft[PropertyAnnualSubmission])
          }
      _ <- createOrUpdateAnnualSubmission(
             ctx.taxYear,
             ctx.incomeSourceId,
             ctx.nino,
             PropertyAnnualSubmission
               .fromRaRAllowances(propertyAnnualSubmissionFromDownstream, rentARoomAllowances)
           )
      res <- persistAnswers(ctx.toJourneyContext(JourneyName.RentARoomAllowances), rentARoomAllowancesStoreAnswers)
    } yield res
  }
}
