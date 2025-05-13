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
import models.LossType.UKProperty
import models._
import models.common._
import models.domain.{FetchedPropertyData, JourneyAnswers}
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
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PropertyService @Inject() (
  mergeService: MergeService,
  connector: IntegrationFrameworkConnector,
  mongoService: MongoJourneyAnswersService
)(implicit ec: ExecutionContext)
    extends Logging {

  def getPropertyPeriodicSubmissions(taxYear: TaxYear, nino: Nino, incomeSourceId: IncomeSourceId)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[PropertyPeriodicSubmissionResponse] = {

    val result: ITPEnvelope[List[PropertyPeriodicSubmission]] =
      for {
        periodicSubmissionIds <- EitherT(connector.getAllPeriodicSubmissionIds(taxYear, nino, incomeSourceId))
                                   .leftMap(error => ApiServiceError(error.status))
        currentPeriodicSubmissions <-
          getCurrentPeriodicSubmissionsForIds(taxYear, nino, incomeSourceId, periodicSubmissionIds)
      } yield {
        logger.debug(
          s"[getPropertyPeriodicSubmissions] Periodic submission ids from IF ids: ${periodicSubmissionIds.map(_.submissionId).mkString(", ")}"
        )
        logger.debug(
          s"[getPropertyPeriodicSubmissions] Periodic submission details from IF: $currentPeriodicSubmissions"
        )
        currentPeriodicSubmissions
      }

    result.subflatMap(propertyPeriodicSubmissionList => transformToResponse(propertyPeriodicSubmissionList))
  }

  private def getCurrentPeriodicSubmissionsForIds(
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
              logger.info("[getPropertySubmissions] Property submissions obtained successfully")
              Some(
                submission.copy(submissionId =
                  Some(
                    PeriodicSubmissionId(submissionId.submissionId)
                  )
                )
              ).asRight[ApiError]
            case Right(None) =>
              logger.warn("[getPropertySubmissions] Did not get property submissions")
              None.asRight[ApiError]
            case Left(e) =>
              logger.error(
                s"[getPropertySubmissions] Error when trying to get property submissions. Status: ${e.status} Body: ${e.body}"
              )
              e.asLeft[Option[PropertyPeriodicSubmission]]
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

  def getPeriodicSubmission(taxYear: TaxYear, nino: Nino, incomeSourceId: IncomeSourceId)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Option[PropertyPeriodicSubmission]] =
    getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId)
      .map(_.periodicSubmissions.headOption)
      .flatMap {
        case Some(newest) => ITPEnvelope.liftPure(Some(newest))
        case None         => ITPEnvelope.liftPure(None)
      }

  def getPropertyAnnualSubmission(taxYear: TaxYear, taxableEntityId: Nino, incomeSourceId: IncomeSourceId)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[PropertyAnnualSubmission] =
    EitherT(connector.getPropertyAnnualSubmission(taxYear, taxableEntityId, incomeSourceId))
      .map { pas =>
        logger.debug(s"[getPropertyAnnualSubmission] Annual submission details from IF: $pas")
        pas
      }
      .leftMap { error =>
        logger.error(s"[getPropertyAnnualSubmission] Annual submission details error")
        ApiServiceError(error.status)
      }
      .subflatMap { annualSubmission =>
        annualSubmission.fold[Either[ServiceError, PropertyAnnualSubmission]] {
          logger.error(s"[getPropertyAnnualSubmission] Annual submission details not found in IF")
          DataNotFoundError.asLeft[PropertyAnnualSubmission]
        } { data =>
          logger.info(s"[getPropertyAnnualSubmission] Annual submission data found: $data")
          data.asRight[ServiceError]
        }
      }

  private def fetchAllJourneyDataFromRepository(
    ctx: JourneyContext
  ): ITPEnvelope[(Map[String, JourneyAnswers], Map[String, Map[String, JourneyAnswers]], Map[String, Map[String, JourneyAnswers]])] =
    if (ctx.journey == JourneyName.NoJourney) {
      logger.error(
        s"[fetchAllJourneyDataFromRepository] Journey Repo could not be accessed, journey name: ${ctx.journey.entryName}"
      )
      ITPEnvelope.liftFuture(
        Future.successful(
          InternalError(s"Journey Repo could not be accessed, journey name: ${ctx.journey.entryName}").asLeft
        )
      )
    } else {

      ITPEnvelope.liftFuture {
        mongoService.fetchAllJourneys(ctx).map { journeys =>
          getValidJourneysPerJourneyName(journeys.toList) match {
            case Right((ukJourneyMap, foreignJourneyMap, foreignIncomeJourneyMap)) =>
              logger.debug(
                s"[fetchAllJourneyDataFromRepository] Successfully retrieved journeys. UK: $ukJourneyMap, Foreign Property: $foreignJourneyMap, Foreign Income: $foreignIncomeJourneyMap"
              )
              Right((ukJourneyMap, foreignJourneyMap, foreignIncomeJourneyMap))

            case Left(error) =>
              logger.error(s"[fetchAllJourneyDataFromRepository] Error fetching valid journeys: ${error.message}")
              Left(error)
          }
        }
      }
    }

  def getFetchedPropertyDataMerged(
    ctx: JourneyContext,
    nino: Nino,
    incomeSourceId: IncomeSourceId
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, FetchedPropertyData] = {

    val resultAnnual = getPropertyAnnualSubmission(ctx.taxYear, nino, incomeSourceId)

    val resultPeriodic = getPeriodicSubmission(ctx.taxYear, nino, incomeSourceId)

    for {
      resultFromDownstreamAnnual        <- resultAnnual
      resultFromDownstreamPeriodicMaybe <- resultPeriodic
      resultFromRepository              <- fetchAllJourneyDataFromRepository(ctx) // ToDo, make a proper repo error?
      (ukResultFromRepository, foreignResultFromRepository, foreignIncomeResultFromRepository) = resultFromRepository
    } yield {
      val mergedData = mergeService.mergeAll(
        resultFromDownstreamAnnual,
        resultFromDownstreamPeriodicMaybe,
        ukResultFromRepository,
        foreignResultFromRepository,
        foreignIncomeResultFromRepository
      )
      logger.debug(s"[getFetchedPropertyDataMerged] Annual, Periodic and Repository merged data is: $mergedData")
      mergedData
    }

  }

  private def getValidUKJourneysPerJourneyName(
    journeyAnswers: List[JourneyAnswers]
  ): Either[ServiceError, Map[String, JourneyAnswers]] = {
    val journeyAnswersGrouped = journeyAnswers.groupBy(j => j.journey.entryName)
    journeyAnswersGrouped.foldLeft(Map[String, JourneyAnswers]().asRight[ServiceError]) { (acc, kv) =>
      acc match {
        case Right(ja) =>
          val (k, v) = kv
          val r: Either[ServiceError, Map[String, JourneyAnswers]] = if (v.forall(_.countryCode.isDefined)) {
            logger.debug(s"[getValidUKJourneysPerJourneyName] Got the journey answers from the repository. JA: $ja")
            ja.asRight[ServiceError]
          } else if (v.size == 1) {
            logger.debug(
              s"[getValidUKJourneysPerJourneyName] Fetched UK Journey Answers from the repository. JA: ${ja + (k -> v.head)})"
            )
            (ja + (k -> v.head)).asRight[ServiceError]
          } else {
            logger.error(s"[getValidUKJourneysPerJourneyName] Could not find UK Journey Answers from the repository")
            RepositoryError.asLeft[Map[String, JourneyAnswers]]
          }
          r
        case left =>
          logger.error(
            s"[getValidUKJourneysPerJourneyName] Error in getting UK Journey Answers from the repository. Error $left"
          )
          left

      }
    }

  }

  private def getForeignJourneysPerJourneyName(
    journeyAnswers: List[JourneyAnswers]
  ): Either[ServiceError, Map[String, Map[String, JourneyAnswers]]] = {
    val journeyAnswersGrouped = journeyAnswers.groupBy(j => j.journey.entryName)
    journeyAnswersGrouped.foldLeft(Map[String, Map[String, JourneyAnswers]]().asRight[ServiceError]) { (acc, kv) =>
      acc match {
        case Right(validJourneys) =>
          val (journeyName, journeys) = kv
          val foreignJourneyMap: Either[ServiceError, Map[String, JourneyAnswers]] =
            journeys.groupBy(_.countryCode).foldLeft(Map[String, JourneyAnswers]().asRight[ServiceError]) {
              (innerAcc, innerKV) =>
                innerAcc match {
                  case Right(innerValidJourneys) =>
                    innerKV match {
                      case (Some(countryCode), List(foreignJourneyAnswers)) =>
                        logger.debug(
                          s"[getForeignJourneysPerJourneyName] Fetched Foreign Journey Answers from the repository for country: $countryCode"
                        )
                        (innerValidJourneys + (countryCode -> foreignJourneyAnswers)).asRight[ServiceError]
                      case (c, ja) =>
                        logger
                          .warn(
                            s"[getForeignJourneysPerJourneyName] For country: $c journey answers: $ja"
                          )
                        innerValidJourneys.asRight[ServiceError]
                    }
                  case left =>
                    logger.error(
                      s"[getForeignJourneysPerJourneyName] Failed to get any Foreign Journey Answers from the repository: $left"
                    )
                    left
                }
            }
          foreignJourneyMap match {
            case Right(fjm) =>
              logger.debug(
                s"[getForeignJourneysPerJourneyName] Foreign journey map from the repository: ${validJourneys + (journeyName -> fjm)}"
              )
              (validJourneys + (journeyName -> fjm)).asRight[ServiceError]
            case x =>
              logger.warn(
                s"[getForeignJourneysPerJourneyName] For foreign journey map from the repository: $validJourneys"
              )
              validJourneys.asRight[ServiceError]
          }
        case left =>
          logger.error(s"[getForeignJourneysPerJourneyName] Failed to get any data from the repository. Error $left")
          left
      }
    }
  }

  private def getValidJourneysPerJourneyName(
    journeyAnswers: List[JourneyAnswers]
  ): Either[ServiceError, (Map[String, JourneyAnswers], Map[String, Map[String, JourneyAnswers]], Map[String, Map[String, JourneyAnswers]])] =
    for {
      ukJourneyMap <- {
        logger.debug("[getValidJourneysPerJourneyName] Getting UK Property journey map")
        getValidUKJourneysPerJourneyName(journeyAnswers)
      }
      foreignJourneyMap <- {
        logger.debug("[getValidJourneysPerJourneyName] Getting Foreign Property journey map")
        getForeignJourneysPerJourneyName(journeyAnswers)
      }
      foreignIncomeJourneyMap <- {
        logger.debug("[getValidJourneysPerJourneyName] Getting Foreign Income journey map")
        getForeignJourneysPerJourneyName(journeyAnswers)
      }
    } yield (ukJourneyMap, foreignJourneyMap, foreignIncomeJourneyMap)

  private def getAnnualSubmission(ctx: JourneyContext, nino: Nino)(implicit hc: HeaderCarrier) = {
    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)
    getPropertyAnnualSubmission(ctx.taxYear, nino, ctx.incomeSourceId).leftFlatMap {
      case DataNotFoundError =>
        logger.warn(s"[getAnnualSubmission] No annual data submission found")
        ITPEnvelope.liftPure(emptyPropertyAnnualSubmission)
      case error =>
        logger.error(
          s"[getAnnualSubmission] Error returned when trying to fetch annual data submission: ${error.message}"
        )
        ITPEnvelope.liftEither(error.asLeft[PropertyAnnualSubmission])
    }
  }

  private def createBroughtForwardLoss(
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    lossAmount: BigDecimal
  )(implicit hc: HeaderCarrier): ITPEnvelope[String] = {
    EitherT(connector.createBroughtForwardLoss(taxYearBroughtForwardFrom, nino, incomeSourceId, lossAmount))
      .map(_.lossId)
      .leftMap { error =>
        logger.error(s"[createBroughtForwardLoss]: Error creating loss brought forward")
        ApiServiceError(error.status)
      }
  }

  private def getBroughtForwardLosses(
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
    nino: Nino,
    incomeSourceId: IncomeSourceId
  )(implicit hc: HeaderCarrier): ITPEnvelope[Seq[BroughtForwardLossResponseWithId]] = {
    EitherT(connector.getBroughtForwardLosses(taxYearBroughtForwardFrom, nino, incomeSourceId))
      .map(_.losses)
      .leftMap {error =>
        logger.error(s"[getBroughtForwardLosses]: Error retrieving losses brought forward")
        ApiServiceError(error.status)
      }
  }

  private def updateBroughtForwardLoss(
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
    nino: Nino,
    lossId: String,
    lossAmount: BigDecimal
  )(implicit hc: HeaderCarrier): ITPEnvelope[BroughtForwardLossResponse] = {
    EitherT(connector.updateBroughtForwardLoss(taxYearBroughtForwardFrom, nino, lossId, lossAmount))
      .leftMap {error =>
        logger.error(s"[updateBroughtForwardLoss]: Error updating losses brought forward")
        ApiServiceError(error.status)
      }
  }

  private def createOrUpdateBroughtForwardLoss(
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
    nino: Nino,
    ctx: JourneyContext,
    lossAmount: BigDecimal
  )(implicit hc: HeaderCarrier): ITPEnvelope[String] = {
    for {
      broughtForwardLosses <- getBroughtForwardLosses(
                                taxYearBroughtForwardFrom,
                                nino,
                                incomeSourceId = ctx.incomeSourceId
                              ).orElse(ITPEnvelope.liftPure(Seq.empty[BroughtForwardLossResponseWithId]))
      broughtForwardLossId <- broughtForwardLosses.find(bfl => bfl.typeOfLoss == UKProperty) match {
                                  case Some(bfl) =>
                                    updateBroughtForwardLoss(
                                      taxYearBroughtForwardFrom,
                                      nino,
                                      bfl.lossId,
                                      lossAmount
                                    ).map(_ => bfl.lossId)
                                  case _ =>
                                    createBroughtForwardLoss(
                                      taxYearBroughtForwardFrom,
                                      nino,
                                      ctx.incomeSourceId,
                                      lossAmount
                                    )
                                }
    } yield broughtForwardLossId
  }

  def saveIncome(journeyContext: JourneyContext, nino: Nino, propertyRentalsIncome: PropertyRentalsIncome)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {

      currentPeriodicSubmission <- getPeriodicSubmission(
                                     journeyContext.taxYear,
                                     nino,
                                     journeyContext.incomeSourceId
                                   )
      submissionResponse <- savePeriodicSubmission(
                              journeyContext.toJourneyContextWithNino(nino),
                              currentPeriodicSubmission,
                              propertyRentalsIncome
                            )
      _ <- mongoService.persistAnswers(journeyContext, StoredIncome.fromRentalsIncome(propertyRentalsIncome)).map(isPersistSuccess =>
             if (!isPersistSuccess) {
               logger.error("[saveIncome] Could not persist")
             } else {
               logger.info("[saveIncome] Persist successful")
             }
           )
    } yield submissionResponse

  def saveRentalsAndRaRIncome(journeyContext: JourneyContext, nino: Nino, rentalsAndRaRIncome: RentalsAndRaRIncome)(
    implicit hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {

      currentPeriodicSubmission <- getPeriodicSubmission(
                                     journeyContext.taxYear,
                                     nino,
                                     journeyContext.incomeSourceId
                                   )
      submissionResponse <- savePeriodicSubmission(
                              journeyContext.toJourneyContextWithNino(nino),
                              currentPeriodicSubmission,
                              rentalsAndRaRIncome
                            )
      _ <- mongoService.persistAnswers(journeyContext, StoredIncome.fromRentalsAndRaRIncome(rentalsAndRaRIncome)).map(
             isPersistSuccess =>
               if (!isPersistSuccess) {
                 logger.error("[saveRentalsAndRaRIncome] Could not persist")
               } else {
                 logger.info("[saveRentalsAndRaRIncome] Persist successful")
               }
           )
    } yield submissionResponse

  def saveEsbas(
    ctx: JourneyContext,
    nino: Nino,
    esbaInfo: EsbaInfo
  )(implicit hc: HeaderCarrier): ITPEnvelope[Unit] =
    for {
      submissionSuccess <- if(
          !esbaInfo.claimEnhancedStructureBuildingAllowance ||
            esbaInfo.enhancedStructureBuildingAllowances.isEmpty
      ){
        ITPEnvelope.liftPure(())
      } else {
        for {
          annualSubmission <- getAnnualSubmission(ctx, nino)
          r <- this.createOrUpdateAnnualSubmission(
            ctx.taxYear,
            ctx.incomeSourceId,
            nino,
            PropertyAnnualSubmission.fromEsbas(annualSubmission, esbaInfo.toEsba)
          )
        } yield r
      }
      _ <- mongoService.persistAnswers(ctx, esbaInfo.extractToSavePart())
             .map(isPersistSuccess =>
               if (!isPersistSuccess) {
                 logger.error("[saveEsbas] Could not persist")
               } else {
                 logger.info("[saveEsbas] Persist successful")
               }
             )
    } yield submissionSuccess

  def saveSBA(
    ctx: JourneyContext,
    nino: Nino,
    sbaInfo: SbaInfo
  )(implicit hc: HeaderCarrier): ITPEnvelope[Unit] =
    for {
      submissionSuccess <- if(!sbaInfo.claimStructureBuildingAllowance || sbaInfo.structureBuildingFormGroup.isEmpty) {
          ITPEnvelope.liftPure(())
        } else {
          for {
            annualSubmission <- getAnnualSubmission(ctx, nino)
            result <- this.createOrUpdateAnnualSubmission(
              ctx.taxYear,
              ctx.incomeSourceId,
              nino,
              PropertyAnnualSubmission.fromSbas(annualSubmission, sbaInfo.toSba)
            )
          } yield result
        }
      _ <- mongoService.persistAnswers(ctx, sbaInfo.toSbaToSave)
             .map(isPersistSuccess =>
               if (!isPersistSuccess) {
                 logger.error("[saveSBA] Could not persist")
               } else {
                 logger.info("[saveSBA] Persist successful")
               }
             )
    } yield submissionSuccess

  def saveRaRAbout(ctx: JourneyContext, nino: Nino, rarAbout: RaRAbout)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Boolean] =
    for {
      annualSubmission <- getAnnualSubmission(ctx, nino)
      annualSubmissionRequest <-
        ITPEnvelope.liftPure(PropertyAnnualSubmission.fromUkRentARoomAbout(rarAbout, annualSubmission))
      maybePeriodicSubmission <- getPeriodicSubmission(ctx.taxYear, nino, ctx.incomeSourceId)
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
      res <- mongoService.persistAnswers(
               ctx,
               IsClaimExpensesOrRRR(rarAbout.claimExpensesOrRelief.isClaimExpensesOrRelief)
             )

    } yield res

  def savePropertyAbout(ctx: JourneyContext, propertyAbout: PropertyAbout)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Boolean] = mongoService.persistAnswers(ctx, propertyAbout)


  def savePropertyRentalAbout(ctx: JourneyContext, propertyRentalsAbout: PropertyRentalsAbout)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Boolean] = mongoService.persistAnswers(ctx, propertyRentalsAbout)

  def saveRentalsAndRaRAbout(ctx: JourneyContext, nino: Nino, rentalsAndRaRAbout: RentalsAndRaRAbout)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Boolean] =
    for {
      annualSubmission <- getAnnualSubmission(ctx, nino)
      annualSubmissionRequest <-
        ITPEnvelope.liftPure(
          PropertyAnnualSubmission.fromRentalsAndRentARoomAbout(rentalsAndRaRAbout, annualSubmission)
        )
      maybePeriodicSubmission <- getPeriodicSubmission(ctx.taxYear, nino, ctx.incomeSourceId)
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
      _ <- mongoService.persistAnswers(
             ctx,
             IsClaimExpensesOrRRR(rentalsAndRaRAbout.claimExpensesOrRelief.isClaimExpensesOrRelief)
           )
      res <- mongoService.persistAnswers(
               ctx,
               IsClaimPropertyIncomeAllowance(rentalsAndRaRAbout.isClaimPropertyIncomeAllowance)
             )

    } yield res

  def saveExpenses(ctx: JourneyContext, nino: Nino, expenses: Expenses)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {
      maybePeriodicSubmission <- getPeriodicSubmission(ctx.taxYear, nino, ctx.incomeSourceId)
      updatePeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(UpdateUKPropertyPeriodicSubmissionRequest.fromEntity(maybePeriodicSubmission, expenses))
      createPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          CreateUKPropertyPeriodicSubmissionRequest.fromEntity(ctx.taxYear, maybePeriodicSubmission, expenses)
        )
      submissionResponse <- maybePeriodicSubmission match {
                              case None =>
                                createPeriodicSubmission(
                                  nino,
                                  ctx.incomeSourceId,
                                  ctx.taxYear,
                                  createPeriodicSubmissionRequest
                                )
                              case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _, _)) =>
                                updatePeriodicSubmission(
                                  nino,
                                  ctx.incomeSourceId,
                                  ctx.taxYear,
                                  submissionId.submissionId,
                                  updatePeriodicSubmissionRequest
                                ).map(_ => Some(submissionId))
                              case _ =>
                                ITPEnvelope.liftEither(
                                  InternalError("No submission id fetched").asLeft[Option[PeriodicSubmissionId]]
                                )
                            }
      _ <- mongoService.persistAnswers(
        ctx,
        ExpensesStoreAnswers(expenses.consolidatedExpenses.exists(_.isConsolidatedExpenses))
      )
    } yield submissionResponse

  def saveRaRExpenses(ctx: JourneyContext, nino: Nino, raRExpenses: RentARoomExpenses)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {
      maybePeriodicSubmission <- getPeriodicSubmission(ctx.taxYear, nino, ctx.incomeSourceId)
      updatePeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          UpdateUKPropertyPeriodicSubmissionRequest.fromEntity(maybePeriodicSubmission, raRExpenses)
        )
      createPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          CreateUKPropertyPeriodicSubmissionRequest.fromEntity(ctx.taxYear, maybePeriodicSubmission, raRExpenses)
        )
      submissionResponse <- maybePeriodicSubmission match {
                              case None =>
                                createPeriodicSubmission(
                                  nino,
                                  ctx.incomeSourceId,
                                  ctx.taxYear,
                                  createPeriodicSubmissionRequest
                                )
                              case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _, _)) =>
                                updatePeriodicSubmission(
                                  nino,
                                  ctx.incomeSourceId,
                                  ctx.taxYear,
                                  submissionId.submissionId,
                                  updatePeriodicSubmissionRequest
                                ).map(_ => Some(submissionId))
                              case _ =>
                                ITPEnvelope.liftEither(
                                  InternalError("No submission id fetched").asLeft[Option[PeriodicSubmissionId]]
                                )
                            }
      _ <- mongoService.persistAnswers(
        ctx,
        RentARoomExpensesStoreAnswers(raRExpenses.consolidatedExpenses.exists(_.isConsolidatedExpenses))
      )
    } yield submissionResponse

  def deletePropertyAnnualSubmission(incomeSourceId: IncomeSourceId, taxableEntityId: Nino, taxYear: TaxYear)(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[Unit] =
    EitherT(connector.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear))
      .bimap(error => ApiServiceError(error.status), result => result)

  def createPeriodicSubmission(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
    body: CreateUKPropertyPeriodicSubmissionRequest
  )(implicit hc: HeaderCarrier): ITPEnvelope[Option[PeriodicSubmissionId]] =
    EitherT(connector.createPeriodicSubmission(taxYear, nino, incomeSourceId, body)).leftMap { e =>
      logger.error(s"[createPeriodicSubmission] Error when creating Periodic Submission $e")
      ApiServiceError(e.status)
    }

  def updatePeriodicSubmission(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
    submissionId: String,
    updatePropertyPeriodicSubmissionRequest: UpdateUKPropertyPeriodicSubmissionRequest
  )(implicit
    hc: HeaderCarrier
  ): ITPEnvelope[String] =
    EitherT(
      connector
        .updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, updatePropertyPeriodicSubmissionRequest)
    )
      .bimap(
        error => {
          logger.error(
            s"[updatePeriodicSubmission] Error updating periodic submission: URL: $submissionId Status: ${error.status} Error Body: ${error.body}"
          )
          ApiServiceError(error.status)
        },
        _ => ""
      )

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

  private def transformToResponse(
    submissions: List[PropertyPeriodicSubmission]
  ): Either[ServiceError, PropertyPeriodicSubmissionResponse] =
    Right(PropertyPeriodicSubmissionResponse(submissions))

  def savePropertyRentalAdjustments(
    context: JourneyContext,
    nino: Nino,
    propertyRentalAdjustment: PropertyRentalAdjustments
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {

    val adjustmentStoreAnswers = AdjustmentStoreAnswers(
      propertyRentalAdjustment.balancingCharge.isBalancingCharge,
      propertyRentalAdjustment.renovationAllowanceBalancingCharge.isRenovationAllowanceBalancingCharge,
      propertyRentalAdjustment.unusedLossesBroughtForward.isUnusedLossesBroughtForward)
    for {
      maybePeriodicSubmission <- getPeriodicSubmission(
                                   context.taxYear,
                                   nino,
                                   context.incomeSourceId
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
      res <- mongoService.persistAnswers(context, adjustmentStoreAnswers)
      lbfSubmissionSuccess <- {
        (propertyRentalAdjustment.unusedLossesBroughtForward, propertyRentalAdjustment.whenYouReportedTheLoss) match {
          case (UnusedLossesBroughtForward(_, Some(lossAmount)), Some(taxYearBroughtForwardFrom)) =>
            createOrUpdateBroughtForwardLoss(
              taxYearBroughtForwardFrom,
              nino,
              context,
              lossAmount
            ).map(_ => true)
          case _ => ITPEnvelope.liftPure(false)
        }
      }
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
          UpdateUKPropertyPeriodicSubmissionRequest
            .fromEntity(maybePeriodicSubmission, entity)
        )
      createPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          CreateUKPropertyPeriodicSubmissionRequest
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
                              case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _, _)) =>
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

  def saveRaRAdjustments(ctx: JourneyContext, nino: Nino, raRAdjustments: RaRAdjustments)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, ServiceError, Boolean] = {

    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)

    for {
      propertyAnnualSubmissionFromDownstream <-
        this
          .getPropertyAnnualSubmission(
            ctx.taxYear,
            nino,
            ctx.incomeSourceId
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
          case Some(BalancingCharge(isRaRbalancingCharge, _)) =>
            mongoService.persistAnswers(ctx, IsRaRBalancingCharge(isRaRbalancingCharge))
          case _ => ITPEnvelope.liftPure(true)
        }
      }
      lbfSubmissionSuccess <- {
        (raRAdjustments.unusedLossesBroughtForward, raRAdjustments.whenYouReportedTheLoss) match {
          case (Some(UnusedLossesBroughtForward(_, Some(lossAmount))), Some(taxYearBroughtForwardFrom)) =>
            createOrUpdateBroughtForwardLoss(
              taxYearBroughtForwardFrom,
              nino,
              ctx,
              lossAmount
            ).map(_ => true)
          case _ => ITPEnvelope.liftPure(false)
        }
      }
    } yield res
  }

  def savePropertyRentalAllowances(
    ctx: JourneyContext,
    nino: Nino,
    rentalAllowances: RentalAllowances
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {

    val rentalAllowancesStoreAnswers = RentalAllowancesStoreAnswers(rentalAllowances.capitalAllowancesForACar.exists(_.isCapitalAllowancesForACar))

    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)

    for {
      _ <- rentalAllowances.capitalAllowancesForACar match {
             case Some(CapitalAllowancesForACar(false, _)) => ITPEnvelope.liftPure(())
             case _ => for {
               propertyAnnualSubmissionFromDownstream <-
                 this
                   .getPropertyAnnualSubmission(
                     ctx.taxYear,
                     nino,
                     ctx.incomeSourceId
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
                   .fromRentalAllowances(propertyAnnualSubmissionFromDownstream, rentalAllowances)
               )
             } yield ()
           }
      res <- mongoService.persistAnswers(ctx, rentalAllowancesStoreAnswers)
    } yield res
  }

  def saveRentARoomAllowances(
    ctx: JourneyContextWithNino,
    rentARoomAllowances: RentARoomAllowances
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {
    val rentARoomAllowancesStoreAnswers = RentARoomAllowancesStoreAnswers.fromJourneyAnswers(rentARoomAllowances)

    val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)

    for {
      _ <- rentARoomAllowances.capitalAllowancesForACar match {
        case Some(CapitalAllowancesForACar(false, _)) => ITPEnvelope.liftPure(())
        case _ => for {
          propertyAnnualSubmissionFromDownstream <-
            this
              .getPropertyAnnualSubmission(
                ctx.taxYear,
                ctx.nino,
                ctx.incomeSourceId
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
        } yield ()
      }
      res <- mongoService.persistAnswers(ctx.toJourneyContext(JourneyName.RentARoomAllowances), rentARoomAllowancesStoreAnswers)
    } yield res
  }

}
