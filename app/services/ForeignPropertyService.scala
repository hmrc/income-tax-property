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
import models.errors._
import models.request.foreign._
import models.request.foreign.adjustments.ForeignPropertyAdjustmentsWithCountryCode
import models.request.foreign.allowances.{CapitalAllowancesForACar, ForeignPropertyAllowancesWithCountryCode}
import models.request.foreign.expenses.ForeignPropertyExpensesWithCountryCode
import models.request.foreign.sba.ForeignPropertySbaWithCountryCode
import models.responses._
import play.api.Logging
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ForeignPropertyService @Inject() (
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
        propertyPeriodicSubmissions <-
          getPropertySubmissions(taxYear, nino, incomeSourceId, periodicSubmissionIds)
      } yield {
        logger.debug(
          s"[getPropertyPeriodicSubmissions] Foreign Periodic submission ids from IF ids: ${periodicSubmissionIds.map(_.submissionId).mkString(", ")}"
        )
        logger.debug(
          s"[getPropertyPeriodicSubmissions] Foreign Periodic submission details from IF: $propertyPeriodicSubmissions"
        )
        propertyPeriodicSubmissions
      }

    result.subflatMap(propertyPeriodicSubmissionList => transformToResponse(propertyPeriodicSubmissionList))
  }

  def getCurrentPeriodicSubmission(taxYear: TaxYear, nino: Nino, incomeSourceId: IncomeSourceId)(implicit
                                                                                                 hc: HeaderCarrier
  ): ITPEnvelope[Option[PropertyPeriodicSubmission]] =
    getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId)
      .map(_.periodicSubmissions.headOption)
      .flatMap {
        case Some(newest) => ITPEnvelope.liftPure(Some(newest))
        case None         => ITPEnvelope.liftPure(None)
      }

  def getAnnualForeignPropertySubmissionFromDownStream(
                                                        taxYear: TaxYear,
                                                        taxableEntityId: Nino,
                                                        incomeSourceId: IncomeSourceId
                                                      )(implicit
                                                        hc: HeaderCarrier
                                                      ): ITPEnvelope[AnnualForeignPropertySubmission] =
    EitherT(connector.getAnnualForeignPropertySubmission(taxYear, taxableEntityId, incomeSourceId))
      .leftMap(error => ApiServiceError(error.status))
      .subflatMap { maybeAnnualForeignPropertySubmission =>
        maybeAnnualForeignPropertySubmission.fold[Either[ServiceError, AnnualForeignPropertySubmission]] {
          logger.error(s"[getAnnualForeignPropertySubmissionFromDownStream] For Foreign Property no Annual submission found in IF")
          DataNotFoundError.asLeft[AnnualForeignPropertySubmission]
        }(_.asRight[ServiceError])
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
            case Right(None) =>
              logger.error(s"[getPropertySubmissions] Foreign property submission details not found in IF")
              None.asRight[ApiError]
            case Left(e) =>
              logger.error(s"[getPropertySubmissions] Foreign property submission details error found in IF: ${e.status}")
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
          case Left(e) => e.asLeft[List[Option[PropertyPeriodicSubmission]]]
          case Right(r) => acc.map(l => r :: l)
        }
      )
    }).bimap(l => ApiServiceError(l.status), _.flatten)
  }

  def saveForeignPropertySelectCountry(
                                        ctx: JourneyContext,
                                        foreignPropertySelectCountry: ForeignPropertySelectCountry
                                      )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] =
    mongoService.persistAnswers(
      ctx,
      foreignPropertySelectCountry
    )

  def saveForeignPropertyExpenses(
                                   journeyContext: JourneyContext,
                                   nino: Nino,
                                   foreignPropertyExpensesWithCountryCode: ForeignPropertyExpensesWithCountryCode
                                 )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {
      currentPeriodicSubmission <- getCurrentPeriodicSubmission(
        journeyContext.taxYear,
        nino,
        journeyContext.incomeSourceId
      )
      submissionResponse <- createOrUpdatePeriodicSubmission(
        journeyContext.toJourneyContextWithNino(nino),
        currentPeriodicSubmission,
        foreignPropertyExpensesWithCountryCode
      )
      _ <- mongoService.persistForeignAnswers(
            journeyContext,
            ForeignPropertyExpensesStoreAnswers(
              isConsolidatedExpenses = foreignPropertyExpensesWithCountryCode.consolidatedExpenses.exists(_.isConsolidatedOrIndividualExpenses)
            ),
            foreignPropertyExpensesWithCountryCode.countryCode
          ).map(isPersistSuccess =>
            if (!isPersistSuccess) {
              logger.error("Could not persist Foreign Expenses")
            } else {
              logger.info("Foreign Expenses persisted successfully")
            }
          )
    } yield submissionResponse

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

      submissionResponse <- createOrUpdatePeriodicSubmission(
        journeyContext.toJourneyContextWithNino(nino),
        currentPeriodicSubmission,
        foreignPropertyTaxWithCountryCode
      )
      _ <- mongoService.persistForeignAnswers(
        journeyContext,
        ForeignPropertyTaxStoreAnswers(
          isForeignIncomeTax = foreignPropertyTaxWithCountryCode.foreignIncomeTax.map(_.isForeignIncomeTax)
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


  private def createOrUpdatePeriodicSubmission[T](
                                                   contextWithNino: JourneyContextWithNino,
                                                   maybePeriodicSubmission: Option[PropertyPeriodicSubmission],
                                                   entity: T
                                                 )(implicit hc: HeaderCarrier): ITPEnvelope[Option[PeriodicSubmissionId]] =
    for {
      updatePeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          UpdateForeignPropertyPeriodicSubmissionRequest
            .fromEntity(maybePeriodicSubmission, entity)
        )
      createPeriodicSubmissionRequest <-
        ITPEnvelope.liftEither(
          CreateForeignPropertyPeriodicSubmissionRequest
            .fromEntity(contextWithNino.taxYear, maybePeriodicSubmission, entity)
        )
      submissionResponse <- maybePeriodicSubmission match {
        case None =>
          createForeignPeriodicSubmission(
            contextWithNino.nino,
            contextWithNino.incomeSourceId,
            contextWithNino.taxYear,
            createPeriodicSubmissionRequest
          )
        case Some(PropertyPeriodicSubmission(Some(submissionId), _, _, _, _, _)) =>
          updateForeignPeriodicSubmission(
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

  def createForeignPeriodicSubmission(
                                       nino: Nino,
                                       incomeSourceId: IncomeSourceId,
                                       taxYear: TaxYear,
                                       body: CreateForeignPropertyPeriodicSubmissionRequest
                                     )(implicit hc: HeaderCarrier): ITPEnvelope[Option[PeriodicSubmissionId]] =
    EitherT(connector.createForeignPeriodicSubmission(taxYear, nino, incomeSourceId, body)).leftMap(e =>
      ApiServiceError(e.status)
    )

  private def updateForeignPeriodicSubmission(
                                               nino: Nino,
                                               incomeSourceId: IncomeSourceId,
                                               taxYear: TaxYear,
                                               submissionId: String,
                                               updateForeignPropertyPeriodicSubmissionRequest: UpdateForeignPropertyPeriodicSubmissionRequest
                                             )(implicit
                                               hc: HeaderCarrier
                                             ): ITPEnvelope[String] =
    EitherT(
      connector
        .updateForeignPeriodicSubmission(
          nino,
          incomeSourceId,
          taxYear,
          submissionId,
          updateForeignPropertyPeriodicSubmissionRequest
        )
    )
      .bimap(error => ApiServiceError(error.status), _ => "")



  private def transformToResponse(
                                   submissions: List[PropertyPeriodicSubmission]
                                 ): Either[ServiceError, PropertyPeriodicSubmissionResponse] =
    Right(PropertyPeriodicSubmissionResponse(submissions))

  def saveForeignIncome(
                         journeyContext: JourneyContext,
                         nino: Nino,
                         foreignIncome: ForeignIncomeWithCountryCode
                       )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] =
    for {
      currentPeriodicSubmission <- getCurrentPeriodicSubmission(
        journeyContext.taxYear,
        nino,
        journeyContext.incomeSourceId
      )

      submissionResponse <- createOrUpdatePeriodicSubmission(
        journeyContext.toJourneyContextWithNino(nino),
        currentPeriodicSubmission,
        foreignIncome
      )
      _ <- mongoService.persistForeignAnswers(
        journeyContext,
        ForeignIncomeStoreAnswers(
          premiumsGrantLeaseReceived = foreignIncome.premiumsGrantLeaseReceived,
          premiumsOfLeaseGrantAgreed =
            foreignIncome.premiumsOfLeaseGrantAgreed.fold(false)(_.premiumsOfLeaseGrantAgreed),
          calculatedPremiumLeaseTaxable =
            foreignIncome.calculatedPremiumLeaseTaxable.fold(false)(_.calculatedPremiumLeaseTaxable),
          twelveMonthPeriodsInLease = foreignIncome.twelveMonthPeriodsInLease,
          receivedGrantLeaseAmount = foreignIncome.receivedGrantLeaseAmount
        ),
        foreignIncome.countryCode
      ).map(isPersistSuccess =>
        if (!isPersistSuccess) {
          logger.error("Could not persist Foreign Income")
        } else {
          logger.info("Foreign Income persisted successfully")
        }
      )
    } yield submissionResponse



  def createOrUpdateAnnualForeignPropertySubmission(
                                                     taxYear: TaxYear,
                                                     incomeSourceId: IncomeSourceId,
                                                     nino: Nino,
                                                     body: AnnualForeignPropertySubmission
                                                   )(implicit hc: HeaderCarrier): ITPEnvelope[Boolean] =
    body match {
      case AnnualForeignPropertySubmission(None) =>
        ITPEnvelope.liftPure(false)
      case _ =>
        EitherT(
          connector.createOrUpdateAnnualForeignPropertySubmission(taxYear, incomeSourceId, nino, body)
        ).map(_ => true)
          .leftMap(e => ApiServiceError(e.status))
    }

  def createOrUpdateAnnualForeignPropertySubmissionAdjustments(
                                                                taxYear: TaxYear,
                                                                incomeSourceId: IncomeSourceId,
                                                                nino: Nino,
                                                                body: AnnualForeignPropertySubmissionAdjustments
                                                              )(implicit hc: HeaderCarrier): ITPEnvelope[Boolean] =
    body match {
      case AnnualForeignPropertySubmissionAdjustments(None) =>
        ITPEnvelope.liftPure(false)
      case _ =>
        EitherT(
          connector.createOrUpdateAnnualForeignPropertySubmissionAdjustments(taxYear, incomeSourceId, nino, body)
        ).map(_ => true)
          .leftMap(e => ApiServiceError(e.status))
    }

  private def createOrUpdateAnnualForeignPropertySubmissionAllowances(
                                                                       taxYear: TaxYear,
                                                                       incomeSourceId: IncomeSourceId,
                                                                       nino: Nino,
                                                                       body: AnnualForeignPropertySubmissionAllowances
                                                                     )(implicit hc: HeaderCarrier): ITPEnvelope[Boolean] =
    body match {
      case AnnualForeignPropertySubmissionAllowances(None) =>
        ITPEnvelope.liftPure(false)
      case _ =>
        EitherT(
          connector.createOrUpdateAnnualForeignPropertySubmissionAllowances(taxYear, incomeSourceId, nino, body)
        ).map(_ => true)
          .leftMap(e => ApiServiceError(e.status))
    }

  def saveForeignPropertyAllowances(
                                     journeyContext: JourneyContext,
                                     nino: Nino,
                                     foreignPropertyAllowancesWithCountryCode: ForeignPropertyAllowancesWithCountryCode
                                   )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {
    for {
      isSubmissionSuccess <- {
        foreignPropertyAllowancesWithCountryCode.capitalAllowancesForACar match {
          case Some(CapitalAllowancesForACar(false, _)) => ITPEnvelope.liftPure(true)
          case _ =>
            val emptyAnnualForeignPropertySubmission = AnnualForeignPropertySubmission(None)
            for {
              annualForeignPropertySubmissionFromDownstream <-
                this
                  .getAnnualForeignPropertySubmissionFromDownStream(
                    journeyContext.taxYear,
                    nino,
                    journeyContext.incomeSourceId
                  )
                  .leftFlatMap {
                    case DataNotFoundError => ITPEnvelope.liftPure(emptyAnnualForeignPropertySubmission)
                    case e                 => ITPEnvelope.liftEither(e.asLeft[AnnualForeignPropertySubmission])
                  }
              result <- {
                val annualForeignPropertySubmissionWithNewAllowances = AnnualForeignPropertySubmission
                  .fromForeignPropertyAllowances(
                    Option(annualForeignPropertySubmissionFromDownstream),
                    foreignPropertyAllowancesWithCountryCode
                  )

                createOrUpdateAnnualForeignPropertySubmissionAllowances(
                  journeyContext.taxYear,
                  journeyContext.incomeSourceId,
                  nino,
                  annualForeignPropertySubmissionWithNewAllowances
                )
              }
            } yield result
        }
      }
      _ <- mongoService.persistForeignAnswers(
             journeyContext,
             ForeignAllowancesStoreAnswers(
               zeroEmissionsCarAllowance = foreignPropertyAllowancesWithCountryCode.zeroEmissionsCarAllowance,
               zeroEmissionsGoodsVehicleAllowance =
                 foreignPropertyAllowancesWithCountryCode.zeroEmissionsGoodsVehicleAllowance,
               costOfReplacingDomesticItems = foreignPropertyAllowancesWithCountryCode.costOfReplacingDomesticItems,
               otherCapitalAllowance = foreignPropertyAllowancesWithCountryCode.otherCapitalAllowance,
               foreignPropertyAllowancesWithCountryCode.capitalAllowancesForACar.map(_.isCapitalAllowancesForACar)
             ),
             foreignPropertyAllowancesWithCountryCode.countryCode
           ).flatMap { isPersisted =>
             if (isPersisted) {
               logger.info("Foreign Property allowances persisted successfully")
             } else {
               logger.error("Could not persist Foreign Property allowances")
             }
             ITPEnvelope.liftPure(isPersisted)
           }
    } yield {
      logger.info("Foreign Allowances persisted successfully to Downstream IF:" + isSubmissionSuccess)
      isSubmissionSuccess
    }
  }

  def saveForeignPropertyAdjustments(
                                      journeyContext: JourneyContext,
                                      nino: Nino,
                                      foreignAdjustmentsWithCountryCode: ForeignPropertyAdjustmentsWithCountryCode
                                    )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] =
    for {
      _ <- {
        if (foreignAdjustmentsWithCountryCode.propertyIncomeAllowanceClaim.isDefined) {
          val annualForeignPropertySubmission =
            AnnualForeignPropertySubmission.fromForeignPropertyAdjustmentsPIA(foreignAdjustmentsWithCountryCode)
          createOrUpdateAnnualForeignPropertySubmission(
            journeyContext.taxYear,
            journeyContext.incomeSourceId,
            nino,
            annualForeignPropertySubmission
          )
        } else {
          val annualForeignPropertySubmissionWithNewAdjustments = AnnualForeignPropertySubmission
            .fromForeignPropertyAdjustments(
              foreignAdjustmentsWithCountryCode
            )
          createOrUpdateAnnualForeignPropertySubmissionAdjustments(
            journeyContext.taxYear,
            journeyContext.incomeSourceId,
            nino,
            annualForeignPropertySubmissionWithNewAdjustments
          )
        }

      }
      _ <- {
        if (foreignAdjustmentsWithCountryCode.propertyIncomeAllowanceClaim.isDefined) {
          ITPEnvelope.liftPure(Option.empty[PeriodicSubmissionId])
        } else {
          for {
            currentPeriodicSubmission <- getCurrentPeriodicSubmission(
              journeyContext.taxYear,
              nino,
              journeyContext.incomeSourceId
            )
            submissionResponse <- createOrUpdatePeriodicSubmission(
              journeyContext.toJourneyContextWithNino(nino),
              currentPeriodicSubmission,
              foreignAdjustmentsWithCountryCode
            )
          } yield submissionResponse
        }

      }
      res <- mongoService.persistForeignAnswers(
        journeyContext,
        ForeignAdjustmentsStoreAnswers(
          isBalancingCharge = foreignAdjustmentsWithCountryCode.balancingCharge.isBalancingCharge,
          isForeignUnusedResidentialFinanceCost =
            foreignAdjustmentsWithCountryCode.unusedResidentialFinanceCost.map(
              _.isForeignUnusedResidentialFinanceCost
            ),
          isUnusedLossesPreviousYears =
            foreignAdjustmentsWithCountryCode.unusedLossesPreviousYears.isUnusedLossesPreviousYears,
          whenYouReportedTheLoss = foreignAdjustmentsWithCountryCode.whenYouReportedTheLoss
        ),
        foreignAdjustmentsWithCountryCode.countryCode
      )
    } yield res

  def saveForeignPropertySba(
                              journeyContext: JourneyContext,
                              nino: Nino,
                              foreignPropertySbaWithCountryCode: ForeignPropertySbaWithCountryCode
                            )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {
    for {
      isSubmissionSuccess <- {
        if(!foreignPropertySbaWithCountryCode.claimStructureBuildingAllowance) {
          ITPEnvelope.liftPure(true)
        } else {
          val emptyAnnualForeignPropertySubmission = AnnualForeignPropertySubmission(None)
          for {
            annualForeignPropertySubmissionFromDownstream <-
              this.getAnnualForeignPropertySubmissionFromDownStream(journeyContext.taxYear, nino,
                  journeyContext.incomeSourceId)
                .leftFlatMap {
                  case DataNotFoundError => ITPEnvelope.liftPure(emptyAnnualForeignPropertySubmission)
                  case e                 => ITPEnvelope.liftEither(e.asLeft[AnnualForeignPropertySubmission])
                }
            submissionResult <- {
              val annualForeignPropertySubmissionWithNewAllowances = AnnualForeignPropertySubmission
                .fromForeignPropertySbas(Some(annualForeignPropertySubmissionFromDownstream), foreignPropertySbaWithCountryCode)
                .fold(_ => emptyAnnualForeignPropertySubmission, identity)

              createOrUpdateAnnualForeignPropertySubmission(
                journeyContext.taxYear,
                journeyContext.incomeSourceId,
                nino,
                annualForeignPropertySubmissionWithNewAllowances
              )
            }
          } yield submissionResult
        }
      }
      _ <- mongoService.persistForeignAnswers(
        journeyContext,
        ForeignPropertySbaStoreAnswers(
          claimStructureBuildingAllowance =
            foreignPropertySbaWithCountryCode.claimStructureBuildingAllowance
        ),
        foreignPropertySbaWithCountryCode.countryCode
      ).map(isPersistSuccess =>
        if (!isPersistSuccess) {
          logger.error("Could not persist Foreign Property Sba")
          false
        } else {
          logger.info("Foreign Property Sba persisted successfully")
          true
        }
      )
    } yield isSubmissionSuccess
  }

  def deleteForeignPropertyAnswers(
    ctx: JourneyContext,
    deleteJourneyAnswersRequest: DeleteJourneyAnswers
  ): EitherT[Future, ServiceError, Boolean] = {
    mongoService.deleteAnswers(
      ctx,
      deleteJourneyAnswersRequest.journeyNames.filter(JourneyName.foreignPropertyJourneyNames.map(_.toString).contains(_))
    )
  }
}
