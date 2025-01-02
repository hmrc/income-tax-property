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
import models.errors._
import models.request.foreign._
import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.request.foreign.expenses.ForeignPropertyExpensesWithCountryCode
import models.responses._
import models.{ForeignPropertyExpensesStoreAnswers, ITPEnvelope, PropertyPeriodicSubmissionResponse}
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ForeignPropertyService @Inject() (
  connector: IntegrationFrameworkConnector,
  repository: MongoJourneyAnswersRepository
)(implicit ec: ExecutionContext)
    extends Logging {

  def persistAnswers[A](ctx: JourneyContext, answers: A)(implicit
    writes: Writes[A]
  ): EitherT[Future, ServiceError, Boolean] =
    EitherT(
      repository.upsertAnswers(ctx, Json.toJson(answers)).map {
        case false => RepositoryError.asLeft[Boolean]
        case true  => true.asRight[ServiceError]
      }
    )
  private def persistForeignAnswers[A](ctx: JourneyContext, answers: A, countryCode: String)(implicit
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
      _ <- foreignPropertyExpensesWithCountryCode.consolidatedExpenses match {
             case Some(consolidatedExpenses) =>
               persistForeignAnswers(
                 journeyContext,
                 ForeignPropertyExpensesStoreAnswers(
                   consolidatedExpensesYesOrNo = consolidatedExpenses.consolidatedOrIndividualExpensesYesNo
                 ),
                 foreignPropertyExpensesWithCountryCode.countryCode
               ).map(isPersistSuccess =>
                 if (!isPersistSuccess) {
                   logger.error("Could not persist Foreign Expenses")
                 } else {
                   logger.info("Foreign Expenses persisted successfully")
                 }
               )
             case _ =>
               ITPEnvelope.liftPure(None)
           }
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
      _ <- persistForeignAnswers(
             journeyContext,
             ForeignIncomeStoreAnswers(
               premiumsGrantLeaseReceived = foreignIncome.premiumsGrantLeaseReceived,
               premiumsOfLeaseGrantAgreed =
                 foreignIncome.premiumsOfLeaseGrantAgreed.fold(false)(_.premiumsOfLeaseGrantAgreed),
               reversePremiumsReceived = foreignIncome.reversePremiumsReceived.reversePremiumsReceived,
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

  def getForeignPropertyAnnualSubmissionFromDownStream(taxYear: TaxYear, taxableEntityId: Nino, incomeSourceId: IncomeSourceId)(
    implicit hc: HeaderCarrier
  ): ITPEnvelope[ForeignPropertyAnnualSubmission] =
    EitherT(connector.getForeignPropertyAnnualSubmission(taxYear, taxableEntityId, incomeSourceId))
      .leftMap(error => ApiServiceError(error.status))
      .subflatMap { foreignPropertyAnnualSubmission =>
        foreignPropertyAnnualSubmission.fold[Either[ServiceError, ForeignPropertyAnnualSubmission]](
          DataNotFoundError.asLeft[ForeignPropertyAnnualSubmission]
        )(_.asRight[ServiceError])
      }

  def createOrUpdateForeignPropertyAnnualSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino,
    body: ForeignPropertyAnnualSubmission
  )(implicit hc: HeaderCarrier): ITPEnvelope[Unit] =
    body match {
      case ForeignPropertyAnnualSubmission(None, None) =>
        ITPEnvelope.liftPure(())
      case _ =>
        EitherT(
          connector.createOrUpdateForeignPropertyAnnualSubmission(taxYear, incomeSourceId, nino, body)
        ).leftMap(e => ApiServiceError(e.status))
    }

  def saveForeignPropertyAllowances(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertyAllowancesWithCountryCode: ForeignPropertyAllowancesWithCountryCode
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {

    val emptyForeignPropertyAnnualSubmission = ForeignPropertyAnnualSubmission(None, None)

    for {
      foreignPropertyAnnualSubmissionFromDownstream <-
        this
          .getForeignPropertyAnnualSubmissionFromDownStream(
            journeyContext.taxYear,
            nino,
            journeyContext.incomeSourceId
          )
          .leftFlatMap {
            case DataNotFoundError => ITPEnvelope.liftPure(emptyForeignPropertyAnnualSubmission)
            case e                 => ITPEnvelope.liftEither(e.asLeft[ForeignPropertyAnnualSubmission])
          }
      _ <- createOrUpdateForeignPropertyAnnualSubmission(
             journeyContext.taxYear,
             journeyContext.incomeSourceId,
             nino,
             ForeignPropertyAnnualSubmission
               .fromForeignPropertyAllowances(
                 foreignPropertyAnnualSubmissionFromDownstream,
                 foreignPropertyAllowancesWithCountryCode
               )
           )
      isPersistSuccess <- persistForeignAnswers(
                            journeyContext,
                            foreignPropertyAllowancesWithCountryCode,
                            foreignPropertyAllowancesWithCountryCode.countryCode
                          ).map(isPersistSuccess =>
                            if (!isPersistSuccess) {
                              logger.error("Could not persist Foreign property allowances")
                              false
                            } else {
                              logger.info("Foreign property allowances persisted successfully")
                              true
                            }
                          )
    } yield isPersistSuccess

  }

}
