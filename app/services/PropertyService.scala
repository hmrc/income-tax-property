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
import models.request.{Income, PropertyRentalAdjustments, RentalAllowances}
import models.responses._
import models.{PropertyPeriodicSubmissionResponse, RentalAllowancesStoreAnswers}
import play.api.libs.Files.logger
import play.api.libs.json.{JsValue, Json, Writes}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Period
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PropertyService @Inject()(connector: IntegrationFrameworkConnector, repository: MongoJourneyAnswersRepository)
                               (implicit ec: ExecutionContext) {

  def saveIncome(taxYear: TaxYear,
                 nino: Nino,
                 incomeSourceId:
                 IncomeSourceId,
                 journeyContext: JourneyContext,
                 incomeToSave: Income,
                 ukOtherPropertyIncome: UkOtherPropertyIncome)(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Option[PeriodicSubmissionId]] = {
    for {
      r <- createPeriodicSubmission(
        nino.value,
        incomeSourceId.value,
        taxYear.endYear,
        PropertyPeriodicSubmissionRequest.fromUkOtherPropertyIncome(ukOtherPropertyIncome)
      )
      _ <- persistAnswers(journeyContext, incomeToSave).map(isPersistSuccess =>
        if (!isPersistSuccess) {
          logger.error("Could not persist")
        } else {
          logger.info("Persist successful")
        }
      )
    } yield r
  }

  def updateIncome(
                    taxYear: TaxYear,
                    nino: Nino,
                    incomeSourceId:
                    IncomeSourceId,
                    journeyContext: JourneyContext,
                    incomeToSave: Income,
                    ukOtherPropertyIncome: UkOtherPropertyIncome,
                    submissionId: SubmissionId
                  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, String] = {
    for {
      r <- updatePeriodicSubmission(
        nino.value,
        incomeSourceId.value,
        taxYear.endYear,
        submissionId.value,
        PropertyPeriodicSubmissionRequest.fromUkOtherPropertyIncome(ukOtherPropertyIncome)
      )
      _ <- persistAnswers(journeyContext, incomeToSave).map(isPersistSuccess =>
        if (!isPersistSuccess) {
          logger.error("Could not persist")
        } else {
          logger.info("Persist successful")
        }
      )
    } yield r
  }

  def getPropertyPeriodicSubmissions(taxYear: Int,
                                     taxableEntityId: String,
                                     incomeSourceId: String)
                                    (implicit hc: HeaderCarrier): ITPEnvelope[PropertyPeriodicSubmissionResponse] = {

    val result: ITPEnvelope[List[PropertyPeriodicSubmission]] =
      for {
        periodicSubmissionIds <- EitherT(connector.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId))
          .leftMap(error => ApiServiceError(error.status))
        propertyPeriodicSubmissions <- getPropertySubmissions(taxYear, taxableEntityId, incomeSourceId, periodicSubmissionIds)
      } yield propertyPeriodicSubmissions

    result.subflatMap(propertyPeriodicSubmissionList => transformToResponse(propertyPeriodicSubmissionList))
  }

  def getPropertyAnnualSubmission(taxYear: Int,
                                  taxableEntityId: String,
                                  incomeSourceId: String)
                                 (implicit hc: HeaderCarrier): ITPEnvelope[PropertyAnnualSubmission] = {
    EitherT(connector.getPropertyAnnualSubmission(taxYear, taxableEntityId, incomeSourceId))
      .leftMap(error => ApiServiceError(error.status))
      .subflatMap(annualSubmission => {
        annualSubmission.fold[Either[ServiceError, PropertyAnnualSubmission]](DataNotFoundError.asLeft[PropertyAnnualSubmission])(_.asRight[ServiceError])
      })
  }

  def deletePropertyAnnualSubmission(incomeSourceId: String,
                                     taxableEntityId: String,
                                     taxYear: Int)
                                    (implicit hc: HeaderCarrier): ITPEnvelope[Unit] = {

    EitherT(connector.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear))
      .bimap(error => ApiServiceError(error.status),
        result => result)
  }

  def createPeriodicSubmission(nino: String, incomeSourceId: String, taxYear: Int, body: PropertyPeriodicSubmissionRequest)
                              (implicit hc: HeaderCarrier): ITPEnvelope[Option[PeriodicSubmissionId]] = {

    EitherT(connector.createPeriodicSubmission(taxYear, nino, incomeSourceId, body)).leftMap(e => ApiServiceError(e.status))
  }

  def updatePeriodicSubmission(
                                nino: String,
                                incomeSourceId: String,
                                taxYear: Int,
                                submissionId: String,
                                propertyPeriodicSubmissionRequest: PropertyPeriodicSubmissionRequest
                              )
                              (
                                implicit hc: HeaderCarrier
                              ): ITPEnvelope[String] = {

    EitherT(connector.updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, propertyPeriodicSubmissionRequest))
      .bimap(error => ApiServiceError(error.status), _ => "")
  }

  @Deprecated
  def createOrUpdateAnnualSubmission(nino: String, incomeSourceId: String, taxYear: Int, body: Option[JsValue])
                                    (implicit hc: HeaderCarrier): ITPEnvelope[Unit] = {

    EitherT(connector.createOrUpdateAnnualSubmission(taxYear, nino, incomeSourceId, body.get))
      .bimap(error => ApiServiceError(error.status), _ => ())
  }

  def createOrUpdateAnnualSubmission(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino, body: PropertyAnnualSubmission)
                                    (implicit hc: HeaderCarrier): ITPEnvelope[Unit] = {
    EitherT(
      connector.createOrUpdateAnnualSubmission(taxYear, incomeSourceId, nino, body)
    ).leftMap(e => ApiServiceError(e.status))
  }

  private def getPropertySubmissions(taxYear: Int, taxableEntityId: String, incomeSourceId: String, periodicSubmissionIds: List[PeriodicSubmissionIdModel])
                                    (implicit hc: HeaderCarrier, ec: ExecutionContext): ITPEnvelope[List[PropertyPeriodicSubmission]] = {
    val propertyPeriodicSubmissions = periodicSubmissionIds
      .filter(submissionId => Period.between(submissionId.fromDate, submissionId.toDate).getYears >= 1)
      .map {
        submissionId =>
          // get each of the property periodic submission details
          connector.getPropertyPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId, submissionId.submissionId)
      }
    val all: Future[List[Either[ApiError, Option[PropertyPeriodicSubmission]]]] = Future.sequence(propertyPeriodicSubmissions) //.map(_.flatten)

    EitherT(
      all.map(list => {
        list.foldLeft[Either[ApiError, List[Option[PropertyPeriodicSubmission]]]](
          List[Option[PropertyPeriodicSubmission]]().asRight[ApiError]
        )((acc, a) => a match {
          case Left(e) => e.asLeft[List[Option[PropertyPeriodicSubmission]]]
          case Right(r) => acc.map(l => r :: l)
        })
      })).bimap(l => ApiServiceError(l.status), _.flatten)
  }


  private def transformToResponse(submissions: List[PropertyPeriodicSubmission]): Either[ServiceError, PropertyPeriodicSubmissionResponse] = {
    if (submissions.nonEmpty) {
      Right(PropertyPeriodicSubmissionResponse(submissions))
    } else {
      Left(DataNotFoundError)
    }
  }

  def persistAnswers[A](ctx: JourneyContext, answers: A)(implicit
                                                         writes: Writes[A]): EitherT[Future, ServiceError, Boolean] = {
    EitherT(
      repository.upsertAnswers(ctx, Json.toJson(answers)).map {
        case false => RepositoryError.asLeft[Boolean]
        case true => true.asRight[ServiceError]
      }
    )
  }

  def savePropertyRentalAdjustments(contextWithNino: JourneyContextWithNino, propertyRentalAdjustment: PropertyRentalAdjustments)
                                   (implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {

    val adjustmentStoreAnswers = AdjustmentStoreAnswers(propertyRentalAdjustment.balancingCharge.balancingChargeYesNo,
      propertyRentalAdjustment.businessPremisesRenovationAllowanceBalancingCharges.renovationAllowanceBalancingChargeYesNo)
    val ukOtherAdjustments = UkOtherAdjustments(
      None,
      propertyRentalAdjustment.balancingCharge.balancingChargeAmount,
      Some(propertyRentalAdjustment.privateUseAdjustment),
      propertyRentalAdjustment.businessPremisesRenovationAllowanceBalancingCharges.renovationAllowanceBalancingChargeAmount,
      None,
      None)

    val annualUkOtherProperty = AnnualUkOtherProperty(Some(ukOtherAdjustments), None)
    val propertyAnnualSubmission = PropertyAnnualSubmission(None, None, None, None, Some(annualUkOtherProperty))


    for {
      _ <- createOrUpdateAnnualSubmission(
        contextWithNino.taxYear,
        contextWithNino.incomeSourceId,
        contextWithNino.nino,
        propertyAnnualSubmission)

      res <- persistAnswers(contextWithNino.toJourneyContext(JourneyName.RentalAdjustments), adjustmentStoreAnswers)
    } yield res

  }

  def savePropertyRentalAllowances(ctx: JourneyContextWithNino, answers: RentalAllowances)
                                  (implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {
    val storeAnswers = RentalAllowancesStoreAnswers.fromJourneyAnswers(answers)
    val submission: PropertyAnnualSubmission = getPropertySubmission(answers)
    for {
      _ <- createOrUpdateAnnualSubmission(ctx.taxYear, ctx.incomeSourceId, ctx.nino, submission)
      res <- persistAnswers(ctx.toJourneyContext(JourneyName.RentalAllowances), storeAnswers)
    } yield res
  }

  private def getPropertySubmission(answers: RentalAllowances) = {
    val allowances = UkOtherAllowances(answers.annualInvestmentAllowance,
      answers.zeroEmissionGoodsVehicleAllowance,
      answers.businessPremisesRenovationAllowance,
      answers.otherCapitalAllowance,
      answers.replacementOfDomesticGoodsAllowance,
      answers.electricChargePointAllowance.electricChargePointAllowanceAmount,
      None,
      None,
      answers.zeroEmissionCarAllowance,
      None
    )
    val annualUkOtherProperty = AnnualUkOtherProperty(None, Some(allowances))
    PropertyAnnualSubmission(None, None, None, None, Some(annualUkOtherProperty))
  }
}
