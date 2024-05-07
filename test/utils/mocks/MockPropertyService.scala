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

package utils.mocks

import cats.data.EitherT
import models.ITPEnvelope.ITPEnvelope
import models.{ITPEnvelope, PropertyPeriodicSubmissionResponse}
import models.common._
import models.errors.{ApiServiceError, ServiceError}
import models.request.{Income, PropertyRentalAdjustments, RentalAllowances}
import models.responses.{PeriodicSubmissionId, PropertyAnnualSubmission, PropertyPeriodicSubmissionRequest, UkOtherPropertyIncome}
import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.{JsValue, Writes}
import services.PropertyService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockPropertyService extends MockFactory {

  protected val mockPropertyService: PropertyService = mock[PropertyService]

  def mockGetAllPeriodicSubmissions(taxYear: Int,
                                    taxableEntityId: String,
                                    incomeSourceId: String,
                                    result: Either[ServiceError, PropertyPeriodicSubmissionResponse]
                                   ): CallHandler4[Int, String, String, HeaderCarrier, EitherT[Future, ServiceError, PropertyPeriodicSubmissionResponse]] = {
    (mockPropertyService.getPropertyPeriodicSubmissions(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(EitherT.fromEither(result))
  }

  def mockGetAnnualSubmission(taxYear: Int,
                              taxableEntityId: String,
                              incomeSourceId: String,
                              result: Either[ServiceError, PropertyAnnualSubmission]
                             ): CallHandler4[Int, String, String, HeaderCarrier, EitherT[Future, ServiceError, PropertyAnnualSubmission]] = {
    (mockPropertyService.getPropertyAnnualSubmission(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(EitherT.fromEither(result))
  }

  //TODO Replace JsValue with case class - same as the original createPeriodicSubmissions function in IF Connector
  def mockCreatePeriodicSubmissions(taxableEntityId: String,
                                    incomeSourceId: String,
                                    taxYear: Int,
                                    body: PropertyPeriodicSubmissionRequest,
                                    result: Either[ServiceError, Option[PeriodicSubmissionId]]
                                   ): CallHandler5[String, String, Int, PropertyPeriodicSubmissionRequest, HeaderCarrier, EitherT[Future, ServiceError, Option[PeriodicSubmissionId]]] = {
    (mockPropertyService.createPeriodicSubmission(_: String, _: String, _: Int, _: PropertyPeriodicSubmissionRequest)(_: HeaderCarrier))
      .expects(taxableEntityId, incomeSourceId, taxYear, *, *)
      .returning(EitherT.fromEither(result))
  }

  def mockSaveIncome(nino: Nino,
                     businessId: IncomeSourceId,
                     incomeSourceId: IncomeSourceId,
                     taxYear: TaxYear,
                     journeyContext: JourneyContext,
                     income: Income,
                     ukOtherPropertyIncome: UkOtherPropertyIncome,
                     result: Either[ServiceError, Option[PeriodicSubmissionId]]
                    ): CallHandler8[TaxYear, IncomeSourceId, Nino, IncomeSourceId, JourneyContext, Income, UkOtherPropertyIncome, HeaderCarrier, EitherT[Future, ServiceError, Option[PeriodicSubmissionId]]] = {
    (mockPropertyService
      .saveIncome(
        _: TaxYear,
        _: IncomeSourceId,
        _: Nino,
        _: IncomeSourceId,
        _: JourneyContext,
        _: Income,
        _: UkOtherPropertyIncome
      )(_: HeaderCarrier)).expects(taxYear, businessId, nino, incomeSourceId, journeyContext, income, ukOtherPropertyIncome, *)
      .returning(EitherT.fromEither(result))
  }

  //TODO Replace JsValue with case class - same as the original updatePeriodicSubmissions function in IF Connector
  def mockUpdatePeriodicSubmissions(taxableEntityId: String,
                                    incomeSourceId: String,
                                    taxYear: Int,
                                    submissionId: String,
                                    body: PropertyPeriodicSubmissionRequest,
                                    result: Either[ServiceError, String]
                                   ): CallHandler6[String, String, Int, String, PropertyPeriodicSubmissionRequest, HeaderCarrier, EitherT[Future, ServiceError, String]] = {
    (mockPropertyService.updatePeriodicSubmission(_: String, _: String, _: Int, _: String, _: PropertyPeriodicSubmissionRequest)(_: HeaderCarrier))
      .expects(taxableEntityId, incomeSourceId, taxYear, submissionId, body, *)
      .returning(EitherT.fromEither(result))
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def mockDeleteAnnualSubmission(incomeSourceId: String,
                                 taxableEntityId: String,
                                 taxYear: Int,
                                 result: Either[ServiceError, Unit]
                                ): CallHandler4[String, String, Int, HeaderCarrier, EitherT[Future, ServiceError, Unit]] = {
    (mockPropertyService.deletePropertyAnnualSubmission(_: String, _: String, _: Int)(_: HeaderCarrier))
      .expects(incomeSourceId, taxableEntityId, taxYear, *)
      .returning(EitherT.fromEither[Future](result))
  }

  def mockCreateOrUpdateAnnualSubmissions(taxYear: TaxYear,
                                          businessId: IncomeSourceId,
                                          nino: Nino,
                                          body: PropertyAnnualSubmission,
                                          result: Either[ServiceError, Unit]
                                         ): CallHandler5[TaxYear, IncomeSourceId, Nino, PropertyAnnualSubmission, HeaderCarrier, EitherT[Future, ServiceError, Unit]] = {
    (mockPropertyService.createOrUpdateAnnualSubmission(_: TaxYear, _: IncomeSourceId, _: Nino, _: PropertyAnnualSubmission)(_: HeaderCarrier))
      .expects(taxYear, businessId, nino, *, *)
      .returning(EitherT.fromEither(result))
  }

  def mockCreateOrUpdateAnnualSubmissionsNew2(taxableEntityId: String,
                                             incomeSourceId: String,
                                             taxYear: Int,
                                             incomeSubmissionId: String,
                                             body: PropertyPeriodicSubmissionRequest,
                                             result: Either[ServiceError, String]
                                            ): CallHandler6[String, String, Int, String, PropertyPeriodicSubmissionRequest, HeaderCarrier, ITPEnvelope[String]] = {
    (mockPropertyService.updatePeriodicSubmission(_: String, _: String, _: Int, _: String, _: PropertyPeriodicSubmissionRequest)(_: HeaderCarrier))
      .expects(taxableEntityId, incomeSourceId, taxYear, incomeSubmissionId, *, *)
      .returning(ITPEnvelope.liftEither(result))
  }

  def mockCreateOrUpdateAnnualSubmissionsNew(taxableEntityId: String,
                                          incomeSourceId: String,
                                          taxYear: Int,
                                          body: PropertyPeriodicSubmissionRequest,
                                          result: Either[ServiceError, Option[PeriodicSubmissionId]]
                                         ): CallHandler5[String, String, Int, PropertyPeriodicSubmissionRequest, HeaderCarrier, ITPEnvelope[Option[PeriodicSubmissionId]]] = {
    (mockPropertyService.createPeriodicSubmission(_: String, _: String, _: Int, _: PropertyPeriodicSubmissionRequest)(_: HeaderCarrier))
      .expects(taxableEntityId, incomeSourceId, taxYear, *, *)
      .returning(ITPEnvelope.liftEither(result))
  }

  def mockPersistAnswers[A](ctx: JourneyContext, answers: A): CallHandler3[JourneyContext, A, Writes[A], EitherT[Future, ServiceError, Boolean]] = {
    (mockPropertyService.persistAnswers(_: JourneyContext, _: A)(_: Writes[A]))
      .expects(*, *, *)
      .returning(EitherT.pure(true))
  }

  def mockSavePropertyRentalAllowances[A](ctx: JourneyContextWithNino, answers: RentalAllowances):
  CallHandler3[JourneyContextWithNino, RentalAllowances, HeaderCarrier, EitherT[Future, ServiceError, Boolean]] = {
    (mockPropertyService.savePropertyRentalAllowances(_: JourneyContextWithNino, _: RentalAllowances)(_: HeaderCarrier))
      .expects(*, *, *)
      .returning(EitherT.pure(true))
  }

  def mockSavePropertyRentalAdjustments[A](journeyContextWithNino: JourneyContextWithNino, propertyRentalAdjustments: PropertyRentalAdjustments):
  CallHandler3[JourneyContextWithNino, PropertyRentalAdjustments, HeaderCarrier, EitherT[Future, ServiceError, Boolean]] = {
    (mockPropertyService.savePropertyRentalAdjustments(_: JourneyContextWithNino, _: PropertyRentalAdjustments)(_: HeaderCarrier))
      .expects(*, *, *)
      .returning(EitherT.pure(true))
  }

}
