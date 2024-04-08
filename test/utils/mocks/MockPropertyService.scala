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

import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.{JsValue, Writes}
import uk.gov.hmrc.http.HeaderCarrier
import models.PropertyPeriodicSubmissionResponse
import models.common.{JourneyContext, JourneyContextWithNino}
import models.errors.ServiceError
import models.request.RentalAllowances
import models.responses.PropertyAnnualSubmission
import models.responses.PeriodicSubmissionId
import services.PropertyService

import scala.concurrent.Future

trait MockPropertyService extends MockFactory {

  protected val mockPropertyService: PropertyService = mock[PropertyService]

  def mockGetAllPeriodicSubmissions(taxYear: Int,
                                    taxableEntityId: String,
                                    incomeSourceId: String,
                                    result: Either[ServiceError, PropertyPeriodicSubmissionResponse]
                                   ): CallHandler4[Int, String, String, HeaderCarrier, Future[Either[ServiceError, PropertyPeriodicSubmissionResponse]]] = {
    (mockPropertyService.getPropertyPeriodicSubmissions(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))
  }

  def mockGetAnnualSubmission(taxYear: Int,
                              taxableEntityId: String,
                              incomeSourceId: String,
                              result: Either[ServiceError, PropertyAnnualSubmission]
                             ): CallHandler4[Int, String, String, HeaderCarrier, Future[Either[ServiceError, PropertyAnnualSubmission]]] = {
    (mockPropertyService.getPropertyAnnualSubmission(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))
  }

  def mockCreatePeriodicSubmissions(taxableEntityId: String,
                                    incomeSourceId: String,
                                    taxYear: Int,
                                    body: Option[JsValue],
                                    result: Either[ServiceError, PeriodicSubmissionId]
                                   ): CallHandler5[String, String, Int, Option[JsValue], HeaderCarrier, Future[Either[ServiceError, PeriodicSubmissionId]]] = {
    (mockPropertyService.createPeriodicSubmission(_: String, _: String, _: Int, _: Option[JsValue])(_: HeaderCarrier))
      .expects(taxableEntityId, incomeSourceId, taxYear, *, *)
      .returning(Future.successful(result))
  }

  def mockUpdatePeriodicSubmissions(taxableEntityId: String,
                                    incomeSourceId: String,
                                    taxYear: Int,
                                    submissionId: String,
                                    body: Option[JsValue],
                                    result: Either[ServiceError, String]
                                   ): CallHandler6[String, String, Int, String, Option[JsValue], HeaderCarrier, Future[Either[ServiceError, String]]] = {
    (mockPropertyService.updatePeriodicSubmission(_: String, _: String, _: Int, _: String, _: Option[JsValue])(_: HeaderCarrier))
      .expects(taxableEntityId, incomeSourceId, taxYear, submissionId, *, *)
      .returning(Future.successful(result))
  }

  def mockDeleteAnnualSubmission(incomeSourceId: String,
                                 taxableEntityId: String,
                                 taxYear: Int,
                                 result: Either[ServiceError, Unit]
                                   ): CallHandler4[String, String, Int, HeaderCarrier, Future[Either[ServiceError, Unit]]] = {
    (mockPropertyService.deletePropertyAnnualSubmission(_: String, _: String, _: Int)(_: HeaderCarrier))
      .expects(incomeSourceId, taxableEntityId, taxYear, *)
      .returning(Future.successful(result))
  }

  def mockCreateOrUpdateAnnualSubmissions(taxableEntityId: String,
                                    incomeSourceId: String,
                                    taxYear: Int,
                                    body: Option[JsValue],
                                    result: Either[ServiceError, Unit]
                                   ): CallHandler5[String, String, Int, Option[JsValue], HeaderCarrier, Future[Either[ServiceError, Unit]]] = {
    (mockPropertyService.createOrUpdateAnnualSubmission(_: String, _: String, _: Int, _: Option[JsValue])(_: HeaderCarrier))
      .expects(taxableEntityId, incomeSourceId, taxYear, *, *)
      .returning(Future.successful(result))
  }

  def mockPersistAnswers[A](ctx: JourneyContext, answers: A): CallHandler3[JourneyContext, A, Writes[A], Future[Boolean]] = {
    (mockPropertyService.persistAnswers(_: JourneyContext, _: A)(_: Writes[A]))
      .expects(*, *, *)
      .returning(Future.successful(true))
  }

  def mockSavePropertyRentalAllowances[A](ctx: JourneyContextWithNino, answers: RentalAllowances):
  CallHandler3[JourneyContextWithNino, RentalAllowances, HeaderCarrier, Future[Either[ServiceError, Boolean]]] = {
    (mockPropertyService.savePropertyRentalAllowances(_: JourneyContextWithNino, _: RentalAllowances)(_: HeaderCarrier))
      .expects(*, *, *)
      .returning(Future.successful(Right(true)))
  }

}
