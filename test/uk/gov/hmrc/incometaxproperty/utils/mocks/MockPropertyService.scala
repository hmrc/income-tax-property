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

package uk.gov.hmrc.incometaxproperty.utils.mocks

import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.JsValue
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.incometaxproperty.models.PropertyPeriodicSubmissionResponse
import uk.gov.hmrc.incometaxproperty.models.errors.ServiceError
import uk.gov.hmrc.incometaxproperty.models.responses.PropertyAnnualSubmission
import uk.gov.hmrc.incometaxproperty.models.responses.PeriodicSubmissionId
import uk.gov.hmrc.incometaxproperty.services.PropertyService

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

  def mockCreateOrUpdateAnnualSubmissions(taxableEntityId: String,
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
}
