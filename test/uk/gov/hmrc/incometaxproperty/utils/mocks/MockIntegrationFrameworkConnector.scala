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

import org.scalamock.handlers.{CallHandler2, CallHandler4, CallHandler5, CallHandler6}
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.JsValue
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.incometaxproperty.connectors.IntegrationFrameworkConnector
import uk.gov.hmrc.incometaxproperty.models.errors.ApiError
import uk.gov.hmrc.incometaxproperty.models.responses.{IncomeSourceDetailsModel, PeriodicSubmissionId, PeriodicSubmissionIdModel, PropertyAnnualSubmission, PropertyPeriodicSubmission}

import scala.concurrent.Future

trait MockIntegrationFrameworkConnector extends MockFactory {

  protected val mockIntegrationFrameworkConnector: IntegrationFrameworkConnector = mock[IntegrationFrameworkConnector]

  def mockGetBusinessDetails(nino: String,
                             result: Either[ApiError, Option[IncomeSourceDetailsModel]])
  : CallHandler2[String, HeaderCarrier, Future[Either[ApiError, Option[IncomeSourceDetailsModel]]]] = {
    (mockIntegrationFrameworkConnector.getBusinessDetails(_: String)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(Future.successful(result))
  }

  def mockGetAllPeriodicSubmission(taxYear: Int,
                                   taxableEntityId: String,
                                   incomeSourceId: String,
                                   result: Either[ApiError, List[PeriodicSubmissionIdModel]]
                                  ): CallHandler4[Int, String, String, HeaderCarrier, Future[Either[ApiError, List[PeriodicSubmissionIdModel]]]] = {
    (mockIntegrationFrameworkConnector.getAllPeriodicSubmission(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))
  }

  def mockGetPropertyPeriodicSubmission(taxYear: Int,
                                        taxableEntityId: String,
                                        incomeSourceId: String,
                                        submissionId: String,
                                        result: Either[ApiError, Option[PropertyPeriodicSubmission]]
                                       ): CallHandler5[Int, String, String, String, HeaderCarrier,
    Future[Either[ApiError, Option[PropertyPeriodicSubmission]]]] = {
    (mockIntegrationFrameworkConnector.getPropertyPeriodicSubmission(_: Int, _: String, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, submissionId, *)
      .returning(Future.successful(result))
  }

  def mockGetPropertyAnnualSubmission(taxYear: Int,
                                      taxableEntityId: String,
                                      incomeSourceId: String,
                                      result: Either[ApiError, Option[PropertyAnnualSubmission]]
                                     ): CallHandler4[Int, String, String, HeaderCarrier,
    Future[Either[ApiError, Option[PropertyAnnualSubmission]]]] = {
    (mockIntegrationFrameworkConnector.getPropertyAnnualSubmission(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))
  }

  def mockCreatePeriodicSubmission(taxYear: Int,
                                   taxableEntityId: String,
                                   incomeSourceId: String,
                                   result: Either[ApiError, Option[PeriodicSubmissionId]]
                                  ): CallHandler5[Int, String, String, JsValue, HeaderCarrier,
    Future[Either[ApiError, Option[PeriodicSubmissionId]]]] = {
    (mockIntegrationFrameworkConnector.createPeriodicSubmission(_: Int, _: String, _: String, _: JsValue)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *, *)
      .returning(Future.successful(result))
  }

  def mockUpdatePeriodicSubmission(taxYear: Int,
                                   taxableEntityId: String,
                                   incomeSourceId: String,
                                   submissionId: String,
                                   result: Either[ApiError, Option[String]]
                                  ): CallHandler6[String, String, Int, String, JsValue, HeaderCarrier,
    Future[Either[ApiError, Option[String]]]] = {
    (mockIntegrationFrameworkConnector.updatePeriodicSubmission(_: String, _: String, _: Int, _: String, _: JsValue)(_: HeaderCarrier))
      .expects(taxableEntityId, incomeSourceId, taxYear, submissionId, *, *)
      .returning(Future.successful(result))
  }

  def mockCreateAnnualSubmission(taxYear: Int,
                                   taxableEntityId: String,
                                   incomeSourceId: String,
                                 result: Either[ApiError,Unit]
                                  ): CallHandler5[Int, String, String, JsValue, HeaderCarrier,
    Future[Either[ApiError, Unit]]] = {
    (mockIntegrationFrameworkConnector.createOrUpdateAnnualSubmission(_: Int, _: String, _: String, _: JsValue)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *, *)
      .returning(Future.successful(result))
  }

}
