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

import connectors.IntegrationFrameworkConnector
import models.common.{IncomeSourceId, Nino, TaxYear}
import models.errors.ApiError
import models.request.{CreatePropertyPeriodicSubmissionRequest, UpdatePropertyPeriodicSubmissionRequest}
import models.responses._
import org.scalamock.handlers.{CallHandler2, CallHandler4, CallHandler5, CallHandler6}
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockIntegrationFrameworkConnector extends MockFactory {

  protected val mockIntegrationFrameworkConnector: IntegrationFrameworkConnector = mock[IntegrationFrameworkConnector]

  def mockGetBusinessDetails(
    nino: String,
    result: Either[ApiError, Option[IncomeSourceDetailsModel]]
  ): CallHandler2[String, HeaderCarrier, Future[Either[ApiError, Option[IncomeSourceDetailsModel]]]] =
    (mockIntegrationFrameworkConnector
      .getBusinessDetails(_: String)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(Future.successful(result))

  def mockGetAllPeriodicSubmission(
    taxYear: Int,
    taxableEntityId: String,
    incomeSourceId: String,
    result: Either[ApiError, List[PeriodicSubmissionIdModel]]
  ): CallHandler4[Int, String, String, HeaderCarrier, Future[Either[ApiError, List[PeriodicSubmissionIdModel]]]] =
    (mockIntegrationFrameworkConnector
      .getAllPeriodicSubmission(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))

  def mockGetPropertyPeriodicSubmission(
    taxYear: Int,
    taxableEntityId: String,
    incomeSourceId: String,
    submissionId: String,
    result: Either[ApiError, Option[PropertyPeriodicSubmission]]
  ): CallHandler5[Int, String, String, String, HeaderCarrier, Future[
    Either[ApiError, Option[PropertyPeriodicSubmission]]
  ]] =
    (mockIntegrationFrameworkConnector
      .getPropertyPeriodicSubmission(_: Int, _: String, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, submissionId, *)
      .returning(Future.successful(result))

  def mockGetPropertyAnnualSubmission(
    taxYear: Int,
    taxableEntityId: String,
    incomeSourceId: String,
    result: Either[ApiError, Option[PropertyAnnualSubmission]]
  ): CallHandler4[Int, String, String, HeaderCarrier, Future[Either[ApiError, Option[PropertyAnnualSubmission]]]] =
    (mockIntegrationFrameworkConnector
      .getPropertyAnnualSubmission(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))

  def mockCreatePeriodicSubmission(
    taxYear: Int,
    taxableEntityId: String,
    incomeSourceId: String,
    createRequest: CreatePropertyPeriodicSubmissionRequest,
    result: Either[ApiError, Option[PeriodicSubmissionId]]
  ): CallHandler5[Int, String, String, CreatePropertyPeriodicSubmissionRequest, HeaderCarrier, Future[
    Either[ApiError, Option[PeriodicSubmissionId]]
  ]] =
    (mockIntegrationFrameworkConnector
      .createPeriodicSubmission(_: Int, _: String, _: String, _: CreatePropertyPeriodicSubmissionRequest)(
        _: HeaderCarrier
      ))
      .expects(taxYear, taxableEntityId, incomeSourceId, createRequest, *)
      .returning(Future.successful(result))

  def mockUpdatePeriodicSubmission(
    taxYear: Int,
    taxableEntityId: String,
    incomeSourceId: String,
    updateRequest: UpdatePropertyPeriodicSubmissionRequest,
    submissionId: String,
    result: Either[ApiError, Option[String]]
  ): CallHandler6[String, String, Int, String, UpdatePropertyPeriodicSubmissionRequest, HeaderCarrier, Future[
    Either[ApiError, Option[String]]
  ]] =
    (mockIntegrationFrameworkConnector
      .updatePeriodicSubmission(_: String, _: String, _: Int, _: String, _: UpdatePropertyPeriodicSubmissionRequest)(
        _: HeaderCarrier
      ))
      .expects(taxableEntityId, incomeSourceId, taxYear, submissionId, updateRequest, *)
      .returning(Future.successful(result))

  def mockDeleteAnnualSubmissions(
    incomeSourceId: String,
    taxableEntityId: String,
    taxYear: Int,
    result: Either[ApiError, Unit]
  ): CallHandler4[String, String, Int, HeaderCarrier, Future[Either[ApiError, Unit]]] =
    (mockIntegrationFrameworkConnector
      .deletePropertyAnnualSubmission(_: String, _: String, _: Int)(_: HeaderCarrier))
      .expects(incomeSourceId, taxableEntityId, taxYear, *)
      .returning(Future.successful(result))

  def mockCreateAnnualSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    result: Either[ApiError, Unit]
  ): CallHandler5[TaxYear, IncomeSourceId, Nino, PropertyAnnualSubmission, HeaderCarrier, Future[
    Either[ApiError, Unit]
  ]] =
    (mockIntegrationFrameworkConnector
      .createOrUpdateAnnualSubmission(_: TaxYear, _: IncomeSourceId, _: Nino, _: PropertyAnnualSubmission)(
        _: HeaderCarrier
      ))
      .expects(taxYear, incomeSourceId, taxableEntityId, *, *)
      .returning(Future.successful(result))

  def mockCreateAnnualSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    propertyAnnualSubmission: Option[PropertyAnnualSubmission],
    result: Either[ApiError, Unit]
  ): CallHandler5[TaxYear, IncomeSourceId, Nino, PropertyAnnualSubmission, HeaderCarrier, Future[
    Either[ApiError, Unit]
  ]] =
    propertyAnnualSubmission match {
      case Some(pas) =>
        (mockIntegrationFrameworkConnector
          .createOrUpdateAnnualSubmission(_: TaxYear, _: IncomeSourceId, _: Nino, _: PropertyAnnualSubmission)(
            _: HeaderCarrier
          ))
          .expects(taxYear, incomeSourceId, taxableEntityId, pas, *)
          .returning(Future.successful(result))
      case _ => mockCreateAnnualSubmission(taxYear, incomeSourceId, taxableEntityId, result)
    }
}
