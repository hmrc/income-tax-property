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
import org.scalamock.handlers.{CallHandler4, CallHandler5, CallHandler6}
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockIntegrationConnector extends MockFactory {

  protected val mockIntegrationConnector: IntegrationFrameworkConnector = mock[IntegrationFrameworkConnector]

  def mockGetAllPeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ApiError, List[PeriodicSubmissionIdModel]]
  ): CallHandler4[TaxYear, Nino, IncomeSourceId, HeaderCarrier, Future[Either[ApiError, List[PeriodicSubmissionIdModel]]]] =
    (mockIntegrationConnector
      .getAllPeriodicSubmission(_: TaxYear, _: Nino, _: IncomeSourceId)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))

  def mockGetPropertyPeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    submissionId: String,
    result: Either[ApiError, Option[PropertyPeriodicSubmission]]
  ): CallHandler5[TaxYear, Nino, IncomeSourceId, String, HeaderCarrier, Future[
    Either[ApiError, Option[PropertyPeriodicSubmission]]
  ]] =
    (mockIntegrationConnector
      .getPropertyPeriodicSubmission(_: TaxYear, _: Nino, _: IncomeSourceId, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, submissionId, *)
      .returning(Future.successful(result))

  def mockGetPropertyAnnualSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ApiError, Option[PropertyAnnualSubmission]]
  ): CallHandler4[TaxYear, Nino, IncomeSourceId, HeaderCarrier, Future[Either[ApiError, Option[PropertyAnnualSubmission]]]] =
    (mockIntegrationConnector
      .getPropertyAnnualSubmission(_: TaxYear, _: Nino, _: IncomeSourceId)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))

  def mockCreatePeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    createRequest: CreatePropertyPeriodicSubmissionRequest,
    result: Either[ApiError, Option[PeriodicSubmissionId]]
  ): CallHandler5[TaxYear, Nino, IncomeSourceId, CreatePropertyPeriodicSubmissionRequest, HeaderCarrier, Future[
    Either[ApiError, Option[PeriodicSubmissionId]]
  ]] =
    (mockIntegrationConnector
      .createPeriodicSubmission(_: TaxYear, _: Nino, _: IncomeSourceId, _: CreatePropertyPeriodicSubmissionRequest)(
        _: HeaderCarrier
      ))
      .expects(taxYear, taxableEntityId, incomeSourceId, createRequest, *)
      .returning(Future.successful(result))

  def mockUpdatePeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    updateRequest: UpdatePropertyPeriodicSubmissionRequest,
    submissionId: String,
    result: Either[ApiError, Option[String]]
  ): CallHandler6[Nino, IncomeSourceId, TaxYear, String, UpdatePropertyPeriodicSubmissionRequest, HeaderCarrier, Future[
    Either[ApiError, Option[String]]
  ]] =
    (mockIntegrationConnector
      .updatePeriodicSubmission(_: Nino, _: IncomeSourceId, _: TaxYear, _: String, _: UpdatePropertyPeriodicSubmissionRequest)(
        _: HeaderCarrier
      ))
      .expects(taxableEntityId, incomeSourceId, taxYear, submissionId, updateRequest, *)
      .returning(Future.successful(result))

  def mockDeleteAnnualSubmissions(
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    taxYear: TaxYear,
    result: Either[ApiError, Unit]
  ): CallHandler4[IncomeSourceId, Nino, TaxYear, HeaderCarrier, Future[Either[ApiError, Unit]]] =
    (mockIntegrationConnector
      .deletePropertyAnnualSubmission(_: IncomeSourceId, _: Nino, _: TaxYear)(_: HeaderCarrier))
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
    (mockIntegrationConnector
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
        (mockIntegrationConnector
          .createOrUpdateAnnualSubmission(_: TaxYear, _: IncomeSourceId, _: Nino, _: PropertyAnnualSubmission)(
            _: HeaderCarrier
          ))
          .expects(taxYear, incomeSourceId, taxableEntityId, pas, *)
          .returning(Future.successful(result))
      case _ => mockCreateAnnualSubmission(taxYear, incomeSourceId, taxableEntityId, result)
    }
}
