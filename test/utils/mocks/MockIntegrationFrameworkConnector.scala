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
import models.request.foreign.{AnnualForeignPropertySubmission, AnnualForeignPropertySubmissionAdjustments, UpdateForeignPropertyPeriodicSubmissionRequest, CreateForeignPropertyPeriodicSubmissionRequest, AnnualForeignPropertySubmissionAllowances}
import models.request.foreignincome.ForeignIncomeSubmissionDividends
import models.request.{WhenYouReportedTheLoss, CreateUKPropertyPeriodicSubmissionRequest, UpdateUKPropertyPeriodicSubmissionRequest}
import models.responses._
import org.scalamock.handlers.{CallHandler6, CallHandler4, CallHandler5}
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockIntegrationFrameworkConnector extends MockFactory {

  protected val mockIntegrationFrameworkConnector: IntegrationFrameworkConnector = mock[IntegrationFrameworkConnector]

  def mockGetAllPeriodicSubmissionIds(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ApiError, List[PeriodicSubmissionIdModel]]
  ): CallHandler4[TaxYear, Nino, IncomeSourceId, HeaderCarrier, Future[
    Either[ApiError, List[PeriodicSubmissionIdModel]]
  ]] =
    (mockIntegrationFrameworkConnector
      .getAllPeriodicSubmissionIds(_: TaxYear, _: Nino, _: IncomeSourceId)(_: HeaderCarrier))
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
    (mockIntegrationFrameworkConnector
      .getPropertyPeriodicSubmission(_: TaxYear, _: Nino, _: IncomeSourceId, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, submissionId, *)
      .returning(Future.successful(result))

  def mockGetPropertyAnnualSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ApiError, Option[PropertyAnnualSubmission]]
  ): CallHandler4[TaxYear, Nino, IncomeSourceId, HeaderCarrier, Future[
    Either[ApiError, Option[PropertyAnnualSubmission]]
  ]] =
    (mockIntegrationFrameworkConnector
      .getPropertyAnnualSubmission(_: TaxYear, _: Nino, _: IncomeSourceId)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))

  def mockCreatePeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    createRequest: CreateUKPropertyPeriodicSubmissionRequest,
    result: Either[ApiError, Option[PeriodicSubmissionId]]
  ): CallHandler5[TaxYear, Nino, IncomeSourceId, CreateUKPropertyPeriodicSubmissionRequest, HeaderCarrier, Future[
    Either[ApiError, Option[PeriodicSubmissionId]]
  ]] =
    (mockIntegrationFrameworkConnector
      .createPeriodicSubmission(_: TaxYear, _: Nino, _: IncomeSourceId, _: CreateUKPropertyPeriodicSubmissionRequest)(
        _: HeaderCarrier
      ))
      .expects(taxYear, taxableEntityId, incomeSourceId, createRequest, *)
      .returning(Future.successful(result))

  def mockCreateForeignPeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    createRequest: CreateForeignPropertyPeriodicSubmissionRequest,
    result: Either[ApiError, Option[PeriodicSubmissionId]]
  ): CallHandler5[TaxYear, Nino, IncomeSourceId, CreateForeignPropertyPeriodicSubmissionRequest, HeaderCarrier, Future[
    Either[ApiError, Option[PeriodicSubmissionId]]
  ]] =
    (mockIntegrationFrameworkConnector
      .createForeignPeriodicSubmission(
        _: TaxYear,
        _: Nino,
        _: IncomeSourceId,
        _: CreateForeignPropertyPeriodicSubmissionRequest
      )(
        _: HeaderCarrier
      ))
      .expects(taxYear, taxableEntityId, incomeSourceId, createRequest, *)
      .returning(Future.successful(result))

  def mockUpdatePeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    updateRequest: UpdateUKPropertyPeriodicSubmissionRequest,
    submissionId: String,
    result: Either[ApiError, Option[String]]
  ): CallHandler6[
    Nino,
    IncomeSourceId,
    TaxYear,
    String,
    UpdateUKPropertyPeriodicSubmissionRequest,
    HeaderCarrier,
    Future[
      Either[ApiError, Option[String]]
    ]
  ] =
    (mockIntegrationFrameworkConnector
      .updatePeriodicSubmission(
        _: Nino,
        _: IncomeSourceId,
        _: TaxYear,
        _: String,
        _: UpdateUKPropertyPeriodicSubmissionRequest
      )(
        _: HeaderCarrier
      ))
      .expects(taxableEntityId, incomeSourceId, taxYear, submissionId, updateRequest, *)
      .returning(Future.successful(result))

  def mockUpdateForeignPeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    updateRequest: UpdateForeignPropertyPeriodicSubmissionRequest,
    submissionId: String,
    result: Either[ApiError, Option[String]]
  ): CallHandler6[
    Nino,
    IncomeSourceId,
    TaxYear,
    String,
    UpdateForeignPropertyPeriodicSubmissionRequest,
    HeaderCarrier,
    Future[
      Either[ApiError, Option[String]]
    ]
  ] =
    (mockIntegrationFrameworkConnector
      .updateForeignPeriodicSubmission(
        _: Nino,
        _: IncomeSourceId,
        _: TaxYear,
        _: String,
        _: UpdateForeignPropertyPeriodicSubmissionRequest
      )(
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
    (mockIntegrationFrameworkConnector
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

  def mockGetAnnualForeignPropertySubmission(
                                       taxYear: TaxYear,
                                       taxableEntityId: Nino,
                                       incomeSourceId: IncomeSourceId,
                                       result: Either[ApiError, Option[AnnualForeignPropertySubmission]]
                                     ): CallHandler4[TaxYear, Nino, IncomeSourceId, HeaderCarrier, Future[
    Either[ApiError, Option[AnnualForeignPropertySubmission]]
  ]] =
    (mockIntegrationFrameworkConnector
      .getAnnualForeignPropertySubmission(_: TaxYear, _: Nino, _: IncomeSourceId)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))


  def mockCreateAnnualForeignSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    result: Either[ApiError, Unit]
  ): CallHandler5[TaxYear, IncomeSourceId, Nino, AnnualForeignPropertySubmission, HeaderCarrier, Future[
    Either[ApiError, Unit]
  ]] =
    (mockIntegrationFrameworkConnector
      .createOrUpdateAnnualForeignPropertySubmission(
        _: TaxYear,
        _: IncomeSourceId,
        _: Nino,
        _: AnnualForeignPropertySubmission
      )(
        _: HeaderCarrier
      ))
      .expects(taxYear, incomeSourceId, taxableEntityId, *, *)
      .returning(Future.successful(result))

  def mockCreateAnnualForeignPropertySubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    maybeAnnualForeignPropertySubmission: Option[AnnualForeignPropertySubmission],
    result: Either[ApiError, Unit]
  ): CallHandler5[TaxYear, IncomeSourceId, Nino, AnnualForeignPropertySubmission, HeaderCarrier, Future[
    Either[ApiError, Unit]
  ]] =
    maybeAnnualForeignPropertySubmission match {
      case Some(pas) =>
        (mockIntegrationFrameworkConnector
          .createOrUpdateAnnualForeignPropertySubmission(
            _: TaxYear,
            _: IncomeSourceId,
            _: Nino,
            _: AnnualForeignPropertySubmission
          )(
            _: HeaderCarrier
          ))
          .expects(taxYear, incomeSourceId, taxableEntityId, pas, *)
          .returning(Future.successful(result))
      case _ => mockCreateAnnualForeignSubmission(taxYear, incomeSourceId, taxableEntityId, result)
    }
  def mockCreateAnnualForeignPropertySubmissionAdjustments(
                                                            taxYear: TaxYear,
                                                            incomeSourceId: IncomeSourceId,
                                                            taxableEntityId: Nino,
                                                            result: Either[ApiError, Unit]
  ): CallHandler5[TaxYear, IncomeSourceId, Nino, AnnualForeignPropertySubmissionAdjustments, HeaderCarrier, Future[Either[ApiError, Unit]]] =
        (mockIntegrationFrameworkConnector
          .createOrUpdateAnnualForeignPropertySubmissionAdjustments(
            _: TaxYear,
            _: IncomeSourceId,
            _: Nino,
            _: AnnualForeignPropertySubmissionAdjustments
          )(
            _: HeaderCarrier
          ))
          .expects(taxYear, incomeSourceId, taxableEntityId, *, *)
          .returning(Future.successful(result))

  def mockCreateAnnualForeignPropertySubmissionAllowances(
                                                            taxYear: TaxYear,
                                                            incomeSourceId: IncomeSourceId,
                                                            taxableEntityId: Nino,
                                                            result: Either[ApiError, Unit]
                                                          ): CallHandler5[TaxYear, IncomeSourceId, Nino, AnnualForeignPropertySubmissionAllowances, HeaderCarrier, Future[Either[ApiError, Unit]]] =
    (mockIntegrationFrameworkConnector
      .createOrUpdateAnnualForeignPropertySubmissionAllowances(
        _: TaxYear,
        _: IncomeSourceId,
        _: Nino,
        _: AnnualForeignPropertySubmissionAllowances
      )(
        _: HeaderCarrier
      ))
      .expects(taxYear, incomeSourceId, taxableEntityId, *, *)
      .returning(Future.successful(result))

  def mockGetBroughtForwardLosses(
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ApiError, BroughtForwardLosses]
  ): CallHandler4[WhenYouReportedTheLoss, Nino, IncomeSourceId, HeaderCarrier, Future[Either[ApiError, BroughtForwardLosses]]] =
    (mockIntegrationFrameworkConnector
      .getBroughtForwardLosses(
        _: WhenYouReportedTheLoss,
        _: Nino,
        _: IncomeSourceId
      )(
        _: HeaderCarrier
      )
    )
      .expects(taxYearBroughtForwardFrom, nino, incomeSourceId, *)
      .returning(Future.successful(result))

  def mockUpdateBroughtForwardLoss(
                                   taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
                                   nino: Nino,
                                   lossId: String,
                                   lossAmount: BigDecimal,
                                   result: Either[ApiError, BroughtForwardLossResponse]
                                 ): CallHandler5[WhenYouReportedTheLoss, Nino, String, BigDecimal, HeaderCarrier, Future[Either[ApiError, BroughtForwardLossResponse]]] =
    (mockIntegrationFrameworkConnector
      .updateBroughtForwardLoss(
        _: WhenYouReportedTheLoss,
        _: Nino,
        _: String,
        _: BigDecimal
      )(
        _: HeaderCarrier
      )
      )
      .expects(taxYearBroughtForwardFrom, nino, lossId, lossAmount, *)
      .returning(Future.successful(result))

  def mockCreateForeignIncomeSubmissionDividends(
                                                            taxYear: TaxYear,
                                                            taxableEntityId: Nino,
                                                            result: Either[ApiError, Unit]
                                                          ): CallHandler4[TaxYear, Nino, ForeignIncomeSubmissionDividends, HeaderCarrier, Future[Either[ApiError, Unit]]] =
    (mockIntegrationFrameworkConnector
      .createOrUpdateForeignDividendsSubmission(
        _: TaxYear,
        _: Nino,
        _: ForeignIncomeSubmissionDividends
      )(
        _: HeaderCarrier
      ))
      .expects(taxYear, taxableEntityId, *, *)
      .returning(Future.successful(result))


}
