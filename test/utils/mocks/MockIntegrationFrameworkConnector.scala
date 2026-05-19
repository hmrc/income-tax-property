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
import models.request.foreign._
import models.request.foreignincome.ForeignIncomeSubmission
import models.request.{CreateUKPropertyPeriodicSubmissionRequest, UpdateUKPropertyPeriodicSubmissionRequest, WhenYouReportedTheLoss}
import models.responses._
import org.mockito.ArgumentMatchersSugar
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockIntegrationFrameworkConnector extends MockitoSugar with ArgumentMatchersSugar {

  protected val mockIntegrationFrameworkConnector: IntegrationFrameworkConnector = mock[IntegrationFrameworkConnector]

  def mockGetAllPeriodicSubmissionIds(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ApiError, List[PeriodicSubmissionIdModel]]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .getAllPeriodicSubmissionIds(eqTo(taxYear), eqTo(taxableEntityId), eqTo(incomeSourceId))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockGetPropertyPeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    submissionId: String,
    result: Either[ApiError, Option[PropertyPeriodicSubmission]]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .getPropertyPeriodicSubmission(eqTo(taxYear), eqTo(taxableEntityId), eqTo(incomeSourceId), eqTo(submissionId))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockGetPropertyAnnualSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ApiError, Option[PropertyAnnualSubmission]]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .getPropertyAnnualSubmission(eqTo(taxYear), eqTo(taxableEntityId), eqTo(incomeSourceId))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockCreatePeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    createRequest: CreateUKPropertyPeriodicSubmissionRequest,
    result: Either[ApiError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .createPeriodicSubmission(eqTo(taxYear), eqTo(taxableEntityId), eqTo(incomeSourceId), eqTo(createRequest))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockCreateForeignPeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    createRequest: CreateForeignPropertyPeriodicSubmissionRequest,
    result: Either[ApiError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .createForeignPeriodicSubmission(eqTo(taxYear), eqTo(taxableEntityId), eqTo(incomeSourceId), eqTo(createRequest))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockUpdatePeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    updateRequest: UpdateUKPropertyPeriodicSubmissionRequest,
    submissionId: String,
    result: Either[ApiError, Option[String]]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .updatePeriodicSubmission(eqTo(taxableEntityId), eqTo(incomeSourceId), eqTo(taxYear), eqTo(submissionId), eqTo(updateRequest))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockUpdateForeignPeriodicSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    updateRequest: UpdateForeignPropertyPeriodicSubmissionRequest,
    submissionId: String,
    result: Either[ApiError, Option[String]]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .updateForeignPeriodicSubmission(eqTo(taxableEntityId), eqTo(incomeSourceId), eqTo(taxYear), eqTo(submissionId), eqTo(updateRequest))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockDeleteAnnualSubmissions(
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    taxYear: TaxYear,
    result: Either[ApiError, Unit]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .deletePropertyAnnualSubmission(eqTo(incomeSourceId), eqTo(taxableEntityId), eqTo(taxYear))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockCreateAnnualSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    result: Either[ApiError, Unit]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .createOrUpdateAnnualSubmission(eqTo(taxYear), eqTo(incomeSourceId), eqTo(taxableEntityId), any[PropertyAnnualSubmission])(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockCreateAnnualSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    propertyAnnualSubmission: Option[PropertyAnnualSubmission],
    result: Either[ApiError, Unit]
  ): Unit =
    propertyAnnualSubmission match {
      case Some(pas) =>
        when(mockIntegrationFrameworkConnector
          .createOrUpdateAnnualSubmission(eqTo(taxYear), eqTo(incomeSourceId), eqTo(taxableEntityId), eqTo(pas))(any[HeaderCarrier]))
          .thenReturn(Future.successful(result))
      case _ => mockCreateAnnualSubmission(taxYear, incomeSourceId, taxableEntityId, result)
    }

  def mockGetAnnualForeignPropertySubmission(
                                       taxYear: TaxYear,
                                       taxableEntityId: Nino,
                                       incomeSourceId: IncomeSourceId,
                                       result: Either[ApiError, Option[AnnualForeignPropertySubmission]]
                                     ): Unit =
    when(mockIntegrationFrameworkConnector
      .getAnnualForeignPropertySubmission(eqTo(taxYear), eqTo(taxableEntityId), eqTo(incomeSourceId))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))


  def mockCreateAnnualForeignSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    result: Either[ApiError, Unit]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .createOrUpdateAnnualForeignPropertySubmission(eqTo(taxYear), eqTo(incomeSourceId), eqTo(taxableEntityId), any[AnnualForeignPropertySubmission])(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockCreateAnnualForeignPropertySubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    maybeAnnualForeignPropertySubmission: Option[AnnualForeignPropertySubmission],
    result: Either[ApiError, Unit]
  ): Unit =
    maybeAnnualForeignPropertySubmission match {
      case Some(pas) =>
        when(mockIntegrationFrameworkConnector
          .createOrUpdateAnnualForeignPropertySubmission(eqTo(taxYear), eqTo(incomeSourceId), eqTo(taxableEntityId), eqTo(pas))(any[HeaderCarrier]))
          .thenReturn(Future.successful(result))
      case _ => mockCreateAnnualForeignSubmission(taxYear, incomeSourceId, taxableEntityId, result)
    }
  def mockCreateAnnualForeignPropertySubmissionAdjustments(
                                                            taxYear: TaxYear,
                                                            incomeSourceId: IncomeSourceId,
                                                            taxableEntityId: Nino,
                                                            result: Either[ApiError, Unit]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .createOrUpdateAnnualForeignPropertySubmissionAdjustments(eqTo(taxYear), eqTo(incomeSourceId), eqTo(taxableEntityId), any[AnnualForeignPropertySubmissionAdjustments])(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockCreateAnnualForeignPropertySubmissionAllowances(
                                                            taxYear: TaxYear,
                                                            incomeSourceId: IncomeSourceId,
                                                            taxableEntityId: Nino,
                                                            result: Either[ApiError, Unit]
                                                          ): Unit =
    when(mockIntegrationFrameworkConnector
      .createOrUpdateAnnualForeignPropertySubmissionAllowances(eqTo(taxYear), eqTo(incomeSourceId), eqTo(taxableEntityId), any[AnnualForeignPropertySubmissionAllowances])(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockGetPropertyBroughtForwardLoss(
    nino: Nino,
    lossId: String,
    result: Either[ApiError, BroughtForwardLossResponse]
  ): Unit =
    when(mockIntegrationFrameworkConnector.getBroughtForwardLoss(eqTo(nino), eqTo(lossId))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockGetBroughtForwardLosses(
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ApiError, BroughtForwardLosses]
  ): Unit =
    when(mockIntegrationFrameworkConnector
      .getBroughtForwardLosses(eqTo(taxYearBroughtForwardFrom), eqTo(nino), eqTo(incomeSourceId))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockCreatePropertyBroughtForwardLoss(
                                            taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
                                            nino: Nino,
                                            incomeSourceId: IncomeSourceId,
                                            lossAmount: BigDecimal,
                                            result: Either[ApiError, BroughtForwardLossId]
                                          ): Unit =
    when(mockIntegrationFrameworkConnector
      .createBroughtForwardLoss(eqTo(taxYearBroughtForwardFrom), eqTo(nino), eqTo(incomeSourceId), eqTo(lossAmount))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockUpdatePropertyBroughtForwardLoss(
                                            taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
                                            nino: Nino,
                                            lossId: String,
                                            lossAmount: BigDecimal,
                                            result: Either[ApiError, BroughtForwardLossResponse]
                                          ): Unit =
    when(mockIntegrationFrameworkConnector
      .updateBroughtForwardLoss(eqTo(taxYearBroughtForwardFrom), eqTo(nino), eqTo(lossId), eqTo(lossAmount))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockUpdateBroughtForwardLoss(
                                   taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
                                   nino: Nino,
                                   lossId: String,
                                   lossAmount: BigDecimal,
                                   result: Either[ApiError, BroughtForwardLossResponse]
                                 ): Unit =
    when(mockIntegrationFrameworkConnector
      .updateBroughtForwardLoss(eqTo(taxYearBroughtForwardFrom), eqTo(nino), eqTo(lossId), eqTo(lossAmount))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockGetForeignIncomeSubmission(
                                        taxYear: TaxYear,
                                        nino: Nino,
                                        result: Either[ApiError, Option[ForeignIncomeSubmission]]
                                      ): Unit =
    when(mockIntegrationFrameworkConnector
      .getForeignIncomeSubmission(eqTo(taxYear), eqTo(nino))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockCreateOrUpdateForeignIncomeSubmission(
                                                   taxYear: TaxYear,
                                                   nino: Nino,
                                                   body: ForeignIncomeSubmission,
                                                   result: Either[ApiError, Unit]
                                                 ): Unit =
    when(mockIntegrationFrameworkConnector
      .createOrUpdateForeignIncomeSubmission(eqTo(taxYear), eqTo(nino), eqTo(body))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockDeleteForeignIncomeSubmission(
                                           taxYear: TaxYear,
                                           nino: Nino,
                                           result: Either[ApiError, Unit]
                                         ): Unit =
    when(mockIntegrationFrameworkConnector
      .deleteForeignIncomeSubmission(eqTo(taxYear), eqTo(nino))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

}
