/*
 * Copyright 2025 HM Revenue & Customs
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

import connectors.HipConnector
import models.IncomeSourceType
import models.common.{IncomeSourceId, Nino}
import models.errors.ApiError
import models.request.WhenYouReportedTheLoss
import models.responses.{BroughtForwardLossId, HipPropertyBFLResponse}
import org.scalamock.handlers.{CallHandler3, CallHandler5, CallHandler6}
import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockHipConnector extends MockFactory { _: TestSuite =>

  protected val mockHipConnector: HipConnector = mock[HipConnector]

  def mockHipCreatePropertyBroughtForwardLossSubmission(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    incomeSourceType: IncomeSourceType,
    lossAmount: BigDecimal,
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
    result: Either[ApiError, BroughtForwardLossId]
  ): CallHandler6[Nino, IncomeSourceId, IncomeSourceType, BigDecimal, WhenYouReportedTheLoss, HeaderCarrier, Future[
    Either[ApiError, BroughtForwardLossId]
  ]] = (
    mockHipConnector
      .createPropertyBroughtForwardLoss(
        _: Nino,
        _: IncomeSourceId,
        _: IncomeSourceType,
        _: BigDecimal,
        _: WhenYouReportedTheLoss
      )(
        _: HeaderCarrier
      ))
      .expects(nino, incomeSourceId, incomeSourceType, lossAmount, taxYearBroughtForwardFrom, *)
      .returning(Future.successful(result))

  def mockHipGetPropertyBroughtForwardLossSubmission(
    nino: Nino,
    lossId: String,
    result: Either[ApiError, HipPropertyBFLResponse]
  ): CallHandler3[Nino, String, HeaderCarrier, Future[
    Either[ApiError, HipPropertyBFLResponse]
  ]] =
    (mockHipConnector
      .getPropertyBroughtForwardLoss(_: Nino, _: String)(_: HeaderCarrier))
      .expects(nino, lossId, *)
      .returning(Future.successful(result))

  def mockHipUpdatePropertyBroughtForwardLossSubmission(
                                                         nino: Nino,
                                                         broughtForwardLossAmount: BigDecimal,
                                                         taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
                                                         lossId: BroughtForwardLossId,
                                                         result: Either[ApiError, HipPropertyBFLResponse]
                                                       ): CallHandler5[Nino, BigDecimal, WhenYouReportedTheLoss, BroughtForwardLossId, HeaderCarrier, Future[
    Either[ApiError, HipPropertyBFLResponse]
  ]] = (
    mockHipConnector
      .updatePropertyBroughtForwardLoss(
        _: Nino,
        _: BigDecimal,
        _: WhenYouReportedTheLoss,
        _: BroughtForwardLossId
      )(
        _: HeaderCarrier
      ))
    .expects(nino, broughtForwardLossAmount, taxYearBroughtForwardFrom, lossId, *)
    .returning(Future.successful(result))
}
