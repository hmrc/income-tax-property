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
import models.responses.BroughtForwardLossId
import org.scalamock.handlers.CallHandler6
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockHipConnector extends MockFactory {

  protected val mockHipConnector: HipConnector = mock[HipConnector]

  def mockCreatePropertyBroughtForwardLossSubmission(
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
}
