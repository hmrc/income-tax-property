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
import org.mockito.ArgumentMatchersSugar
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockHipConnector extends MockitoSugar with ArgumentMatchersSugar {

  protected val mockHipConnector: HipConnector = mock[HipConnector]

  def mockHipCreatePropertyBroughtForwardLossSubmission(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    incomeSourceType: IncomeSourceType,
    lossAmount: BigDecimal,
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
    result: Either[ApiError, BroughtForwardLossId]
  ): Unit =
    when(mockHipConnector.createPropertyBroughtForwardLoss(eqTo(nino), eqTo(incomeSourceId), eqTo(incomeSourceType), eqTo(lossAmount), eqTo(taxYearBroughtForwardFrom))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockHipGetPropertyBroughtForwardLossSubmission(
    nino: Nino,
    lossId: String,
    result: Either[ApiError, HipPropertyBFLResponse]
  ): Unit =
    when(mockHipConnector.getPropertyBroughtForwardLoss(eqTo(nino), eqTo(lossId))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))

  def mockHipUpdatePropertyBroughtForwardLossSubmission(
    nino: Nino,
    broughtForwardLossAmount: BigDecimal,
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
    lossId: BroughtForwardLossId,
    result: Either[ApiError, HipPropertyBFLResponse]
  ): Unit =
    when(mockHipConnector.updatePropertyBroughtForwardLoss(eqTo(nino), eqTo(broughtForwardLossAmount), eqTo(taxYearBroughtForwardFrom), eqTo(lossId))(any[HeaderCarrier]))
      .thenReturn(Future.successful(result))
}
