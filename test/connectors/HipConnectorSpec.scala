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

package connectors


import models.IncomeSourceType
import models.IncomeSourceType.UKPropertyFHL
import models.common.TaxYear.asTys
import models.common.{IncomeSourceId, Nino}
import models.request.WhenYouReportedTheLoss.{toTaxYear, y2021to2022}
import models.request.{HipPropertyBFLRequest, WhenYouReportedTheLoss}
import models.responses.BroughtForwardLossId
import org.scalamock.scalatest.MockFactory
import play.api.http.Status.OK
import play.api.libs.json.Json
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId}
import utils.TaxYearUtils.taxYear

import scala.concurrent.ExecutionContext.Implicits.global

class HipConnectorSpec extends ConnectorIntegrationSpec with MockFactory {

  private val nino = Nino("test-nino")
  private val incomeSourceId = IncomeSourceId("test-income-source-id")
  private val incomeSourceType: IncomeSourceType = UKPropertyFHL
  private val lossAmount: BigDecimal = BigDecimal(100.01)
  private val broughtForwardLossTaxYear: WhenYouReportedTheLoss = y2021to2022

  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))

  private val underTest = new HipConnector(httpClientV2, appConfigStub)

  ".createPropertyBroughtForwardLoss" should {
    "return the Loss ID" when {
      "successfully created Property BF Loss" in {
        val requestBody = Json.toJson(Requests.validCreateBFLRequest).toString()
        val taxYear: String = asTys(toTaxYear(broughtForwardLossTaxYear))

        val response = BroughtForwardLossId(lossId="test-loss-id")
        val httpResponse: HttpResponse = HttpResponse(OK, Json.toJson(response).toString())

        stubPostHttpClientCall(
          s"${appConfigStub.hipBaseUrl}/income-sources/brought-forward-losses/$nino?taxYear=$taxYear",
          requestBody,
          httpResponse
        )

        await(
          underTest.createPropertyBroughtForwardLoss(
            nino,
            incomeSourceId,
            incomeSourceType,
            lossAmount,
            broughtForwardLossTaxYear
          )(hc)
        ) shouldBe Right()
      }
    }
    "return API error" when {
      "failed to successfully create Property BF Loss" in {
        ???
      }
    }
  }

  object Requests {
    val validCreateBFLRequest: HipPropertyBFLRequest = HipPropertyBFLRequest(
      incomeSourceId = incomeSourceId,
      incomeSourceType = incomeSourceType,
      broughtForwardLossAmount = lossAmount,
      taxYearBroughtForwardFrom = taxYear
    )
  }
  object Responses{
  }
}
