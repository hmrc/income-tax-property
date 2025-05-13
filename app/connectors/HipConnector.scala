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

import config.AppConfig
import connectors.response.PostBroughtForwardLossResponse
import models.IncomeSourceType
import models.common.TaxYear.asTys
import models.common.{IncomeSourceId, Nino}
import models.errors.ApiError
import models.request.WhenYouReportedTheLoss.toTaxYear
import models.request.{HipPropertyBFLRequest, WhenYouReportedTheLoss}
import models.responses.BroughtForwardLossId
import play.api.libs.json.Json
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HeaderNames, StringContextOps}
import utils.Logging

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class HipConnector @Inject() (
  http: HttpClientV2,
  appConfig: AppConfig
)(implicit ec: ExecutionContext)
    extends Logging {

  def createPropertyBroughtForwardLoss(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    incomeSourceType: IncomeSourceType,
    lossAmount: BigDecimal,
    taxYearBroughtForwardFrom: WhenYouReportedTheLoss
  )(implicit hc: HeaderCarrier): Future[Either[ApiError, BroughtForwardLossId]] = {
    val hipApiVersion: String = "1500"
    val taxYear: String = asTys(toTaxYear(taxYearBroughtForwardFrom)) // Format: yy-yy
    val url = s"${appConfig.hipBaseUrl}/income-sources/brought-forward-losses/$nino?taxYear=$taxYear"

    val requestBody: HipPropertyBFLRequest = HipPropertyBFLRequest(
      incomeSourceId = incomeSourceId,
      incomeSourceType = incomeSourceType,
      broughtForwardLossAmount = lossAmount,
      taxYearBroughtForwardFrom = toTaxYear(taxYearBroughtForwardFrom).endYear
    )

    logger.debug(s"[HipConnector] Calling createPropertyBroughtForwardLoss with url: $url, body: ${Json.toJson(requestBody)}")

    http
      .post(url"$url")
      .setHeader("Environment" -> appConfig.hipEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.hipAuthTokenFor(hipApiVersion)}") // TODO - Needed??
      .withBody(Json.toJson(requestBody))
      .execute[PostBroughtForwardLossResponse]
      .map { response: PostBroughtForwardLossResponse =>
        if (response.result.isLeft) {
          val correlationId =
            response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
          logger.error(
            s"[HipConnector] Error creating a brought forward loss from the HIP Integration Framework: URL: $url" +
              s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}"
          )
        }
        response.result
      }
  }

}
