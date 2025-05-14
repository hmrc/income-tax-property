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
import connectors.Connector.hcWithCorrelationId
import connectors.response.{GetHipPropertyBFLResponse, PostBroughtForwardLossResponse}
import models.IncomeSourceType
import models.common.TaxYear.asTys
import models.common.{IncomeSourceId, Nino}
import models.errors.ApiError
import models.request.WhenYouReportedTheLoss.toTaxYear
import models.request.{HipPropertyBFLRequest, WhenYouReportedTheLoss}
import models.responses.{BroughtForwardLossId, HipPropertyBFLResponse}
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

  // HIP API#1502
  def getPropertyBroughtForwardLoss(
     nino: Nino,
     lossId: String
   )(implicit hc: HeaderCarrier): Future[Either[ApiError, HipPropertyBFLResponse]] = {
    val apiVersion = "1502"
    val url = s"${appConfig.hipBaseUrl}/income-sources/brought-forward-losses/$nino/$lossId"
    logger.debug(
      s"[HipConnector] Calling getPropertyBroughtForwardLoss with url: $url"
    )

    http
      .get(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.hipEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.hipAuthTokenFor(apiVersion)}")
      .execute[GetHipPropertyBFLResponse]
      .map { response: GetHipPropertyBFLResponse =>
        if (response.result.isLeft) {
          val correlationId =
            response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
          logger.error(
            s"[HipConnector] Error retrieving a brought forward loss from the HIP Integration Framework: URL: $url" +
              s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}"
          )
        }
        response.result
      }
  }

}
