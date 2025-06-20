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
import connectors.response.{GetHipPropertyBFLResponse, PostBroughtForwardLossResponse, PutHipPropertyBFLResponse}
import models.IncomeSourceType
import models.common.TaxYear.asTys
import models.common.{IncomeSourceId, Nino}
import models.errors.ApiError
import models.request.WhenYouReportedTheLoss.toTaxYear
import models.request.{HipPropertyBFLRequest, HipPropertyUpdateBFLRequest, WhenYouReportedTheLoss}
import models.responses.{BroughtForwardLossId, HipPropertyBFLResponse}
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json.Json
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HeaderNames, StringContextOps}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class HipConnector @Inject() (
  http: HttpClientV2,
  appConfig: AppConfig
)(implicit ec: ExecutionContext) {
  lazy val logger: Logger = LoggerFactory.getLogger("hip-connector")

  // HIP API#1500
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

    val requestBody = HipPropertyBFLRequest(
      incomeSourceId = incomeSourceId,
      incomeSourceType = incomeSourceType,
      broughtForwardLossAmount = lossAmount,
      taxYearBroughtForwardFrom = toTaxYear(taxYearBroughtForwardFrom).endYear
    )

    logger.debug(s"[HipConnector] Calling createPropertyBroughtForwardLoss with url: $url, body: ${Json.toJson(requestBody)}")

    http
      .post(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.hipEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.hipAuthTokenFor(hipApiVersion)}")
      .withBody[HipPropertyBFLRequest](requestBody)
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

  // HIP API#1501
  def updatePropertyBroughtForwardLoss(
                                        nino: Nino,
                                        broughtForwardLossAmount: BigDecimal,
                                        taxYearBroughtForwardFrom: WhenYouReportedTheLoss,
                                        lossID: BroughtForwardLossId
                                      )(implicit hc: HeaderCarrier): Future[Either[ApiError, HipPropertyBFLResponse]] = {
    val hipApiVersion: String = "1501"
    val taxYear: String = asTys(toTaxYear(taxYearBroughtForwardFrom)) // Format: yy-yy
    val url = s"${appConfig.hipBaseUrl}/income-sources/brought-forward-losses/$nino/${lossID.lossId}?taxYear=$taxYear"

    val requestBody = HipPropertyUpdateBFLRequest(
      updatedBroughtForwardLossAmount = broughtForwardLossAmount
    )

    logger.debug(s"[HipConnector] Calling updatePropertyBroughtForwardLoss with url: $url, body: ${Json.toJson(requestBody)}")

    http
      .put(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.hipEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.hipAuthTokenFor(hipApiVersion)}")
      .withBody[HipPropertyUpdateBFLRequest](requestBody)
      .execute[PutHipPropertyBFLResponse]
      .map { response: PutHipPropertyBFLResponse =>
        if (response.result.isLeft) {
          val correlationId =
            response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
          logger.error(
            s"[HipConnector] Error updating a brought forward loss from the HIP Integration Framework: URL: $url" +
              s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}"
          )
        }
        response.result
      }
  }

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
