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

package uk.gov.hmrc.incometaxproperty.connectors

import uk.gov.hmrc.http.{HeaderCarrier, HttpClient}
import uk.gov.hmrc.incometaxproperty.config.AppConfig
import uk.gov.hmrc.incometaxproperty.connectors.response.GetBusinessDetailsResponse
import uk.gov.hmrc.incometaxproperty.connectors.response.GetBusinessDetailsResponse.getBusinessDetailsResponseReads
import uk.gov.hmrc.incometaxproperty.models.errors.ApiError
import uk.gov.hmrc.incometaxproperty.models.responses.IncomeSourceDetailsModel

import java.net.URL
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class IntegrationFrameworkConnector @Inject()(httpClient: HttpClient, appConf: AppConfig)
                                             (implicit ec: ExecutionContext) extends IFConnector {


  override protected[connectors] val appConfig: AppConfig = appConf

  def getBusinessDetails(nino: String)
                        (implicit hc: HeaderCarrier): Future[Either[ApiError, Option[IncomeSourceDetailsModel]]] = {
    val url = new URL(s"${appConfig.ifBaseUrl}/registration/business-details/nino/$nino")
    val apiVersion = "1171"

    callGetBusinessDetails(url)(ifHeaderCarrier(url, apiVersion)).map(_.result)
  }

  private def callGetBusinessDetails(url: URL)(implicit hc: HeaderCarrier): Future[GetBusinessDetailsResponse] = {
    httpClient.GET[GetBusinessDetailsResponse](url)
  }
}
