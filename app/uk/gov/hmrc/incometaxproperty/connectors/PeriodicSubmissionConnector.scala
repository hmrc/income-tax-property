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

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient}
import uk.gov.hmrc.incometaxproperty.config.AppConfig
import uk.gov.hmrc.incometaxproperty.connectors.error.{ApiError, SingleErrorBody}
import uk.gov.hmrc.incometaxproperty.connectors.response.GetPeriodicSubmissionDataResponse
import uk.gov.hmrc.incometaxproperty.models.propertyperiodicsubmission.response.PropertyPeriodicSubmissionResponse

import java.net.URL
import javax.inject.Inject
import scala.concurrent.Future

class PeriodicSubmissionConnector @Inject()(httpClient: HttpClient, appConf: AppConfig)
                                           (implicit ec: ExecutionContext) {

  val apiVersion = "1649"

  def getPeriodicSubmission(taxYear: String, taxableEntityId: String, incomeSourceId: String)
                        (implicit hc: HeaderCarrier): Future[Either[ApiError, PropertyPeriodicSubmissionResponse]] = {
    val url = new URL(s"/income-tax/business/property/$taxYear/$taxableEntityId/$incomeSourceId/period")
    val apiResponse =  httpClient.GET[GetPeriodicSubmissionDataResponse](url)
    val dataResponse = apiResponse.map { response =>
      if (response.result.isLeft) {
      Left(ApiError(response.httpResponse.status, SingleErrorBody(response.getClass.getSimpleName, response.httpResponse.body)))
      //pagerDutyLoggerService.pagerDutyLog(response.httpResponse, response.getClass.getSimpleName)
      } else {
        response.result
      }
    }
    dataResponse
  }
}
