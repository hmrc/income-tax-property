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

package connectors.response

import connectors.Parser
import models.common.IncomeSourceId
import models.errors.ApiError
import models.request.LossType
import play.api.http.Status._
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

import java.time.LocalDate

case class GetBroughtForwardLossResponse(httpResponse: HttpResponse, result: Either[ApiError, BroughtForwardLossResponse])

case class BroughtForwardLossResponse(
  typeOfLoss: LossType,
  lossAmount: BigDecimal,
  taxYearBroughtForwardFrom: String,
  lastModified: LocalDate
)

object BroughtForwardLossResponse {
  implicit val format: Format[BroughtForwardLossResponse] = Json.format[BroughtForwardLossResponse]
}

object GetBroughtForwardLossResponse {
  implicit val getBroughtForwardLossResponse: HttpReads[GetBroughtForwardLossResponse] = new HttpReads[GetBroughtForwardLossResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName


    override def read(method: String, url: String, response: HttpResponse): GetBroughtForwardLossResponse = response.status match {
      case OK =>
        val result = response.json.validate[BroughtForwardLossResponse].fold[Either[ApiError, BroughtForwardLossResponse]](
          _ => badSuccessJsonResponse, parsedModel => Right(parsedModel)
        )
        GetBroughtForwardLossResponse(response, result)

      case NOT_FOUND => GetBroughtForwardLossResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST => GetBroughtForwardLossResponse(response, handleError(response, response.status))
      case _ => GetBroughtForwardLossResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }
  }
}