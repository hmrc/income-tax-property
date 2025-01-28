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
import models.errors.ApiError
import play.api.http.Status._
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class BroughtForwardLossId(lossId: String)

object BroughtForwardLossId {
  implicit val format: Format[BroughtForwardLossId] = Json.format[BroughtForwardLossId]
}

case class CreateBroughtForwardLossResponse(httpResponse: HttpResponse, result: Either[ApiError, BroughtForwardLossId])


object CreateBroughtForwardLossResponse {

  implicit val createBroughtForwardLossResponse: HttpReads[CreateBroughtForwardLossResponse] = new HttpReads[CreateBroughtForwardLossResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): CreateBroughtForwardLossResponse = response.status match {
      case OK =>
        val result = response.json.validate[BroughtForwardLossId].fold[Either[ApiError, BroughtForwardLossId]](
          _ => badSuccessJsonResponse, parsedModel => Right(parsedModel)
        )
        CreateBroughtForwardLossResponse(httpResponse = response, result = result)
      case NOT_FOUND => CreateBroughtForwardLossResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST => CreateBroughtForwardLossResponse(response, handleError(response, response.status))
      case _ => CreateBroughtForwardLossResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }
  }
}
