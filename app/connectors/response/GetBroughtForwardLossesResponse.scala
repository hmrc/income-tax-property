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
import models.responses.BroughtForwardLosses
import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class GetBroughtForwardLossesResponse(httpResponse: HttpResponse, result: Either[ApiError, BroughtForwardLosses])

object GetBroughtForwardLossesResponse {
  implicit val getBroughtForwardLosses: HttpReads[GetBroughtForwardLossesResponse] = new HttpReads[GetBroughtForwardLossesResponse] with Parser {
    override protected val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): GetBroughtForwardLossesResponse = response.status match {
      case OK => GetBroughtForwardLossesResponse(response, extractResult(response))
      case NOT_FOUND => GetBroughtForwardLossesResponse(response, Right(BroughtForwardLosses(Seq.empty)))
      case BAD_REQUEST | UNPROCESSABLE_ENTITY | INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE =>
        GetBroughtForwardLossesResponse(response, handleError(response, response.status))
      case _ => GetBroughtForwardLossesResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }

    private def extractResult(response: HttpResponse): Either[ApiError, BroughtForwardLosses] = {
      val json = response.json
      json.validate[BroughtForwardLosses]
        .fold[Either[ApiError, BroughtForwardLosses]](_ => badSuccessJsonResponse, parsedModel => Right(parsedModel))
    }

  }
}