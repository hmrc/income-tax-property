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
import models.responses.{BroughtForwardLossResponse, HipPropertyBFLResponse}
import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class GetHipPropertyBFLResponse(httpResponse: HttpResponse, result: Either[ApiError, HipPropertyBFLResponse])

object GetHipPropertyBFLResponse {
  implicit val getHipPropertyBroughtForwardLoss: HttpReads[GetHipPropertyBFLResponse] = new HttpReads[GetHipPropertyBFLResponse] with Parser {

    override protected val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): GetHipPropertyBFLResponse = response.status match {
      case OK => GetHipPropertyBFLResponse(response, extractResult(response))
      case NOT_FOUND => GetHipPropertyBFLResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST | UNPROCESSABLE_ENTITY | INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE =>
        GetHipPropertyBFLResponse(response, handleError(response, response.status))
      case _ => GetHipPropertyBFLResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }

    private def extractResult(response: HttpResponse): Either[ApiError, HipPropertyBFLResponse] = {
      val json = response.json
      json.validate[HipPropertyBFLResponse]
        .fold[Either[ApiError, HipPropertyBFLResponse]](_ => badSuccessJsonResponse, parsedModel => Right(parsedModel))
    }
  }
}
