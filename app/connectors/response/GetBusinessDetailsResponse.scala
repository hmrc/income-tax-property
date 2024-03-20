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

package connectors.response

import connectors.Parser
import models.errors.ApiError
import models.responses.IncomeSourceDetailsModel
import play.api.Logging
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR, NOT_FOUND, OK, SERVICE_UNAVAILABLE}
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class GetBusinessDetailsResponse(httpResponse: HttpResponse, result: Either[ApiError, Option[IncomeSourceDetailsModel]])

object GetBusinessDetailsResponse extends Logging {

  implicit val getBusinessDetailsResponseReads: HttpReads[GetBusinessDetailsResponse] = new HttpReads[GetBusinessDetailsResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): GetBusinessDetailsResponse = response.status match {
      case OK => GetBusinessDetailsResponse(response, extractResult(response))
      case NOT_FOUND => GetBusinessDetailsResponse(response, Right(None))
      case INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE | BAD_REQUEST =>
        GetBusinessDetailsResponse(response, handleError(response, response.status))
      case _ => GetBusinessDetailsResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }

    private def extractResult(response: HttpResponse): Either[ApiError, Option[IncomeSourceDetailsModel]] = {
      val json = response.json
      logger.error("GetBusinessDetailsResponse: " + json.toString())
      json.validate[IncomeSourceDetailsModel]
        .fold[Either[ApiError, Option[IncomeSourceDetailsModel]]](_ => badSuccessJsonResponse, parsedModel => Right(Some(parsedModel)))
    }
  }
}
