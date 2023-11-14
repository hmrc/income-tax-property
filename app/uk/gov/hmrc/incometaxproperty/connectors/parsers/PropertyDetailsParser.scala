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

package uk.gov.hmrc.incometaxproperty.connectors.parsers

import uk.gov.hmrc.incometaxproperty.models.errors.ApiError
import play.api.Logging
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR, NOT_FOUND, OK, SERVICE_UNAVAILABLE, UNPROCESSABLE_ENTITY}
import uk.gov.hmrc.http.{HttpReads, HttpResponse}
import uk.gov.hmrc.incometaxproperty.connectors.Parser
import uk.gov.hmrc.incometaxproperty.models.responses.PropertyDetailsModel

case class PropertyDetailsParser(httpResponse: HttpResponse, result: Either[ApiError, Option[PropertyDetailsModel]])

object PropertyDetailsParser extends Logging {

  implicit val getBusinessDetailsResponseReads: HttpReads[PropertyDetailsParser] = new HttpReads[PropertyDetailsParser] with Parser {

    override val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): PropertyDetailsParser = response.status match {
      case OK => PropertyDetailsParser(response, extractResult(response))
      case NOT_FOUND => PropertyDetailsParser(response, Right(None))
      case INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE | BAD_REQUEST | UNPROCESSABLE_ENTITY =>
        PropertyDetailsParser(response, handleError(response, response.status))
      case _ => PropertyDetailsParser(response, handleError(response, INTERNAL_SERVER_ERROR))
    }

    private def extractResult(response: HttpResponse): Either[ApiError, Option[PropertyDetailsModel]] = {
      val json = response.json
      json.validate[PropertyDetailsModel]
        .fold[Either[ApiError, Option[PropertyDetailsModel]]](_ => badSuccessJsonResponse, parsedModel => Right(Some(parsedModel)))
    }
  }
}