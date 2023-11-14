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

import uk.gov.hmrc.incometaxproperty.models.errors.{ApiError, ApiServiceError, ParsingError, ServiceError}
import play.api.Logging
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR, NOT_FOUND, OK, SERVICE_UNAVAILABLE, UNPROCESSABLE_ENTITY}
import play.api.libs.json.{JsPath, JsonValidationError}
import uk.gov.hmrc.http.{HttpReads, HttpResponse}
import uk.gov.hmrc.incometaxproperty.models.responses.{ErrorBodyModel, ErrorsBodyModel, IncomeSourceDetailsModel, PropertyDetailsModel}


object PropertyDetailsParser extends Logging {

  type GetIncomeSourceDetailResponse = Either[ServiceError, IncomeSourceDetailsModel]

  implicit object getProperDetailsResponseReads extends HttpReads[GetIncomeSourceDetailResponse] {

     val parserName: String = this.getClass.getSimpleName

     def read(method: String, url: String, response: HttpResponse): GetIncomeSourceDetailResponse = response.status match {
      case OK => response.json.validate[IncomeSourceDetailsModel](IncomeSourceDetailsModel.format).fold[GetIncomeSourceDetailResponse](
        validationErrors => badSuccessJsonFromAPI(validationErrors),
        parsedModel => Right(parsedModel)
      )
      case NOT_FOUND => handleIFError(response)
      case INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE | BAD_REQUEST | UNPROCESSABLE_ENTITY => handleIFError(response)
      case _ => handleIFError(response)
    }
  }

  def badSuccessJsonFromAPI[Response](validationErrors: collection.Seq[(JsPath, collection.Seq[JsonValidationError])] ): Either[ServiceError, Response] = {
    Left(ParsingError(s"$validationErrors"))
  }

  def handleIFError[Response](response: HttpResponse): Either[ServiceError, Response] = {


    try {
      val json = response.json

      lazy val apiError = json.asOpt[ErrorBodyModel]
      lazy val apiErrors = json.asOpt[ErrorsBodyModel]

      (apiError, apiErrors) match {
        case (Some(apiError), _) => Left(ApiServiceError(apiError.reason))
        case (_, Some(apiErrors)) => Left(ApiServiceError(apiErrors.failures.toString()))
        case _ =>
          Left(ApiServiceError(""))
      }
    } catch {
      case _:
        Exception =>
        Left(ParsingError(""))
    }
  }

}