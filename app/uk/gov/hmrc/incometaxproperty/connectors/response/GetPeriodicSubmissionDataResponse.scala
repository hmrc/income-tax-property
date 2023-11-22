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

package uk.gov.hmrc.incometaxproperty.connectors.response

import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}
import uk.gov.hmrc.incometaxproperty.connectors.Parser
import uk.gov.hmrc.incometaxproperty.models.errors.ApiError
import uk.gov.hmrc.incometaxproperty.models.responses.PeriodicSubmissionModel

case class GetPeriodicSubmissionDataResponse(httpResponse: HttpResponse, result: Either[ApiError, PeriodicSubmissionModel])

object GetPeriodicSubmissionDataResponse {

  implicit val getPeriodicSubmissionDataReads: HttpReads[GetPeriodicSubmissionDataResponse] =
    new HttpReads[GetPeriodicSubmissionDataResponse] with Parser {

      override protected[connectors] val parserName: String = this.getClass.getSimpleName

      override def read(method: String, url: String, response: HttpResponse): GetPeriodicSubmissionDataResponse = response.status match {
        case OK => GetPeriodicSubmissionDataResponse(response, extractResult(response))
        case NOT_FOUND => GetPeriodicSubmissionDataResponse(response, Right(PeriodicSubmissionModel(List.empty)))
        case INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE | BAD_REQUEST | UNPROCESSABLE_ENTITY =>
          GetPeriodicSubmissionDataResponse(response, handleError(response, response.status))
        case _ => GetPeriodicSubmissionDataResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
      }

      private def extractResult(response: HttpResponse): Either[ApiError, PeriodicSubmissionModel] = {
        response.json.validate[PeriodicSubmissionModel]
          .fold[Either[ApiError, PeriodicSubmissionModel]](_ => badSuccessJsonResponse, parsedModel => Right(parsedModel))
      }
    }
}
