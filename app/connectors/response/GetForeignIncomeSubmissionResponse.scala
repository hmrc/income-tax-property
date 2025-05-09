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
import models.request.foreignincome.ForeignIncomeSubmission
import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class GetForeignIncomeSubmissionResponse(
  httpResponse: HttpResponse,
  result: Either[ApiError, Option[ForeignIncomeSubmission]]
)

object GetForeignIncomeSubmissionResponse {

  implicit val getForeignIncomeSubmissionDataReads: HttpReads[GetForeignIncomeSubmissionResponse] =
    new HttpReads[GetForeignIncomeSubmissionResponse] with Parser {

      override protected[connectors] val parserName: String = this.getClass.getSimpleName

      override def read(method: String, url: String, response: HttpResponse): GetForeignIncomeSubmissionResponse =
        response.status match {
          case OK        => GetForeignIncomeSubmissionResponse(response, extractResult(response))
          case NOT_FOUND => GetForeignIncomeSubmissionResponse(response, Right(None))
          case INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE | BAD_REQUEST | UNPROCESSABLE_ENTITY | BAD_GATEWAY =>
            GetForeignIncomeSubmissionResponse(response, handleError(response, response.status))
          case _ => GetForeignIncomeSubmissionResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
        }

      private def extractResult(response: HttpResponse): Either[ApiError, Option[ForeignIncomeSubmission]] =
        response.json
          .validate[ForeignIncomeSubmission]
          .fold[Either[ApiError, Option[ForeignIncomeSubmission]]](
            _ => badSuccessJsonResponse,
            (parsedModel: ForeignIncomeSubmission) => parsedModel match {
              case ForeignIncomeSubmission(None, None, None, None, None, None) => badSuccessJsonResponse
              case _ => Right(Some(parsedModel))
            }
          )
    }
}