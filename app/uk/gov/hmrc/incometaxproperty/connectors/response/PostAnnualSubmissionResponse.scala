/*
 * Copyright 2024 HM Revenue & Customs
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

import play.api.Logging
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR, NOT_FOUND, NO_CONTENT, SERVICE_UNAVAILABLE, UNPROCESSABLE_ENTITY}
import uk.gov.hmrc.http.{HttpReads, HttpResponse}
import uk.gov.hmrc.incometaxproperty.connectors.Parser
import uk.gov.hmrc.incometaxproperty.models.errors.ApiError

case class PostAnnualSubmissionResponse(httpResponse: HttpResponse, result: Either[ApiError, Unit])

object PostAnnualSubmissionResponse extends Logging {

  implicit val postPeriodicSubmission: HttpReads[PostAnnualSubmissionResponse] = new HttpReads[PostAnnualSubmissionResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): PostAnnualSubmissionResponse = response.status match {
      case NO_CONTENT => PostAnnualSubmissionResponse(response, Right(None))
      case NOT_FOUND => PostAnnualSubmissionResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST | UNPROCESSABLE_ENTITY | INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE =>
        PostAnnualSubmissionResponse(response, handleError(response, response.status))
      case _ => PostAnnualSubmissionResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }
  }
}
