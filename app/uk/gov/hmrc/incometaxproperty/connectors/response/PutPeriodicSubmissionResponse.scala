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

import play.api.Logging
import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}
import uk.gov.hmrc.incometaxproperty.connectors.Parser
import uk.gov.hmrc.incometaxproperty.models.errors.ApiError

case class PutPeriodicSubmissionResponse(httpResponse: HttpResponse, result: Either[ApiError, Option[String]])

object PutPeriodicSubmissionResponse extends Logging {

  implicit val putPeriodicSubmission: HttpReads[PutPeriodicSubmissionResponse] = new HttpReads[PutPeriodicSubmissionResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): PutPeriodicSubmissionResponse = response.status match {
      case NO_CONTENT => PutPeriodicSubmissionResponse(response, Right(None))
      case NOT_FOUND => PutPeriodicSubmissionResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST | UNPROCESSABLE_ENTITY | INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE =>
        PutPeriodicSubmissionResponse(response, handleError(response, response.status))
      case _ => PutPeriodicSubmissionResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }
  }
}
