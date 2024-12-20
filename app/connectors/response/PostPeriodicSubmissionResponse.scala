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
import models.responses.PeriodicSubmissionId
import play.api.Logging
import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class PostPeriodicSubmissionResponse(httpResponse: HttpResponse, result: Either[ApiError, Option[PeriodicSubmissionId]])

object PostPeriodicSubmissionResponse extends Logging {

  implicit val postPeriodicSubmission: HttpReads[PostPeriodicSubmissionResponse] = new HttpReads[PostPeriodicSubmissionResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): PostPeriodicSubmissionResponse = response.status match {
      case CREATED => PostPeriodicSubmissionResponse(response, extractResult(response))
      case NOT_FOUND => PostPeriodicSubmissionResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST | CONFLICT | INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE =>
        PostPeriodicSubmissionResponse(response, handleError(response, response.status))
      case _ => PostPeriodicSubmissionResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }

    private def extractResult(response: HttpResponse): Either[ApiError, Option[PeriodicSubmissionId]] = {
      val json = response.json
      logger.info("PostPeriodicSubmissionResponse: " + json.toString())
      json.validate[PeriodicSubmissionId]
        .fold[Either[ApiError, Option[PeriodicSubmissionId]]](_ => badSuccessJsonResponse, parsedModel => Right(Some(parsedModel)))
    }
  }
}
