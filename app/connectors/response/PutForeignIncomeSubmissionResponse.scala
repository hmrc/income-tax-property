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
import play.api.Logging
import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class PutForeignIncomeSubmissionResponse(httpResponse: HttpResponse, result: Either[ApiError, Unit])

object PutForeignIncomeSubmissionResponse extends Logging {

  implicit val putForeignIncomeSubmissionDataReads: HttpReads[PutForeignIncomeSubmissionResponse] = new HttpReads[PutForeignIncomeSubmissionResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): PutForeignIncomeSubmissionResponse = response.status match {
      case NO_CONTENT => PutForeignIncomeSubmissionResponse(response, Right(None))
      case NOT_FOUND => PutForeignIncomeSubmissionResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST | UNPROCESSABLE_ENTITY | INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE =>
        PutForeignIncomeSubmissionResponse(response, handleError(response, response.status))
      case _ => PutForeignIncomeSubmissionResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }
  }
}
