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
import uk.gov.hmrc.incometaxproperty.models.responses.PropertyAnnualSubmission

case class DeletePropertyAnnualSubmissionResponse(httpResponse: HttpResponse, result: Either[ApiError, Unit])

object DeletePropertyAnnualSubmissionResponse {

  implicit val deletePropertyAnnualSubmissionDataReads: HttpReads[DeletePropertyAnnualSubmissionResponse] =
    new HttpReads[DeletePropertyAnnualSubmissionResponse] with Parser {

      override protected[connectors] val parserName: String = this.getClass.getSimpleName

      override def read(method: String, url: String, response: HttpResponse): DeletePropertyAnnualSubmissionResponse = response.status match {
        case NO_CONTENT => DeletePropertyAnnualSubmissionResponse(response, Right(None))
        case NOT_FOUND => DeletePropertyAnnualSubmissionResponse(response, handleError(response, NOT_FOUND))
        case BAD_REQUEST | UNPROCESSABLE_ENTITY | INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE =>
          DeletePropertyAnnualSubmissionResponse(response, handleError(response, response.status))
        case _ => DeletePropertyAnnualSubmissionResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
      }
    }
}
