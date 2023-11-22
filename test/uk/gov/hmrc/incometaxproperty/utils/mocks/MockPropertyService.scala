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

package uk.gov.hmrc.incometaxproperty.utils.mocks

import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.incometaxproperty.models.PeriodicSubmissionResponse
import uk.gov.hmrc.incometaxproperty.models.errors.ServiceError
import uk.gov.hmrc.incometaxproperty.services.PropertyServices

import scala.concurrent.Future

trait MockPropertyService extends MockFactory {

  protected val mockPropertyServices: PropertyServices = mock[PropertyServices]

  def mockGetPeriodicSubmission(taxYear: String,
                                taxableEntityId: String,
                                incomeSourceId: String,
                                result: Either[ServiceError, PeriodicSubmissionResponse]
                            ): CallHandler4[String, String, String, HeaderCarrier, Future[Either[ServiceError, PeriodicSubmissionResponse]]] = {
    (mockPropertyServices.getPeriodicSubmission(_: String, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(Future.successful(result))
  }
}
