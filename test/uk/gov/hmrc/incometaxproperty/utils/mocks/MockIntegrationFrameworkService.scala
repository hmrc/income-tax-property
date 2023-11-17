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
import uk.gov.hmrc.incometaxproperty.models.BusinessDetailsResponse
import uk.gov.hmrc.incometaxproperty.models.errors.ServiceError
import uk.gov.hmrc.incometaxproperty.services.IntegrationFrameworkService

import scala.concurrent.Future

trait MockIntegrationFrameworkService extends MockFactory {

  protected val mockIntegrationFrameworkService: IntegrationFrameworkService = mock[IntegrationFrameworkService]

  def mockGetBusinessDetails(nino: String,
                             result: Either[ServiceError, BusinessDetailsResponse]
                            ): CallHandler2[String, HeaderCarrier, Future[Either[ServiceError, BusinessDetailsResponse]]] = {
    (mockIntegrationFrameworkService.getBusinessDetails(_: String)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(Future.successful(result))
  }
}
