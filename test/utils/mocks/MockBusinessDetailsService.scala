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

package utils.mocks

import models.BusinessDetailsResponse
import models.errors.ServiceError
import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import services.BusinessDetailsService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockBusinessDetailsService extends MockFactory {

  protected val mockIntegrationFrameworkService: BusinessDetailsService = mock[BusinessDetailsService]

  def mockGetBusinessDetails(nino: String,
                             result: Either[ServiceError, BusinessDetailsResponse]
                            ): CallHandler2[String, HeaderCarrier, Future[Either[ServiceError, BusinessDetailsResponse]]] = {
    (mockIntegrationFrameworkService.getBusinessDetails(_: String)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(Future.successful(result))
  }

  def mockGetBusinessDetailsException(nino: String,
                                   result: Throwable
                                  ): CallHandler2[String, HeaderCarrier, Future[Either[ServiceError, BusinessDetailsResponse]]] =
    (mockIntegrationFrameworkService.getBusinessDetails(_: String)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(Future.failed(result))
}
