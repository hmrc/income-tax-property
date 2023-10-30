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

import org.scalamock.handlers.{CallHandler1, CallHandler2, CallHandler3}
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.incometaxproperty.connectors.IntegrationFrameworkConnector
import uk.gov.hmrc.incometaxproperty.models.BusinessDetails
import uk.gov.hmrc.incometaxproperty.models.errors.ApiError

import scala.concurrent.Future

trait MockIntegrationFrameworkConnector extends MockFactory {

  protected val mockIntegrationFrameworkConnector: IntegrationFrameworkConnector = mock[IntegrationFrameworkConnector]

  def mockGetBusinessDetails(nino: String,
                                  result: Either[ApiError, Option[BusinessDetails]])
  : CallHandler2[String, HeaderCarrier, Future[Either[ApiError, Option[BusinessDetails]]]] = {
    (mockIntegrationFrameworkConnector.getBusinessDetails(_: String)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(Future.successful(result))
  }
}
