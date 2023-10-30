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

package uk.gov.hmrc.incometaxproperty.services

import play.api.http.Status.INTERNAL_SERVER_ERROR
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import uk.gov.hmrc.incometaxproperty.config.AppConfig
import uk.gov.hmrc.incometaxproperty.models.BusinessDetails
import uk.gov.hmrc.incometaxproperty.utils.{AppConfigStub, UnitTest}
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiError, ApiServiceError, SingleErrorBody}
import uk.gov.hmrc.incometaxproperty.utils.mocks.MockIntegrationFrameworkConnector
import scala.concurrent.ExecutionContext.Implicits.global

import java.time.LocalDate

class IntegrationFrameworkServiceSpec extends UnitTest
  with MockIntegrationFrameworkConnector
  with HttpClientSupport
{
  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  private val apiError = ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody.parsingError)
  lazy val appConfigStub: AppConfig = new AppConfigStub().config()

  private val underTest = new IntegrationFrameworkService(mockIntegrationFrameworkConnector)

  ".GetBusinessDetails" should {
    "return error when GetBusinessDetails fails" in {
      mockGetBusinessDetails( "some-nino", Left(apiError))
      await(underTest.getBusinessDetails("some-nino")) shouldBe Left(ApiServiceError(apiError.status.toString))
    }

    "return BusinessDetails when GetBusinessDetails succeeds" in {
      val businessDetails = BusinessDetails(LocalDate.now(), true)
      mockGetBusinessDetails("some-nino", Right(Some(businessDetails)))

      await(underTest.getBusinessDetails("some-nino")) shouldBe Right(Some(businessDetails))
    }
  }
}
