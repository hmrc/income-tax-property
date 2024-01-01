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

package uk.gov.hmrc.incometaxproperty.controllers

import play.api.http.Status.{INTERNAL_SERVER_ERROR, NOT_FOUND, OK}
import play.api.libs.json.Json
import play.api.test.Helpers.status
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiServiceError, DataNotFoundError}
import uk.gov.hmrc.incometaxproperty.models.{BusinessDetailsResponse, PropertyDetails}
import uk.gov.hmrc.incometaxproperty.utils.ControllerUnitTest
import uk.gov.hmrc.incometaxproperty.utils.mocks.{MockAuthorisedAction, MockIntegrationFrameworkService}
import uk.gov.hmrc.incometaxproperty.utils.providers.FakeRequestProvider

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class BusinessDetailsControllerSpec extends ControllerUnitTest
  with MockIntegrationFrameworkService
  with MockAuthorisedAction
  with FakeRequestProvider {

  private val underTest = new BusinessDetailsController(
    mockIntegrationFrameworkService,
    mockAuthorisedAction,
    cc
  )

  ".getBusinessDetailsData" should {

    val businessDetailsResponse = BusinessDetailsResponse(List(PropertyDetails(Some("income-source"), Some(LocalDate.now), Some(true))))

    "return aBusinessDetails when IntegrationFrameworkService returns Right(aBusinessDetails)" in {
      mockAuthorisation()
      mockGetBusinessDetails("some-nino", Right(businessDetailsResponse))

      val result = await(underTest.getBusinessDetails("some-nino")(fakeGetRequest))

      result.header.status shouldBe OK
      Json.parse(consumeBody(result)) shouldBe Json.toJson(businessDetailsResponse)
    }

    "return not found when businessDetailsService returns Left(DataNotFoundError)" in {
      mockAuthorisation()
      mockGetBusinessDetails("some-nino", Left(DataNotFoundError))

      val result = underTest.getBusinessDetails("some-nino")(fakeGetRequest)

      status(result) shouldBe NOT_FOUND
    }

    "return error when businessDetailsService returns Left(ApiServiceError)" in {
      mockAuthorisation()
      mockGetBusinessDetails("some-nino", Left(ApiServiceError(500)))

      val result = underTest.getBusinessDetails("some-nino")(fakeGetRequest)

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }
  }
}
