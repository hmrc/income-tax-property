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
import uk.gov.hmrc.incometaxproperty.models.{PeriodicSubmission, PeriodicSubmissionResponse}
import uk.gov.hmrc.incometaxproperty.utils.ControllerUnitTest
import uk.gov.hmrc.incometaxproperty.utils.mocks.{MockAuthorisedAction, MockPropertyService}
import uk.gov.hmrc.incometaxproperty.utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global

class PeriodicSubmissionControllerSpec extends ControllerUnitTest
  with MockPropertyService
  with MockAuthorisedAction
  with FakeRequestProvider {

  private val underTest = new PeriodicSubmissionController(
    mockPropertyServices,
    mockAuthorisedAction,
    cc
  )

  ".getPeriodicSubmissionData" should {


    val periodicSubmissionResponse = PeriodicSubmissionResponse(List(PeriodicSubmission("submissionId", "2022-11-11", "2023-12-22")))

    "return aPeriodicSubmission when IntegrationFrameworkService returns Right(aPeriodicSubmission)" in {
      mockAuthorisation()
      mockGetAllPeriodicSubmission(2024,
        "taxableEntityId",
        "incomeSourceId",
        Right(periodicSubmissionResponse))

      val result = await(underTest.getAllPeriodicSubmission(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest))

      result.header.status shouldBe OK
      Json.parse(consumeBody(result)) shouldBe Json.toJson(periodicSubmissionResponse)
    }

    "return not found when PeriodicSubmissionService returns Left(DataNotFoundError)" in {
      mockAuthorisation()
      mockGetAllPeriodicSubmission(2024,
        "taxableEntityId",
        "incomeSourceId",
        Left(DataNotFoundError))

      val result = underTest.getAllPeriodicSubmission(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest)

      status(result) shouldBe NOT_FOUND
    }

    "return internal server error when PeriodicSubmissionService returns Left(ApiServiceError)" in {
      mockAuthorisation()
      mockGetAllPeriodicSubmission(2024,
        "taxableEntityId",
        "incomeSourceId",
        Left(ApiServiceError("error")))

      val result = underTest.getAllPeriodicSubmission(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest)

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }
  }
}

