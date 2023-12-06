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
import uk.gov.hmrc.incometaxproperty.models.PropertyPeriodicSubmissionResponse
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiServiceError, DataNotFoundError}
import uk.gov.hmrc.incometaxproperty.models.responses.{PeriodicSubmissionIdModel, PropertyPeriodicSubmission}
import uk.gov.hmrc.incometaxproperty.utils.ControllerUnitTest
import uk.gov.hmrc.incometaxproperty.utils.mocks.{MockAuthorisedAction, MockPropertyService}
import uk.gov.hmrc.incometaxproperty.utils.providers.FakeRequestProvider

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class PeriodicSubmissionControllerSpec extends ControllerUnitTest
  with MockPropertyService
  with MockAuthorisedAction
  with FakeRequestProvider {

  private val underTest = new PeriodicSubmissionController(
    mockPropertyService,
    mockAuthorisedAction,
    cc
  )

  ".getPeriodicSubmissionData" should {

    val periodicSubmissionResponse = PropertyPeriodicSubmissionResponse(List(
      PropertyPeriodicSubmission(
        submittedOn = Some(LocalDateTime.now),
        fromDate = LocalDate.now.minusDays(1),
        toDate = LocalDate.now,
        None, None, None, None
      )
    ))
    List(PeriodicSubmissionIdModel("submissionId", LocalDate.parse("2022-11-11"), LocalDate.parse("2023-12-22")))

    "return a property periodic submission when IntegrationFrameworkService returns Right(aPeriodicSubmission)" in {
      mockAuthorisation()
      mockGetAllPeriodicSubmissions(2024,
        "taxableEntityId",
        "incomeSourceId",
        Right(periodicSubmissionResponse))

      val result = await(underTest.getAllPeriodicSubmissions(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest))

      result.header.status shouldBe OK
      Json.parse(consumeBody(result)) shouldBe Json.toJson(periodicSubmissionResponse)
    }

    "return not found when PeriodicSubmissionService returns Left(DataNotFoundError)" in {
      mockAuthorisation()
      mockGetAllPeriodicSubmissions(2024,
        "taxableEntityId",
        "incomeSourceId",
        Left(DataNotFoundError))

      val result = underTest.getAllPeriodicSubmissions(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest)

      status(result) shouldBe NOT_FOUND
    }

    "return internal server error when PeriodicSubmissionService returns Left(ApiServiceError)" in {
      mockAuthorisation()
      mockGetAllPeriodicSubmissions(2024,
        "taxableEntityId",
        "incomeSourceId",
        Left(ApiServiceError("error")))

      val result = underTest.getAllPeriodicSubmissions(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest)

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }
  }
}

