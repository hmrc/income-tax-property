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

package controllers

import models.PropertyPeriodicSubmissionResponse
import models.errors.{ApiServiceError, DataNotFoundError}
import models.request.PropertyPeriodicSubmissionRequest
import models.responses._
import play.api.http.Status._
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers.status
import utils.ControllerUnitTest
import utils.mocks.{MockAuthorisedAction, MockPropertyService}
import utils.providers.FakeRequestProvider

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
        None,
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
        Left(ApiServiceError(500)))

      val result = underTest.getAllPeriodicSubmissions(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest)

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }
  }

  "Create Periodic Submission" should {

    val validPropertyPeriodicSubmissionRequest = PropertyPeriodicSubmissionRequest(Some(ForeignFhlEea(ForeignFhlIncome(200.00), ForeignFhlExpenses(None, None, None, None, None, None, None, Some(1000.99)))), None, None, None)
    val fakePostRequestWithBody = fakePostRequest.withJsonBody(Json.toJson(validPropertyPeriodicSubmissionRequest))

    val periodicSubmissionResponse = PeriodicSubmissionId("submissionId")
    "return a property periodic submission when IntegrationFrameworkService returns Right(aPeriodicSubmission)" in {
      mockAuthorisation()
      mockCreatePeriodicSubmissions(
        "taxableEntityId",
        "incomeSourceId",
        2024,
        validPropertyPeriodicSubmissionRequest,
        Right(Some(periodicSubmissionResponse)))

      val result = await(underTest.createPeriodicSubmission("taxableEntityId", "incomeSourceId", 2024)(fakePostRequestWithBody))

      result.header.status shouldBe CREATED
      Json.parse(consumeBody(result)) shouldBe Json.toJson(periodicSubmissionResponse)
    }

    "return conflict when PeriodicSubmissionService returns conflict error" in {
      mockAuthorisation()
      mockCreatePeriodicSubmissions(
        "taxableEntityId",
        "incomeSourceId",
        2024,
        validPropertyPeriodicSubmissionRequest,
        Left(ApiServiceError(409)))

      val result = underTest.createPeriodicSubmission("taxableEntityId", "incomeSourceId", 2024)(fakePostRequestWithBody)

      status(result) shouldBe CONFLICT
    }

    "return internal server error when PeriodicSubmissionService returns Left(ApiServiceError)" in {
      mockAuthorisation()
      mockCreatePeriodicSubmissions(
        "taxableEntityId",
        "incomeSourceId",
        2024,
        validPropertyPeriodicSubmissionRequest,
        Left(ApiServiceError(500)))

      val result = underTest.createPeriodicSubmission("taxableEntityId", "incomeSourceId", 2024)(fakePostRequestWithBody)

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }

    "return bad request error when PeriodicSubmissionService returns Left(ApiServiceError)" in {
      mockAuthorisation()
      mockCreatePeriodicSubmissions(
        "taxableEntityId",
        "incomeSourceId",
        2024,
        validPropertyPeriodicSubmissionRequest,
        Left(ApiServiceError(400)))

      val result = underTest.createPeriodicSubmission("taxableEntityId", "incomeSourceId", 2024)(fakePostRequestWithBody)

      status(result) shouldBe BAD_REQUEST
    }
  }

  "Update Periodic Submission" should {
    val validPropertyPeriodicSubmissionRequest = PropertyPeriodicSubmissionRequest(Some(ForeignFhlEea(ForeignFhlIncome(200.00), ForeignFhlExpenses(None, None, None, None, None, None, None, Some(1000.99)))), None, None, None)
    val fakePutRequestWithBody = fakePutRequest.withJsonBody(Json.toJson(validPropertyPeriodicSubmissionRequest))

    "return a property periodic submission when IntegrationFrameworkService returns no content" in {
      mockAuthorisation()
      mockUpdatePeriodicSubmissions(
        "taxableEntityId",
        "incomeSourceId",
        2024,
        "submissionId",
        validPropertyPeriodicSubmissionRequest,
        Right(""))

      val result = await(underTest.updatePeriodicSubmission("taxableEntityId", "incomeSourceId", 2024, "submissionId")(fakePutRequestWithBody))

      result.header.status shouldBe NO_CONTENT
      consumeBody(result) shouldBe empty
    }

    "return unprocessable-entity when PeriodicSubmissionService returns conflict error" in {
      mockAuthorisation()
      mockUpdatePeriodicSubmissions(
        "taxableEntityId",
        "incomeSourceId",
        2024,
        "submissionId",
        validPropertyPeriodicSubmissionRequest,
        Left(ApiServiceError(422)))

      val result = underTest.updatePeriodicSubmission("taxableEntityId", "incomeSourceId", 2024, "submissionId")(fakePutRequestWithBody)

      status(result) shouldBe UNPROCESSABLE_ENTITY
    }

    "return internal server error when PeriodicSubmissionService returns Left(ApiServiceError)" in {
      mockAuthorisation()
      mockUpdatePeriodicSubmissions(
        "taxableEntityId",
        "incomeSourceId",
        2024,
        "submissionId",
        validPropertyPeriodicSubmissionRequest,
        Left(ApiServiceError(500)))

      val result = underTest.updatePeriodicSubmission("taxableEntityId", "incomeSourceId", 2024, "submissionId")(fakePutRequestWithBody)

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }

    "return bad request error when PeriodicSubmissionService returns Left(ApiServiceError)" in {
      mockAuthorisation()
      mockUpdatePeriodicSubmissions(
        "taxableEntityId",
        "incomeSourceId",
        2024,
        "submissionId",
        validPropertyPeriodicSubmissionRequest,
        Left(ApiServiceError(400)))

      val result = underTest.updatePeriodicSubmission("taxableEntityId", "incomeSourceId", 2024, "submissionId")(fakePutRequestWithBody)

      status(result) shouldBe BAD_REQUEST
    }
  }

}

