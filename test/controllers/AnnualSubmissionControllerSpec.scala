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

import models.common.{BusinessId, Nino, TaxYear}
import models.errors.{ApiServiceError, DataNotFoundError}
import models.responses._
import play.api.http.Status._
import play.api.libs.json.Json
import play.api.test.Helpers.status
import utils.ControllerUnitTest
import utils.mocks.{MockAuthorisedAction, MockPropertyService}
import utils.providers.FakeRequestProvider

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global

class AnnualSubmissionControllerSpec extends ControllerUnitTest
  with MockPropertyService
  with MockAuthorisedAction
  with FakeRequestProvider {

  private val underTest = new AnnualSubmissionController(
    mockPropertyService,
    mockAuthorisedAction,
    cc
  )

  ".getAnnualSubmissionData" should {

    val annualSubmission = PropertyAnnualSubmission(
      submittedOn = Some(LocalDateTime.now),
      Some(AnnualForeignFhlEea(
        ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
        ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
      )), None, None, None
    )

    "return a property Annual submission when IntegrationFrameworkService returns Right(aAnnualSubmission)" in {
      mockAuthorisation()
      mockGetAnnualSubmission(2024,
        "taxableEntityId",
        "incomeSourceId",
        Right(annualSubmission))

      val result = await(underTest.getAnnualSubmission(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest))

      result.header.status shouldBe OK
      Json.parse(consumeBody(result)) shouldBe Json.toJson(annualSubmission)
    }

    "return not found when AnnualSubmissionService returns Left(DataNotFoundError)" in {
      mockAuthorisation()
      mockGetAnnualSubmission(2024,
        "taxableEntityId",
        "incomeSourceId",
        Left(DataNotFoundError))

      val result = underTest.getAnnualSubmission(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest)

      status(result) shouldBe NOT_FOUND
    }

    "return internal server error when AnnualSubmissionService returns Left(ApiServiceError)" in {
      mockAuthorisation()
      mockGetAnnualSubmission(2024,
        "taxableEntityId",
        "incomeSourceId",
        Left(ApiServiceError(500)))

      val result = underTest.getAnnualSubmission(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest)

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }
  }


  "Delete Annual Submission" should {

    "return NO_CONTENT when IntegrationFrameworkService returns Right()" in {
      mockAuthorisation()
      mockDeleteAnnualSubmission(
        "incomeSourceId",
        "taxableEntityId",
        2024,
        Right())

      val result = await(underTest.deleteAnnualSubmission("incomeSourceId", "taxableEntityId", 2024)(fakePutRequest))

      result.header.status shouldBe NO_CONTENT
    }

    "return BAD_REQUEST when AnnualSubmissionService returns Left(ApiServiceError(400))" in {
      mockAuthorisation()
      mockDeleteAnnualSubmission(
        "incomeSourceId",
        "taxableEntityId",
        2024,
        Left(ApiServiceError(400)))

      val result = await(underTest.deleteAnnualSubmission("incomeSourceId", "taxableEntityId", 2024)(fakePutRequest))

      result.header.status shouldBe BAD_REQUEST
    }

    "return UNPROCESSABLE_ENTITY when AnnualSubmissionService returns Left(ApiServiceError(422))" in {
      mockAuthorisation()
      mockDeleteAnnualSubmission(
        "incomeSourceId",
        "taxableEntityId",
        2024,
        Left(ApiServiceError(422)))

      val result = await(underTest.deleteAnnualSubmission("incomeSourceId", "taxableEntityId", 2024)(fakePutRequest))

      result.header.status shouldBe UNPROCESSABLE_ENTITY
    }

    "return INTERNAL_SERVER_ERROR when AnnualSubmissionService returns Left(ApiServiceError(500))" in {
      mockAuthorisation()
      mockDeleteAnnualSubmission(
        "incomeSourceId",
        "taxableEntityId",
        2024,
        Left(ApiServiceError(500)))

      val result = await(underTest.deleteAnnualSubmission("incomeSourceId", "taxableEntityId", 2024)(fakePutRequest))

      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }
  }

  val propertyAnnualSubmission = PropertyAnnualSubmission(
    submittedOn = Some(LocalDateTime.now),
    Some(AnnualForeignFhlEea(
      ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
      ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
    )), None, None, None)

  ".createOrUpdateAnnualSubmission" should {
    val validRequestBody = Json.toJson(PropertyAnnualSubmission(
      submittedOn = Some(LocalDateTime.now),
      Some(AnnualForeignFhlEea(
        ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
        ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
      )), None, None, None))


    val fakePutRequestWithJsonBody = fakePutRequest.withJsonBody(Json.toJson(propertyAnnualSubmission))
    val fakePostRequestWithJsonBody = fakePostRequest.withJsonBody(Json.toJson(propertyAnnualSubmission))

    "creating an annual submission returns no content" in {
        mockAuthorisation()
        mockCreateOrUpdateAnnualSubmissions(
          TaxYear(2024),
          BusinessId("incomeSourceId"),
          Nino("nino"),
          propertyAnnualSubmission,
          Right())

        val result = await(underTest.createOrUpdateAnnualSubmission("nino", "incomeSourceId", 2024)(fakePutRequestWithJsonBody))

        result.header.status shouldBe NO_CONTENT
        consumeBody(result) shouldBe empty
      }

      "return unprocessable-entity when AnnualSubmissionService returns conflict error" in {
        mockAuthorisation()
        mockCreateOrUpdateAnnualSubmissions(
          TaxYear(2024),
          BusinessId("incomeSourceId"),
          Nino("nino"),
          propertyAnnualSubmission,
          Left(ApiServiceError(422)))

        val result = underTest.createOrUpdateAnnualSubmission("nino", "incomeSourceId", 2024)(fakePostRequestWithJsonBody)

        status(result) shouldBe UNPROCESSABLE_ENTITY
      }

      "return internal server error when AnnualSubmissionService returns Left(ApiServiceError)" in {
        mockAuthorisation()
        mockCreateOrUpdateAnnualSubmissions(
          TaxYear(2024),
          BusinessId("incomeSourceId"),
          Nino("nino"),
          propertyAnnualSubmission,
          Left(ApiServiceError(500)))

        val result = underTest.createOrUpdateAnnualSubmission("nino", "incomeSourceId", 2024)(fakePostRequestWithJsonBody)

        status(result) shouldBe INTERNAL_SERVER_ERROR
      }

      "return bad request error when AnnualSubmissionService returns Left(ApiServiceError)" in {
        mockAuthorisation()
        mockCreateOrUpdateAnnualSubmissions(
          TaxYear(2024),
          BusinessId("incomeSourceId"),
          Nino("nino"),
          propertyAnnualSubmission,
          Left(ApiServiceError(400)))

        val result = underTest.createOrUpdateAnnualSubmission("nino", "incomeSourceId", 2024)(fakePostRequestWithJsonBody)

        status(result) shouldBe BAD_REQUEST
      }
  }
}

