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
import uk.gov.hmrc.incometaxproperty.models.responses.{AnnualForeignFhlEea, ForeignFhlAdjustments, ForeignFhlAllowances, PropertyAnnualSubmission}
import uk.gov.hmrc.incometaxproperty.utils.ControllerUnitTest
import uk.gov.hmrc.incometaxproperty.utils.mocks.{MockAuthorisedAction, MockPropertyService}
import uk.gov.hmrc.incometaxproperty.utils.providers.FakeRequestProvider

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
      submittedOn = LocalDateTime.now,
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
        Left(ApiServiceError("error")))

      val result = underTest.getAnnualSubmission(2024, "taxableEntityId", "incomeSourceId")(fakeGetRequest)

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }
  }
}

