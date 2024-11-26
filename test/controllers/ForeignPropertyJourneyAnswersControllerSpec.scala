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

import cats.syntax.either._
import models.common._
import models.errors.{ApiServiceError, InvalidJsonFormatError, ServiceError}
import models.request.foreign.ForeignPropertySelectCountry
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers._
import utils.ControllerUnitTest
import utils.mocks.{MockAuthorisedAction, MockForeignPropertyService, MockMongoJourneyAnswersRepository}
import utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global

class ForeignPropertyJourneyAnswersControllerSpec
    extends ControllerUnitTest with MockForeignPropertyService with MockMongoJourneyAnswersRepository
    with MockAuthorisedAction with FakeRequestProvider with ScalaCheckPropertyChecks {

  private val underTest = new ForeignPropertyJourneyAnswersController(
    mockForeignPropertyService,
    journeyStatusService,
    mockAuthorisedAction,
    cc
  )

  val taxYear: TaxYear = TaxYear(2024)
  val incomeSourceId: IncomeSourceId = IncomeSourceId("incomeSourceId")
  val incomeSubmissionId: SubmissionId = SubmissionId("submissionId")
  val nino: Nino = Nino("nino")
  val mtditid: Mtditid = Mtditid("1234567890")

  "create or update foreign property section " should {

    val validRequestBody: JsValue =
      Json.parse("""{
                   |
                   |     "totalIncome" : "lessThanOneThousand",
                   |     "claimPropertyIncomeAllowance" : false
                   |
                   |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(
        JourneyName.ForeignPropertySelectCountry
      )

    "return boolean true for valid request body" in {
      val foreignPropertyInformation = validRequestBody.as[ForeignPropertySelectCountry]

      mockAuthorisation()
      mockSaveSelectCountrySection(
        ctx,
        nino,
        foreignPropertyInformation,
        true.asRight[ServiceError]
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveSelectCountrySection(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "return serviceError BAD_REQUEST when BAD_REQUEST returns from Downstream Api" in {
      val scenarios = Table[ServiceError, Int](
        ("Error", "Expected Response"),
        (ApiServiceError(BAD_REQUEST), BAD_REQUEST),
        (ApiServiceError(CONFLICT), CONFLICT),
        (InvalidJsonFormatError("", "", Nil), INTERNAL_SERVER_ERROR)
      )

      forAll(scenarios) { (serviceError: ServiceError, expectedError: Int) =>
        val foreignPropertyInformation = validRequestBody.as[ForeignPropertySelectCountry]

        mockAuthorisation()
        mockSaveSelectCountrySection(
          ctx,
          nino,
          foreignPropertyInformation,
          serviceError.asLeft[Boolean]
        )

        val request = fakePostRequest.withJsonBody(validRequestBody)
        val result = await(underTest.saveSelectCountrySection(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveSelectCountrySection(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }
}
