/*
 * Copyright 2025 HM Revenue & Customs
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

import cats.implicits.catsSyntaxEitherId
import models.common._
import models.errors.{ApiServiceError, InvalidJsonFormatError, ServiceError}
import models.request.ukAndForeign.UkAndForeignAbout
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.{BAD_REQUEST, CONFLICT, INTERNAL_SERVER_ERROR, NO_CONTENT}
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers.status
import utils.ControllerUnitTest
import utils.mocks._
import utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global

class UkAndForeignJourneyAnswersControllerSpec extends ControllerUnitTest with MockUkAndForeignPropertyService with MockMongoJourneyAnswersRepository
  with MockAuthorisedAction with FakeRequestProvider with ScalaCheckPropertyChecks {

  private val underTest = new UkAndForeignJourneyAnswersController(
    mockUkAndForeignPropertyService,
    mockAuthorisedAction,
    cc
  )

  val taxYear: TaxYear = TaxYear(2025)
  val incomeSourceId: IncomeSourceId = IncomeSourceId("incomeSourceId")
  val mtditid: Mtditid = Mtditid("1234567890")
  val nino: Nino = Nino("nino")

  "create or update uk and foreign property section " should {

    val validRequestBody: JsValue =
      Json.parse(
        """{
          |  "totalPropertyIncome": "lessThan",
          |  "reportIncome": "doNotWantToReport"
          |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(
        JourneyName.UkAndForeignPropertyAbout
      )

    "return boolean true for valid request body" in {
      val ukAndForeignPropertiesInformation = validRequestBody.as[UkAndForeignAbout]

      mockAuthorisation()
      mockSaveUkAndForeignPropertyAbout(
        ctx,
        ukAndForeignPropertiesInformation,
        true.asRight[ServiceError]
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveUkAndForeignPropertyAbout(taxYear, incomeSourceId, nino)(request))
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
        val foreignPropertyInformation = validRequestBody.as[UkAndForeignAbout]

        mockAuthorisation()
        mockSaveUkAndForeignPropertyAbout(
          ctx,
          foreignPropertyInformation,
          serviceError.asLeft[Boolean]
        )

        val request = fakePostRequest.withJsonBody(validRequestBody)
        val result = await(underTest.saveUkAndForeignPropertyAbout(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveUkAndForeignPropertyAbout(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

}
