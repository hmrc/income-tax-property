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

package controllers.foreignincome

import cats.syntax.either._
import models.common._
import models.errors.{ServiceError, InvalidJsonFormatError, ApiServiceError}
import models.request.foreignIncome.ForeignIncomeDividendsWithCountryCode
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{Json, JsValue}
import play.api.test.Helpers._
import utils.ControllerUnitTest
import utils.mocks.{MockForeignIncomeService, MockMongoJourneyAnswersRepository, MockAuthorisedAction}
import utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global

class ForeignIncomeJourneyAnswersControllerSpec
  extends ControllerUnitTest with MockForeignIncomeService with MockMongoJourneyAnswersRepository
    with MockAuthorisedAction with FakeRequestProvider with ScalaCheckPropertyChecks {

  private val underTest = new ForeignIncomeJourneyAnswersController(
    mockForeignIncomeService,
    mockAuthorisedAction,
    cc
  )

  val taxYear: TaxYear = TaxYear(2024)
  val incomeSourceId: IncomeSourceId = IncomeSourceId("incomeSourceId")
  val incomeSubmissionId: SubmissionId = SubmissionId("submissionId123")
  val nino: Nino = Nino("nino")
  val mtditid: Mtditid = Mtditid("1234567890")

  "save foreign income dividends section" should {
    val validForeignIncomeDividends: JsValue =
      Json.parse("""
                   |{
                   |  "countryCode": "AUS",
                   |  "amountBeforeTax": 231.45,
                   |  "taxTakenOff": 321.54,
                   |  "specialWithholdingTax": 490.58,
                   |  "foreignTaxCreditRelief": true,
                   |  "taxableAmount": 80.80
                   |}
                   |""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(
        JourneyName.ForeignIncomeDividends
      )

    "return a header status with NO_CONTENT for a valid request" in {
      val foreignIncomeDividends = validForeignIncomeDividends.as[ForeignIncomeDividendsWithCountryCode]
      mockAuthorisation()
      mockSaveForeignIncomeDividendsSection(
        ctx,
        nino,
        foreignIncomeDividends,
        Right(true)
      )

      val request = fakePostRequest.withJsonBody(validForeignIncomeDividends)
      val result = await(underTest.saveForeignIncomeDividends(taxYear, incomeSourceId, nino)(request))

      foreignIncomeDividends shouldBe ForeignIncomeDividendsWithCountryCode(
        countryCode = "AUS",
        amountBeforeTax = Some(231.45),
        taxTakenOff = Some(321.54),
        specialWithholdingTax = Some(490.58),
        foreignTaxCreditRelief = true,
        taxableAmount = 80.80
      )
      result.header.status shouldBe NO_CONTENT
    }

    "return serviceError when there is an error in Downstream Api or error in Parsing" in {
      val scenarios = Table[ServiceError, Int](
        ("Error", "Expected Response"),
        (ApiServiceError(BAD_REQUEST), BAD_REQUEST),
        (ApiServiceError(CONFLICT), CONFLICT),
        (InvalidJsonFormatError("", "", Nil), INTERNAL_SERVER_ERROR)
      )

      forAll(scenarios) { (serviceError: ServiceError, expectedError: Int) =>
        val foreignIncomeDividends = validForeignIncomeDividends.as[ForeignIncomeDividendsWithCountryCode]

        mockAuthorisation()
        mockSaveForeignIncomeDividendsSection(
          ctx,
          nino,
          foreignIncomeDividends,
          serviceError.asLeft[Boolean]
        )

        val request = fakePostRequest.withJsonBody(validForeignIncomeDividends)
        val result = await(underTest.saveForeignIncomeDividends(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveForeignIncomeDividends(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

}
