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
import models.request.foreign._
import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.request.foreign.expenses.ForeignPropertyExpensesWithCountryCode
import models.responses.PeriodicSubmissionId
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
    mockAuthorisedAction,
    cc
  )

  val taxYear: TaxYear = TaxYear(2024)
  val incomeSourceId: IncomeSourceId = IncomeSourceId("incomeSourceId")
  val incomeSubmissionId: SubmissionId = SubmissionId("submissionId123")
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

  "create or update foreign tax section " should {

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(
        JourneyName.ForeignPropertyTax
      )
    val validRequestBody: JsValue =
      Json.parse("""
                   |{
                   |  "countryCode": "USA",
                   |  "foreignIncomeTax": {
                   |    "foreignIncomeTaxYesNo": true,
                   |    "foreignTaxPaidOrDeducted": 65
                   |     },
                   |  "foreignTaxCreditRelief": true
                   |}
                   |""".stripMargin)

    val foreignPropertyTaxWithCountryCode: ForeignPropertyTaxWithCountryCode =
      validRequestBody.as[ForeignPropertyTaxWithCountryCode]

    "saveForeignPropertyTax" should {

      "return NO_CONTENT when save operation is successful" in {
        mockAuthorisation()
        mockSaveForeignPropertyTax(
          ctx,
          nino,
          foreignPropertyTaxWithCountryCode,
          Right(Some(PeriodicSubmissionId(incomeSubmissionId.value)))
        )

        val request = fakePostRequest.withJsonBody(validRequestBody)
        val result = await(underTest.saveForeignPropertyTax(taxYear, incomeSourceId, nino)(request))

        result.header.status shouldBe NO_CONTENT
      }

      "return service error when downstream API responds with an error" in {
        val scenarios = Table[ServiceError, Int](
          ("Service Error", "Expected Response"),
          (ApiServiceError(BAD_REQUEST), BAD_REQUEST),
          (ApiServiceError(CONFLICT), CONFLICT),
          (InvalidJsonFormatError("ForeignPropertyTax", "Invalid data", Nil), INTERNAL_SERVER_ERROR)
        )

        forAll(scenarios) { (serviceError: ServiceError, expectedResponse: Int) =>
          mockAuthorisation()
          mockSaveForeignPropertyTax(ctx, nino, foreignPropertyTaxWithCountryCode, Left(serviceError))

          val request = fakePostRequest.withJsonBody(validRequestBody)
          val result = await(underTest.saveForeignPropertyTax(taxYear, incomeSourceId, nino)(request))

          result.header.status shouldBe expectedResponse
        }
      }

      "return BAD_REQUEST when request body is invalid" in {
        val invalidRequestBody: JsValue = Json.obj("invalid" -> "data")

        mockAuthorisation()
        val request = fakePostRequest.withJsonBody(invalidRequestBody)
        val result = underTest.saveForeignPropertyTax(taxYear, incomeSourceId, nino)(request)

        status(result) shouldBe BAD_REQUEST
      }

      "return BAD_REQUEST when request body is missing" in {
        mockAuthorisation()
        val request = fakePostRequest
        val result = underTest.saveForeignPropertyTax(taxYear, incomeSourceId, nino)(request)

        status(result) shouldBe BAD_REQUEST
      }
    }

  }

  "save foreign income section " should {

    val validForeignIncome: JsValue =
      Json.parse("""{
                   |    "countryCode": "AUS",
                   |    "rentIncome": 541.65,
                   |    "premiumsGrantLeaseReceived": true,
                   |    "reversePremiumsReceived": {
                   |      "reversePremiumsReceived": true,
                   |      "reversePremiums": 434.45
                   |    },
                   |    "otherPropertyIncome": 102.50,
                   |    "calculatedPremiumLeaseTaxable": {
                   |      "calculatedPremiumLeaseTaxable": true,
                   |      "premiumsOfLeaseGrant": 23.85
                   |    },
                   |    "receivedGrantLeaseAmount": 54.25,
                   |    "twelveMonthPeriodsInLease": 6,
                   |    "premiumsOfLeaseGrantAgreed" : {
                   |      "premiumsOfLeaseGrantAgreed": true,
                   |      "premiumsOfLeaseGrant": 9
                   |    }
                   |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(
        JourneyName.ForeignPropertyIncome
      )

    "return a header status with NO_CONTENT for a valid request" in {
      val foreignIncome = validForeignIncome.as[ForeignIncome]

      mockAuthorisation()
      mockSaveForeignIncomeSection(
        ctx,
        nino,
        foreignIncome,
        Right(Some(PeriodicSubmissionId(incomeSubmissionId.value)))
      )

      val request = fakePostRequest.withJsonBody(validForeignIncome)
      val result = await(underTest.saveForeignIncome(taxYear, incomeSourceId, nino)(request))
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
        val foreignIncomeInformation = validForeignIncome.as[ForeignIncome]

        mockAuthorisation()
        mockSaveForeignIncomeSection(
          ctx,
          nino,
          foreignIncomeInformation,
          serviceError.asLeft[Option[PeriodicSubmissionId]]
        )

        val request = fakePostRequest.withJsonBody(validForeignIncome)
        val result = await(underTest.saveForeignIncome(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveForeignIncome(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "save foreign property expenses " should {

    val validRequestBody: JsValue =
      Json.parse("""{
                   |
                   |     "countryCode" : "ESP",
                   |     "consolidatedExpenses": {
                   |        "consolidatedOrIndividualExpensesYesNo": false
                   |     },
                   |      "premisesRunningCosts" : 70,
                   |      "repairsAndMaintenance" : 80,
                   |      "financialCosts" : 550,
                   |      "professionalFees" : 900,
                   |      "costOfServices" : 345,
                   |      "other" : 734
                   |
                   |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(
        JourneyName.ForeignPropertyExpenses
      )

    "return boolean true for valid request body" in {
      val foreignPropertyExpenses = validRequestBody.as[ForeignPropertyExpensesWithCountryCode]

      mockAuthorisation()
      mockSaveForeignPropertyExpenses(
        ctx,
        nino,
        foreignPropertyExpenses,
        Right(Some(PeriodicSubmissionId("submissionId")))
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveForeignPropertyExpenses(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveForeignPropertyExpenses(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "save foreign property allowances section " should {

    val validForeignPropertyAllowances: JsValue =
      Json.parse("""{
                   |    "countryCode": "AUS",
                   |    "zeroEmissionsCarAllowance": 231.45,
                   |    "zeroEmissionsGoodsVehicleAllowance": 345.65,
                   |    "costOfReplacingDomesticItems": 490.58,
                   |    "otherCapitalAllowance": 600
                   |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(
        JourneyName.ForeignPropertyAllowances
      )

    "return a header status with NO_CONTENT for a valid request" in {
      val foreignPropertyAllowances = validForeignPropertyAllowances.as[ForeignPropertyAllowancesWithCountryCode]

      mockAuthorisation()
      mockSaveForeignPropertyAllowancesSection(
        ctx,
        nino,
        foreignPropertyAllowances,
        Right(Some(PeriodicSubmissionId(incomeSubmissionId.value)))
      )

      val request = fakePostRequest.withJsonBody(validForeignPropertyAllowances)
      val result = await(underTest.saveForeignPropertyAllowances(taxYear, incomeSourceId, nino)(request))

      foreignPropertyAllowances shouldBe ForeignPropertyAllowancesWithCountryCode(
        countryCode = "AUS",
        zeroEmissionsCarAllowance = Some(231.45),
        zeroEmissionsGoodsVehicleAllowance = Some(345.65),
        costOfReplacingDomesticItems = Some(490.58),
        otherCapitalAllowance = Some(600),
        annualInvestmentAllowance = None,
        propertyAllowance = None,
        electricChargePointAllowance = None,
        structuredBuildingAllowance = None
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
        val foreignPropertyAllowances = validForeignPropertyAllowances.as[ForeignPropertyAllowancesWithCountryCode]

        mockAuthorisation()
        mockSaveForeignPropertyAllowancesSection(
          ctx,
          nino,
          foreignPropertyAllowances,
          serviceError.asLeft[Option[PeriodicSubmissionId]]
        )

        val request = fakePostRequest.withJsonBody(validForeignPropertyAllowances)
        val result = await(underTest.saveForeignPropertyAllowances(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveForeignPropertyAllowances(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

}
