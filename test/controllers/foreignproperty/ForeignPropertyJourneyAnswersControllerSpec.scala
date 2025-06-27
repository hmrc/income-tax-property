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

package controllers.foreignproperty

import cats.syntax.either._
import models.common._
import models.errors.{InvalidJsonFormatError, ServiceError, ApiServiceError}
import models.request.BalancingCharge
import models.request.foreign._
import models.request.foreign.adjustments.{ForeignPropertyAdjustmentsWithCountryCode, ForeignUnusedResidentialFinanceCost, UnusedLossesPreviousYears, ForeignWhenYouReportedTheLoss}
import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.request.foreign.expenses.ForeignPropertyExpensesWithCountryCode
import models.request.foreign.sba.{ForeignStructureBuildingAllowanceAddress, ForeignStructureBuildingAllowance, ForeignPropertySbaWithCountryCode}
import models.responses.PeriodicSubmissionId
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.INTERNAL_SERVER_ERROR
import play.api.libs.json.{Json, JsValue}
import play.api.test.Helpers._
import utils.ControllerUnitTest
import utils.mocks.{MockForeignPropertyService, MockMongoJourneyAnswersRepository, MockAuthorisedAction}
import utils.providers.FakeRequestProvider

import java.time.LocalDate
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
  val internalServerError: ServiceError = ApiServiceError(INTERNAL_SERVER_ERROR)

  "create or update foreign property section " should {

    val validRequestBody: JsValue =
      Json.parse("""{
                   |
                   |     "totalIncome" : "under",
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
                   |    "isForeignIncomeTax": true,
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
      val foreignIncome = validForeignIncome.as[ForeignIncomeWithCountryCode]

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
        val foreignIncomeInformation = validForeignIncome.as[ForeignIncomeWithCountryCode]

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
                   |        "isConsolidatedOrIndividualExpenses": false
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
        Right(true)
      )

      val request = fakePostRequest.withJsonBody(validForeignPropertyAllowances)
      val result = await(underTest.saveForeignPropertyAllowances(taxYear, incomeSourceId, nino)(request))

      foreignPropertyAllowances shouldBe ForeignPropertyAllowancesWithCountryCode(
        countryCode = "AUS",
        zeroEmissionsCarAllowance = Some(231.45),
        zeroEmissionsGoodsVehicleAllowance = Some(345.65),
        costOfReplacingDomesticItems = Some(490.58),
        otherCapitalAllowance = Some(600),
        None
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
          serviceError.asLeft[Boolean]
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

  "save foreign property adjustments section" should {
    val validForeignPropertyAdjustments: JsValue =
      Json.parse("""
                   |{
                   |  "countryCode": "AUS",
                   |  "privateUseAdjustment": 231.45,
                   |  "balancingCharge": {
                   |    "isBalancingCharge": true,
                   |    "balancingChargeAmount": 108
                   |  },
                   |  "residentialFinanceCost": 490.58,
                   |  "unusedResidentialFinanceCost": {
                   |    "isForeignUnusedResidentialFinanceCost": true,
                   |    "foreignUnusedResidentialFinanceCostAmount": 110.10
                   |  },
                   |  "unusedLossesPreviousYears": {
                   |    "isUnusedLossesPreviousYears": true,
                   |    "unusedLossesPreviousYearsAmount": 80.80
                   |  },
                   |  "whenYouReportedTheLoss": "y2018to2019"
                   |}
                   |""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(
        JourneyName.ForeignPropertyAdjustments
      )

    "return a header status with NO_CONTENT for a valid request" in {
      val foreignPropertyAdjustments = validForeignPropertyAdjustments.as[ForeignPropertyAdjustmentsWithCountryCode]
      mockAuthorisation()
      mockSaveForeignPropertyAdjustmentsSection(
        ctx,
        nino,
        foreignPropertyAdjustments,
        Right(true)
      )

      val request = fakePostRequest.withJsonBody(validForeignPropertyAdjustments)
      val result = await(underTest.saveForeignPropertyAdjustments(taxYear, incomeSourceId, nino)(request))

      foreignPropertyAdjustments shouldBe ForeignPropertyAdjustmentsWithCountryCode(
        countryCode = "AUS",
        privateUseAdjustment = 231.45,
        balancingCharge = BalancingCharge(
          isBalancingCharge = true,
          balancingChargeAmount = Some(108)
        ),
        residentialFinanceCost = Some(490.58),
        unusedResidentialFinanceCost = Some(ForeignUnusedResidentialFinanceCost(
          isForeignUnusedResidentialFinanceCost = true,
          foreignUnusedResidentialFinanceCostAmount = Some(110.10)
        )),
        propertyIncomeAllowanceClaim = None,
        unusedLossesPreviousYears = UnusedLossesPreviousYears(
          isUnusedLossesPreviousYears = true,
          unusedLossesPreviousYearsAmount = Some(80.80)
        ),
        whenYouReportedTheLoss = Some(ForeignWhenYouReportedTheLoss.y2018to2019)
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
        val foreignPropertyAdjustments = validForeignPropertyAdjustments.as[ForeignPropertyAdjustmentsWithCountryCode]

        mockAuthorisation()
        mockSaveForeignPropertyAdjustmentsSection(
          ctx,
          nino,
          foreignPropertyAdjustments,
          serviceError.asLeft[Boolean]
        )

        val request = fakePostRequest.withJsonBody(validForeignPropertyAdjustments)
        val result = await(underTest.saveForeignPropertyAdjustments(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveForeignPropertyAdjustments(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "save foreign property sba section " should {

    val validForeignPropertySba: JsValue =
      Json.parse("""
                   |{
                   |    "countryCode": "AUS",
                   |    "claimStructureBuildingAllowance": true,
                   |    "allowances":
                   |    [
                   |        {
                   |            "foreignStructureBuildingAllowanceClaim": 100000,
                   |            "foreignStructureBuildingQualifyingDate": "2024-11-12",
                   |            "foreignStructureBuildingQualifyingAmount": 50000,
                   |            "foreignStructureBuildingAddress":
                   |            {
                   |                "name": "Sample Building",
                   |                "number": "123",
                   |                "postCode": "AB123CD"
                   |            }
                   |        }
                   |    ]
                   |}
                   |""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(
        JourneyName.ForeignPropertySba
      )

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveForeignPropertySba(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }

    "correctly parse the values for a valid request" in {

      val foreignPropertySbaWithCountryCode = validForeignPropertySba.as[ForeignPropertySbaWithCountryCode]

      foreignPropertySbaWithCountryCode.countryCode shouldBe "AUS"
      foreignPropertySbaWithCountryCode.claimStructureBuildingAllowance shouldBe true
      foreignPropertySbaWithCountryCode.allowances.get shouldBe Seq(
        ForeignStructureBuildingAllowance(
          foreignStructureBuildingAllowanceClaim = 100000,
          foreignStructureBuildingQualifyingDate = LocalDate.parse("2024-11-12"),
          foreignStructureBuildingQualifyingAmount = 50000,
          foreignStructureBuildingAddress = ForeignStructureBuildingAllowanceAddress(
            name = "Sample Building",
            number = "123",
            postCode = "AB123CD"
          )
        )
      )
    }

    "return a header status with NO_CONTENT for a valid request" in {
      val foreignPropertySba = validForeignPropertySba.as[ForeignPropertySbaWithCountryCode]
      mockAuthorisation()
      mockSaveForeignPropertySbaSection(
        ctx,
        nino,
        foreignPropertySba,
        true.asRight[ServiceError]
      )
      val request = fakePostRequest.withJsonBody(validForeignPropertySba)
      val result = await(underTest.saveForeignPropertySba(taxYear, incomeSourceId, nino)(request))
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
        val foreignPropertySba = validForeignPropertySba.as[ForeignPropertySbaWithCountryCode]
        mockAuthorisation()
        mockSaveForeignPropertySbaSection(
          ctx,
          nino,
          foreignPropertySba,
          serviceError.asLeft[Boolean]
        )
        val request = fakePostRequest.withJsonBody(validForeignPropertySba)
        val result = await(underTest.saveForeignPropertySba(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "for foreign property sba should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveForeignPropertySba(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }

  }

  "deleteForeignPropertyAnswers" should {

    val validForeignPropertyAnswers: JsValue =
      Json.parse("""
                   |{
                   |    "journeyNames": ["foreign-property-select-country"]
                   |}
                   |""".stripMargin)

    val journeyContext = JourneyContext(taxYear, incomeSourceId, mtditid, JourneyName.ForeignPropertySelectCountry)

    "return a header status with NO_CONTENT for a valid request" in {

      val answers = validForeignPropertyAnswers.as[DeleteJourneyAnswers]

      mockAuthorisation()
      mockDeleteForeignPropertyAnswers(journeyContext, answers, true.asRight[ServiceError])

      val request = fakePostRequest.withJsonBody(validForeignPropertyAnswers)
      val result = await(underTest.deleteForeignPropertyAnswers(taxYear, incomeSourceId, nino)(request))
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

        val answers = validForeignPropertyAnswers.as[DeleteJourneyAnswers]

        mockAuthorisation()
        mockDeleteForeignPropertyAnswers(journeyContext, answers, serviceError.asLeft[Boolean])

        val request = fakePostRequest.withJsonBody(validForeignPropertyAnswers)
        val result = await(underTest.deleteForeignPropertyAnswers(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "for foreign property sba should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.deleteForeignPropertyAnswers(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }


//    "return ApiError when IF call fails" in {
//      val validForeignPropertyAnswers: JsValue =
//        Json.parse("""
//                     |{
//                     |    "journeyNames": "foreign-property-select-country"
//                     |}
//                     |""".stripMargin)
//      val journeyContext = JourneyContext(taxYear, incomeSourceId, mtditid, JourneyName.ForeignPropertySelectCountry)
//      val deleteJourneyAnswers = DeleteJourneyAnswers(Seq("foreign-property-select-country"))
//      mockAuthorisation()
//      mockDeleteForeignPropertyAnswers(journeyContext, deleteJourneyAnswers, Left(internalServerError))
//      val request = fakePostRequest.withJsonBody(validForeignPropertyAnswers)
//      await(underTest.deleteForeignPropertyAnswers(taxYear, incomeSourceId, nino)(request)) shouldBe
//        ApiServiceError(500).asLeft[DeleteJourneyAnswers]
//    }
  }

}
