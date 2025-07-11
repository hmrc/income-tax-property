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

package controllers.ukproperty

import cats.syntax.either._
import models.common.JourneyName.{About, RentARoomAbout, RentARoomExpenses, RentARoomAdjustments, RentalAbout}
import models.common._
import models.errors.{ApiServiceError, InvalidJsonFormatError, ServiceError}
import models.request.WhenYouReportedTheLoss.y2018to2019
import models.request._
import models.request.esba.EsbaInfo
import models.request.foreign.TotalIncome
import models.request.sba._
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import models.{RentalsAndRaRAbout, UKPropertySelect}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers._
import utils.ControllerUnitTest
import utils.mocks.{MockAuthorisedAction, MockMongoJourneyAnswersRepository, MockPropertyService}
import utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global

class JourneyAnswersControllerSpec
    extends ControllerUnitTest with MockPropertyService with MockMongoJourneyAnswersRepository with MockAuthorisedAction
    with FakeRequestProvider with ScalaCheckPropertyChecks {

  private val underTest = new JourneyAnswersController(
    mockPropertyService,
    mockAuthorisedAction,
    cc
  )

  val taxYear: TaxYear = TaxYear(2024)
  val incomeSourceId: IncomeSourceId = IncomeSourceId("incomeSourceId")
  val incomeSubmissionId: SubmissionId = SubmissionId("submissionId")
  val nino: Nino = Nino("nino")
  val mtditid: Mtditid = Mtditid("1234567890")

  "create or update property about section" should {

    val validRequestBody: JsValue = Json.parse("""
                                                 |{
                                                 |   "totalIncome": "over",
                                                 |   "ukProperty": ["property.rentals"]
                                                 |}
                                                 |""".stripMargin)
    val ctx: JourneyContext = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(About)

    "return no_content for valid request body" in {

      mockAuthorisation()
      mockSavePropertyAbout(
        ctx,
        PropertyAbout(
          totalIncome = TotalIncome.Over.toString,
          ukProperty = Some(Seq(UKPropertySelect.PropertyRentals)),
          reportPropertyIncome = None
        ),
        true.asRight[ServiceError]
      )
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyAbout(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyAbout(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update uk rent a room about section" should {
    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(RentARoomAbout)

    val validRequestBody: JsValue = Json.parse("""{
                                                 |    "isJointlyLet" : true,
                                                 |    "totalIncomeAmount" : 55.22,
                                                 |    "claimExpensesOrRelief" : {
                                                 |        "isClaimExpensesOrRelief" : true,
                                                 |        "rentARoomAmount" : 10.22
                                                 |    }
                                                 |}""".stripMargin)

    "return CREATED for valid request body" in {

      mockAuthorisation()
      mockSaveUkRentARoomAbout(
        ctx,
        nino,
        RaRAbout(
          isJointlyLet = true,
          55.22,
          ClaimExpensesOrRelief(isClaimExpensesOrRelief = true, Some(10.22))
        ),
        result = true
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveRentARoomAbout(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveRentARoomAbout(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update uk rent a room allowances section" should {
    val ctx: JourneyContextWithNino =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino)

    val validRequestBody: JsValue = Json.parse("""{
                                                 |    "capitalAllowancesForACar" : {
                                                 |        "isCapitalAllowancesForACar" : true,
                                                 |        "capitalAllowancesForACarAmount" : 12.34
                                                 |    },
                                                 |    "zeroEmissionCarAllowance" : 34.56,
                                                 |    "zeroEmissionGoodsVehicleAllowance" : 56.78,
                                                 |    "replacementOfDomesticGoodsAllowance" : 78.90,
                                                 |    "otherCapitalAllowance" : 90.12
                                                 |}""".stripMargin)

    "return no_content for valid request body" in {

      mockAuthorisation()
      mockSaveUkRentARoomAllowances(
        ctx,
        models.request.RentARoomAllowances(
          capitalAllowancesForACar = Some(CapitalAllowancesForACar(isCapitalAllowancesForACar = true, Some(12.34))),
          zeroEmissionCarAllowance = Some(34.56),
          zeroEmissionGoodsVehicleAllowance = Some(56.78),
          replacementOfDomesticGoodsAllowance = Some(78.90),
          otherCapitalAllowance = Some(90.12)
        )
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveRentARoomAllowances(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveRentARoomAllowances(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update uk rent a room expenses section" should {
    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(RentARoomExpenses)

    val validRequestBody: JsValue = Json.parse("""{
                                                 |    "consolidatedExpenses" : {
                                                 |        "isConsolidatedExpenses" : true,
                                                 |        "consolidatedExpensesAmount" : 12.34
                                                 |    },
                                                 |    "rentsRatesAndInsurance": 34.56,
                                                 |    "repairsAndMaintenanceCosts" : 56.78,
                                                 |    "legalManagementOtherFee" : 78.90,
                                                 |    "costOfServicesProvided" : 90.12,
                                                 |    "otherPropertyExpenses" : 12.34
                                                 |}""".stripMargin)

    "return CREATED for valid request body" in {

      mockAuthorisation()
      mockSaveUkRaRExpenses(
        ctx,
        nino,
        models.request.RentARoomExpenses(
          consolidatedExpenses = Some(ConsolidatedExpenses(isConsolidatedExpenses = true, consolidatedExpensesAmount = Some(12.34))),
          rentsRatesAndInsurance = Some(34.56),
          repairsAndMaintenanceCosts = Some(56.78),
          legalManagementOtherFee = Some(78.90),
          costOfServicesProvided = Some(90.12),
          otherPropertyExpenses = Some(12.34)
        ),

        Some(PeriodicSubmissionId("1")).asRight[ServiceError]
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveRaRExpenses(taxYear, incomeSourceId, nino, ctx.journey.toString)(request))
      result.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveRaRExpenses(taxYear, incomeSourceId, nino, ctx.journey.toString)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }

    "return INTERNAL_SERVER_ERROR when service returns service error" in {

      mockAuthorisation()
      mockSaveUkRaRExpenses(
        ctx,
        nino,
        models.request.RentARoomExpenses(
          consolidatedExpenses = Some(ConsolidatedExpenses(isConsolidatedExpenses = true, consolidatedExpensesAmount = Some(12.34))),
          rentsRatesAndInsurance = Some(34.56),
          repairsAndMaintenanceCosts = Some(56.78),
          legalManagementOtherFee = Some(78.90),
          costOfServicesProvided = Some(90.12),
          otherPropertyExpenses = Some(12.34)
        ),
        ApiServiceError(500).asLeft[Option[PeriodicSubmissionId]]
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveRaRExpenses(taxYear, incomeSourceId, nino, ctx.journey.toString)(request))
      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }
  }

  "create or update uk rent a room adjustments section" should {
    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(RentARoomAdjustments)

    val validRequestBody: JsValue = Json.parse("""{
                                                 |    "balancingCharge" : {
                                                 |        "isBalancingCharge" : true,
                                                 |        "balancingChargeAmount" : 12.34
                                                 |    },
                                                 |    "unusedResidentialPropertyFinanceCostsBroughtFwd": 12,
                                                 |    "unusedLossesBroughtForward" : {
                                                 |        "isUnusedLossesBroughtForward" : true,
                                                 |        "unusedLossesBroughtForwardAmount" : 12.56
                                                 |    },
                                                 |    "whenYouReportedTheLoss" : "y2018to2019"
                                                 |}""".stripMargin)

    "return CREATED for valid request body" in {

      mockAuthorisation()
      mockSaveUkRaRAdjustments(
        ctx,
        nino,
        RaRAdjustments(
          Some(BalancingCharge(isBalancingCharge = true, Some(12.34))),
          Some(BigDecimal(12)),
          Some(UnusedLossesBroughtForward(isUnusedLossesBroughtForward = true, Some(12.56))),
          Some(WhenYouReportedTheLoss.y2018to2019)
        ),
        true.asRight[ServiceError]
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyRaRAdjustments(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyRaRAdjustments(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }

    "return INTERNAL_SERVER_ERROR when service returns service error" in {

      mockAuthorisation()
      mockSaveUkRaRAdjustments(
        ctx,
        nino,
        RaRAdjustments(
          Some(BalancingCharge(isBalancingCharge = true, Some(12.34))),
          Some(BigDecimal(12)),
          Some(UnusedLossesBroughtForward(isUnusedLossesBroughtForward = true, Some(12.56))),
          Some(WhenYouReportedTheLoss.y2018to2019)
        ),
        ApiServiceError(500).asLeft[Boolean]
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyRaRAdjustments(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }
  }

  "create or update property rentals about section" should {

    val validRequestBody: JsValue = Json.parse("""
                                                 |{
                                                 |   "isClaimPropertyIncomeAllowance": true
                                                 |}
                                                 |""".stripMargin)
    val ctx: JourneyContext = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(RentalAbout)

    "should return no_content for valid request body" in {

      mockAuthorisation()
      mockSavePropertyRentalAbout(
        ctx,
        PropertyRentalsAbout(isClaimPropertyIncomeAllowance = true),
        true.asRight[ServiceError]
      )
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyRentalAbout(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyRentalAbout(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }



  "create or update property rental adjustments section" should {

    val propertyRentalAdjustmentsJs: JsValue = Json.parse("""
                                                            |{
                                                            |  "privateUseAdjustment": 12.34,
                                                            |  "balancingCharge": {
                                                            |    "isBalancingCharge": true,
                                                            |    "balancingChargeAmount": 108
                                                            |  },
                                                            |  "propertyIncomeAllowance": 34.56,
                                                            |  "renovationAllowanceBalancingCharge": {
                                                            |    "isRenovationAllowanceBalancingCharge": true,
                                                            |    "renovationAllowanceBalancingChargeAmount": 92
                                                            |  },
                                                            |  "residentialFinanceCost": 56.78,
                                                            |  "unusedResidentialFinanceCost": 78.89,
                                                            |  "unusedLossesBroughtForward" : {
                                                            |        "isUnusedLossesBroughtForward" : true,
                                                            |        "unusedLossesBroughtForwardAmount" : 12.56
                                                            |    },
                                                            |    "whenYouReportedTheLoss" : "y2018to2019"
                                                            |}
        """.stripMargin)
    val ctx = JourneyContext(taxYear, incomeSourceId, mtditid, JourneyName.RentalAdjustments)

    "return no_content for valid request body" in {

      mockAuthorisation()
      mockSavePropertyRentalAdjustments(
        ctx,
        nino,
        PropertyRentalAdjustments(
          BigDecimal(12.34),
          BalancingCharge(isBalancingCharge = true, Some(108)),
          Some(BigDecimal(34.56)),
          RenovationAllowanceBalancingCharge(
            isRenovationAllowanceBalancingCharge = true,
            renovationAllowanceBalancingChargeAmount = Some(92)
          ),
          BigDecimal(56.78),
          Some(BigDecimal(78.89)),
          UnusedLossesBroughtForward(isUnusedLossesBroughtForward = true, Some(12.56)),
          Some(y2018to2019)
        )
      )

      val request = fakePostRequest.withJsonBody(propertyRentalAdjustmentsJs)
      val result = await(underTest.savePropertyRentalAdjustments(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body does not have property rental adjustments and is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyRentalAdjustments(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update property allowances section" should {

    val validRequestBody: JsValue = Json.parse("""
                                                 |{
                                                 |  "annualInvestmentAllowance": 11,
                                                 |  "zeroEmissionCarAllowance": 11,
                                                 |  "zeroEmissionGoodsVehicleAllowance": 11,
                                                 |  "businessPremisesRenovationController": 11,
                                                 |  "replacementOfDomesticGoodsController": 11,
                                                 |  "otherCapitalAllowance": 11
                                                 |}
        """.stripMargin)
    val ctx = JourneyContext(taxYear, incomeSourceId, mtditid, JourneyName.RentalAllowances)

    "should return no_content for valid request body" in {

      mockAuthorisation()
      mockSavePropertyRentalAllowances(
        ctx,
        nino,
        RentalAllowances(
          None,
          Some(11),
          Some(11),
          Some(11),
          Some(11),
          Some(11),
          Some(11)
        )
      )
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyRentalAllowances(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyRentalAllowances(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create property income section" should {
    val validRequestBody: JsValue = Json.parse("""
                                                 |{
                                                 |            "propertyRentalIncome" : 15,
                                                 |            "isNonUKLandlord" : false,
                                                 |            "otherIncomeFromProperty" : 25,
                                                 |            "deductingTax" : {
                                                 |                "isTaxDeducted" : true,
                                                 |                "taxDeductedAmount" : 20
                                                 |            },
                                                 |            "premiumsGrantLease" : {
                                                 |                "premiumsGrantLeaseReceived" : true,
                                                 |                "premiumsGrantLease" : 5
                                                 |            },
                                                 |            "reversePremiumsReceived" : {
                                                 |                "reversePremiumsReceived" : true,
                                                 |                "reversePremiums" : 10
                                                 |            }
                                                 |        }
                                                 |""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalIncome)

    "return created for valid request body" in {

      mockAuthorisation()
      val saveIncomeRequest = validRequestBody.as[PropertyRentalsIncome]
      mockSaveIncome(
        nino,
        incomeSourceId,
        taxYear,
        ctx,
        saveIncomeRequest,
        Right(Some(PeriodicSubmissionId("submissionId")))
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveIncome(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveIncome(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update rent a room expenses section" should {

    val ctx =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentARoomExpenses)

    val rentARoomNonConsolidatedRequest: JsValue = Json.parse(
      """{
        |  "consolidatedExpenses": {
        |     "isConsolidatedExpenses": true,
        |     "consolidatedExpensesAmount": 1000
        |  },
        |  "rentsRatesAndInsurance": 12,
        |  "repairsAndMaintenanceCosts": 23,
        |  "legalManagementOtherFee": 34,
        |  "costsOfServicesProvided": 45,
        |  "residentialPropertyFinanceCosts": 56,
        |  "unusedResidentialPropertyFinanceCostsBroughtFwd": 67,
        |  "otherPropertyExpenses": 78,
        |  "propertyBusinessTravelCosts": 89,
        |  "loanInterestOrOtherFinancialCost": 90,
        |  "otherAllowablePropertyExpenses": 100
        |}
        |""".stripMargin
    )

    val rentARoomConsolidatedRequest: JsValue = Json.parse(
      """
        |{
        |  "consolidatedExpenses": {
        |     "isConsolidatedExpenses": true,
        |     "consolidatedExpensesAmount": 1000
        |  }
        |}
        |""".stripMargin
    )

    val rentARoomConsolidatedMoreThan85KRequest: JsValue = Json.parse(
      """
        | {
        |     "rentsRatesAndInsurance": 12,
        |     "repairsAndMaintenanceCosts": 23,
        |     "legalManagementOtherFee": 34,
        |     "costsOfServicesProvided": 45,
        |     "residentialPropertyFinanceCosts": 56,
        |     "unusedResidentialPropertyFinanceCostsBroughtFwd": 67,
        |     "otherPropertyExpenses": 78
        |  }
        |""".stripMargin
    )

    val rentARoomNonConsolidatedRequestBody: Expenses = rentARoomNonConsolidatedRequest.as[Expenses]
    val rentARoomConsolidatedRequestBody: Expenses = rentARoomConsolidatedRequest.as[Expenses]
    val rentARoomConsolidatedMoreThan85KBody: Expenses = rentARoomConsolidatedMoreThan85KRequest.as[Expenses]

    "transpose all the values from the request body into the Expense model" in {
      rentARoomNonConsolidatedRequestBody.consolidatedExpenses shouldBe Some(ConsolidatedExpenses(true, Some(1000)))
      rentARoomNonConsolidatedRequestBody.rentsRatesAndInsurance shouldBe Some(12)
      rentARoomNonConsolidatedRequestBody.repairsAndMaintenanceCosts shouldBe Some(23)
      rentARoomNonConsolidatedRequestBody.costsOfServicesProvided shouldBe Some(45)
      rentARoomNonConsolidatedRequestBody.propertyBusinessTravelCosts shouldBe Some(89)
      rentARoomNonConsolidatedRequestBody.loanInterestOrOtherFinancialCost shouldBe Some(90)
      rentARoomNonConsolidatedRequestBody.otherAllowablePropertyExpenses shouldBe Some(100)
      rentARoomNonConsolidatedRequestBody.otherProfessionalFees shouldBe None
    }

    "return created for valid request body" in {

      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomNonConsolidatedRequestBody,
        Some(PeriodicSubmissionId("1")).asRight[ServiceError]
      )
      val requestNonConsolidated = fakePostRequest.withJsonBody(rentARoomNonConsolidatedRequest)
      val resultNonConsolidated =
        await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(requestNonConsolidated))
      resultNonConsolidated.header.status shouldBe CREATED

      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomConsolidatedRequestBody,
        Some(PeriodicSubmissionId("1")).asRight[ServiceError]
      )
      val requestConsolidated = fakePostRequest.withJsonBody(rentARoomConsolidatedRequest)
      val resultConsolidated =
        await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(requestConsolidated))
      resultConsolidated.header.status shouldBe CREATED

      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomConsolidatedMoreThan85KBody,
        Some(PeriodicSubmissionId("1")).asRight[ServiceError]
      )
      val requestConsolidatedMoreThan85 = fakePostRequest.withJsonBody(rentARoomConsolidatedMoreThan85KRequest)
      val resultConsolidatedMoreThan85 = await(
        underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(requestConsolidatedMoreThan85)
      )
      resultConsolidatedMoreThan85.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty when saving an expense" in {
      mockAuthorisation()
      val result = underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }

    "should return a conflict error when the downstream API returns a conflict error when updating an expense" in {
      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomNonConsolidatedRequestBody,
        ApiServiceError(CONFLICT).asLeft[Option[PeriodicSubmissionId]]
      )

      val request = fakePostRequest.withJsonBody(rentARoomNonConsolidatedRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(request))
      result.header.status shouldBe CONFLICT
    }

    "should return internal server error when the downstream API returns internal server error when updating an expense" in {
      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomNonConsolidatedRequestBody,
        ApiServiceError(INTERNAL_SERVER_ERROR).asLeft[Option[PeriodicSubmissionId]]
      )

      val request = fakePostRequest.withJsonBody(rentARoomNonConsolidatedRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(request))
      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }

  }

  "create or update property expenses section" should {

    val ctx =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalExpenses)

    val createOrUpdateUIRequest: JsValue = Json.parse("""{
                                                        |  "consolidatedExpenses": {
                                                        |    "isConsolidatedExpenses": false
                                                        |  },
                                                        |  "rentsRatesAndInsurance": 100,
                                                        |  "repairsAndMaintenanceCosts": 200,
                                                        |  "loanInterestOrOtherFinancialCost": 300,
                                                        |  "otherProfessionalFees": 400,
                                                        |  "costsOfServicesProvided": 500,
                                                        |  "otherAllowablePropertyExpenses": 600,
                                                        |  "propertyBusinessTravelCosts": 700
                                                        |}""".stripMargin)

    val expensesFromUIRequestBody: Expenses = createOrUpdateUIRequest.as[Expenses]

    "should deserialize all the fields submitted from the UI to the backend" in {
      expensesFromUIRequestBody.consolidatedExpenses shouldBe Some(ConsolidatedExpenses(false, None))
      expensesFromUIRequestBody.rentsRatesAndInsurance shouldBe Some(100)
      expensesFromUIRequestBody.repairsAndMaintenanceCosts shouldBe Some(200)
      expensesFromUIRequestBody.loanInterestOrOtherFinancialCost shouldBe Some(300)
      expensesFromUIRequestBody.otherProfessionalFees shouldBe Some(400)
      expensesFromUIRequestBody.costsOfServicesProvided shouldBe Some(500)
      expensesFromUIRequestBody.otherAllowablePropertyExpenses shouldBe Some(600)
      expensesFromUIRequestBody.propertyBusinessTravelCosts shouldBe Some(700)
    }

    "return created for valid request body" in {

      mockAuthorisation()

      mockSaveExpenses(ctx, nino, expensesFromUIRequestBody, Some(PeriodicSubmissionId("1")).asRight[ServiceError])

      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rental-expenses")(request))
      result.header.status shouldBe CREATED
    }

    "should return no_content for valid request body" in {
      mockAuthorisation()

      mockSaveExpenses(ctx, nino, expensesFromUIRequestBody, Some(PeriodicSubmissionId("1")).asRight[ServiceError])

      val request = fakePutRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rental-expenses")(request))
      result.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty when saving an expense" in {
      mockAuthorisation()
      val result = underTest.saveExpenses(taxYear, incomeSourceId, nino, "rental-expenses")(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }

    "should return a conflict error when the downstream API returns a conflict error when updating an expense" in {
      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        expensesFromUIRequestBody,
        ApiServiceError(CONFLICT).asLeft[Option[PeriodicSubmissionId]]
      )

      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rental-expenses")(request))
      result.header.status shouldBe CONFLICT
    }
    "should return internal server error when the downstream API returns internal server error when updating an expense" in {
      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        expensesFromUIRequestBody,
        ApiServiceError(INTERNAL_SERVER_ERROR).asLeft[Option[PeriodicSubmissionId]]
      )

      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rental-expenses")(request))
      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }

  }

  "update esba section" should {
    val validRequestBody: JsValue =
      Json.parse("""{
                   | "claimEnhancedStructureBuildingAllowance" : true,
                   | "enhancedStructureBuildingAllowances": [
                   |            {
                   |                "enhancedStructureBuildingAllowanceQualifyingDate" : "2020-04-04",
                   |                "enhancedStructureBuildingAllowanceQualifyingAmount" : 12,
                   |                "enhancedStructureBuildingAllowanceClaim" : 43,
                   |                "enhancedStructureBuildingAllowanceAddress" : {
                   |                    "buildingName" : "name12",
                   |                    "buildingNumber" : "123",
                   |                    "postCode" : "XX1 1XX"
                   |                }
                   |            },
                   |            {
                   |                "enhancedStructureBuildingAllowanceQualifyingDate" : "2023-01-22",
                   |                "enhancedStructureBuildingAllowanceQualifyingAmount" : 535,
                   |                "enhancedStructureBuildingAllowanceClaim" : 54,
                   |                "enhancedStructureBuildingAllowanceAddress" : {
                   |                    "buildingName" : "235",
                   |                    "buildingNumber" : "3",
                   |                    "postCode" : "XX1 1XX"
                   |                }
                   |            },
                   |            {
                   |                "enhancedStructureBuildingAllowanceQualifyingDate" : "2024-02-12",
                   |                "enhancedStructureBuildingAllowanceQualifyingAmount" : 22,
                   |                "enhancedStructureBuildingAllowanceClaim" : 23,
                   |                "enhancedStructureBuildingAllowanceAddress" : {
                   |                    "buildingName" : "12",
                   |                    "buildingNumber" : "2",
                   |                    "postCode" : "XX1 1XX"
                   |                }
                   |            }
                   |        ],
                   |        "enhancedStructureBuildingAllowanceClaims" : false
                   |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalESBA)
    "return no_content for valid request body" in {
      val esbaInfo = validRequestBody.as[EsbaInfo]
      mockAuthorisation()

      mockSaveEsbas(
        ctx,
        nino,
        esbaInfo,
        ().asRight[ServiceError]
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveEsba(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "return BAD_REQUEST when BAD_REQUEST returns from Downstream Api" in {
      val scenarios = Table[ServiceError, Int](
        ("Error", "Expected Response"),
        (ApiServiceError(BAD_REQUEST), BAD_REQUEST),
        (ApiServiceError(CONFLICT), CONFLICT),
        (InvalidJsonFormatError("", "", Nil), INTERNAL_SERVER_ERROR)
      )

      forAll(scenarios) { (serviceError: ServiceError, expectedError: Int) =>
        mockAuthorisation()
        val esbaInfo = validRequestBody.as[EsbaInfo]

        mockSaveEsbas(
          ctx,
          nino,
          esbaInfo,
          Left(serviceError)
        )
        val request = fakePostRequest.withJsonBody(validRequestBody)
        val result = await(underTest.saveEsba(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveEsba(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "update sba section" should {
    val validRequestBody: JsValue = Json.parse("""{
                                                 | "claimStructureBuildingAllowance" : true,
                                                 | "structureBuildingFormGroup": [
                                                 |            {
                                                 |                "structureBuildingQualifyingDate" : "2020-04-04",
                                                 |                "structureBuildingQualifyingAmount" : 12,
                                                 |                "structureBuildingAllowanceClaim" : 43,
                                                 |                "structuredBuildingAllowanceAddress" : {
                                                 |                    "buildingName" : "name12",
                                                 |                    "buildingNumber" : "123",
                                                 |                    "postCode" : "XX1 1XX"
                                                 |                }
                                                 |            },
                                                 |            {
                                                 |                "structureBuildingQualifyingDate" : "2023-01-22",
                                                 |                "structureBuildingQualifyingAmount" : 535,
                                                 |                "structureBuildingAllowanceClaim" : 54,
                                                 |                "structuredBuildingAllowanceAddress" : {
                                                 |                    "buildingName" : "235",
                                                 |                    "buildingNumber" : "3",
                                                 |                    "postCode" : "XX1 1XX"
                                                 |                }
                                                 |            },
                                                 |            {
                                                 |                "structureBuildingQualifyingDate" : "2024-02-12",
                                                 |                "structureBuildingQualifyingAmount" : 22,
                                                 |                "structureBuildingAllowanceClaim" : 23,
                                                 |                "structuredBuildingAllowanceAddress" : {
                                                 |                    "buildingName" : "12",
                                                 |                    "buildingNumber" : "2",
                                                 |                    "postCode" : "XX1 1XX"
                                                 |                }
                                                 |            }
                                                 |        ],
                                                 |        "structureAndBuildingAllowanceClaims" : false
                                                 |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalSBA)
    "return no_content for valid request body" in {
      val sbaInfo = validRequestBody.as[SbaInfo]

      mockAuthorisation()

      mockSaveSBA(ctx, nino, sbaInfo, ().asRight[ServiceError])

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyRentalSBA(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "return BAD_REQUEST when BAD_REQUEST returns from Downstream Api" in {
      val scenarios = Table[ServiceError, Int](
        ("Error", "Expected Response"),
        (ApiServiceError(BAD_REQUEST), BAD_REQUEST),
        (ApiServiceError(CONFLICT), CONFLICT),
        (InvalidJsonFormatError("", "", Nil), INTERNAL_SERVER_ERROR)
      )

      forAll(scenarios) { (serviceError: ServiceError, expectedError: Int) =>
        mockAuthorisation()
        val sbaInfo = validRequestBody.as[SbaInfo]

        mockSaveSBA(
          ctx,
          nino,
          sbaInfo,
          Left(serviceError)
        )

        val request = fakePostRequest.withJsonBody(validRequestBody)
        val result = await(underTest.savePropertyRentalSBA(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyRentalSBA(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }



  "create or update property rentals and rent a room about section" should {
    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(RentARoomAbout)

    val validRequestBody: JsValue = Json.parse("""{
                                                 |    "isJointlyLet" : true,
                                                 |    "totalIncomeAmount" : 55.22,
                                                 |    "isClaimPropertyIncomeAllowance": true,
                                                 |    "propertyRentalIncome": 22.33,
                                                 |    "claimExpensesOrRelief" : {
                                                 |        "isClaimExpensesOrRelief" : true,
                                                 |        "rentARoomAmount" : 10.22
                                                 |    }
                                                 |}""".stripMargin)

    "return CREATED for valid request body" in {

      mockAuthorisation()

      val journeyContextForPropertyRentalsAndRentARoomAbout =
        ctx.copy(journey = JourneyName.RentalsAndRaRAbout)

      mockSaveRentalsAndRentARoomAbout(
        journeyContextForPropertyRentalsAndRentARoomAbout,
        nino,
        RentalsAndRaRAbout(
          isJointlyLet = true,
          55.22,
          isClaimPropertyIncomeAllowance = true,
          22.33,
          ClaimExpensesOrRelief(isClaimExpensesOrRelief = true, Some(10.22))
        ),
        result = true
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveRentalsAndRaRAbout(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveRentalsAndRaRAbout(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create property rentals and rent a room income section" should {
    val validRequestBody: JsValue = Json.parse("""
                                                 |{
                                                 |            "propertyRentalIncome" : 15,
                                                 |            "isNonUKLandlord" : false,
                                                 |            "otherIncomeFromProperty" : 25,
                                                 |            "deductingTax" : {
                                                 |                "isTaxDeducted" : true,
                                                 |                "taxDeductedAmount" : 20
                                                 |            },
                                                 |            "premiumsGrantLease" : {
                                                 |                "premiumsGrantLeaseReceived" : true,
                                                 |                "premiumsGrantLease" : 5
                                                 |            },
                                                 |            "reversePremiumsReceived" : {
                                                 |                "reversePremiumsReceived" : true,
                                                 |                "reversePremiums" : 10
                                                 |            }
                                                 |        }
                                                 |""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalsAndRaRIncome)

    "return created for valid request body" in {

      mockAuthorisation()
      val saveIncomeRequest = validRequestBody.as[RentalsAndRaRIncome]
      mockSaveRentalsAndRaRIncome(
        nino,
        incomeSourceId,
        taxYear,
        ctx,
        saveIncomeRequest,
        Right(Some(PeriodicSubmissionId("submissionId")))
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveRentalsAndRaRIncome(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveIncome(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update rentals and rent a room expenses section" should {

    val ctx =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalsAndRaRExpenses)

    val rentARoomNonConsolidatedRequest: JsValue = Json.parse(
      """{
        |  "consolidatedExpenses": {
        |     "isConsolidatedExpenses": false
        |  },
        |  "rentsRatesAndInsurance": 12,
        |  "repairsAndMaintenanceCosts": 23,
        |  "legalManagementOtherFee": 34,
        |  "costsOfServicesProvided": 45,
        |  "residentialPropertyFinanceCosts": 56,
        |  "unusedResidentialPropertyFinanceCostsBroughtFwd": 67,
        |  "otherPropertyExpenses": 78,
        |  "propertyBusinessTravelCosts": 89,
        |  "loanInterestOrOtherFinancialCost": 90,
        |  "otherAllowablePropertyExpenses": 100
        |}
        |""".stripMargin
    )

    val rentARoomConsolidatedRequest: JsValue = Json.parse(
      """
        |{
        |  "consolidatedExpenses": {
        |     "isConsolidatedExpenses": true,
        |     "consolidatedExpensesAmount": 1000
        |  }
        |}
        |""".stripMargin
    )

    val rentARoomConsolidatedMoreThan85KRequest: JsValue = Json.parse(
      """
        | {
        |     "rentsRatesAndInsurance": 12,
        |     "repairsAndMaintenanceCosts": 23,
        |     "legalManagementOtherFee": 34,
        |     "costsOfServicesProvided": 45,
        |     "residentialPropertyFinanceCosts": 56,
        |     "unusedResidentialPropertyFinanceCostsBroughtFwd": 67,
        |     "otherPropertyExpenses": 78
        |  }
        |""".stripMargin
    )

    val rentARoomNonConsolidatedRequestBody: Expenses = rentARoomNonConsolidatedRequest.as[Expenses]
    val rentARoomConsolidatedRequestBody: Expenses = rentARoomConsolidatedRequest.as[Expenses]
    val rentARoomConsolidatedMoreThan85KBody: Expenses = rentARoomConsolidatedMoreThan85KRequest.as[Expenses]

    "return created for valid request body" in {

      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomNonConsolidatedRequestBody,
        Some(PeriodicSubmissionId("1")).asRight[ServiceError]
      )
      val requestNonConsolidated = fakePostRequest.withJsonBody(rentARoomNonConsolidatedRequest)
      val resultNonConsolidated =
        await(
          underTest.saveRentalsAndRaRExpenses(taxYear, incomeSourceId, nino)(
            requestNonConsolidated
          )
        )
      resultNonConsolidated.header.status shouldBe CREATED

      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomConsolidatedRequestBody,
        Some(PeriodicSubmissionId("1")).asRight[ServiceError]
      )
      val requestConsolidated = fakePostRequest.withJsonBody(rentARoomConsolidatedRequest)
      val resultConsolidated =
        await(
          underTest.saveExpenses(taxYear, incomeSourceId, nino, "property-rentals-and-rent-a-room-expenses")(
            requestConsolidated
          )
        )
      resultConsolidated.header.status shouldBe CREATED

      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomConsolidatedMoreThan85KBody,
        Some(PeriodicSubmissionId("1")).asRight[ServiceError]
      )
      val requestConsolidatedMoreThan85 = fakePostRequest.withJsonBody(rentARoomConsolidatedMoreThan85KRequest)
      val resultConsolidatedMoreThan85 = await(
        underTest.saveRentalsAndRaRExpenses(taxYear, incomeSourceId, nino)(
          requestConsolidatedMoreThan85
        )
      )
      resultConsolidatedMoreThan85.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty when saving an expense" in {
      mockAuthorisation()
      val result =
        underTest.saveRentalsAndRaRExpenses(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }

    "should return a conflict error when the downstream API returns a conflict error when updating an expense" in {
      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomNonConsolidatedRequestBody,
        ApiServiceError(CONFLICT).asLeft[Option[PeriodicSubmissionId]]
      )

      val request = fakePostRequest.withJsonBody(rentARoomNonConsolidatedRequest)
      val result =
        await(underTest.saveRentalsAndRaRExpenses(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe CONFLICT
    }

    "should return internal server error when the downstream API returns internal server error when updating an expense" in {
      mockAuthorisation()
      mockSaveExpenses(
        ctx,
        nino,
        rentARoomNonConsolidatedRequestBody,
        ApiServiceError(INTERNAL_SERVER_ERROR).asLeft[Option[PeriodicSubmissionId]]
      )

      val request = fakePostRequest.withJsonBody(rentARoomNonConsolidatedRequest)
      val result =
        await(underTest.saveRentalsAndRaRExpenses(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }

  }

  "create or update rentals and rent a room allowances section" should {

    val validRequestBody: JsValue = Json.parse("""
                                                 |{
                                                 |  "annualInvestmentAllowance": 11,
                                                 |  "zeroEmissionCarAllowance": 11,
                                                 |  "zeroEmissionGoodsVehicleAllowance": 11,
                                                 |  "businessPremisesRenovationController": 11,
                                                 |  "replacementOfDomesticGoodsController": 11,
                                                 |  "otherCapitalAllowance": 11
                                                 |}
        """.stripMargin)
    val ctx = JourneyContext(taxYear, incomeSourceId, mtditid, JourneyName.RentARoomAllowances)

    "should return no_content for valid request body" in {

      mockAuthorisation()
      mockSavePropertyRentalAllowances(
        ctx,
        nino,
        RentalAllowances(
          None,
          Some(11),
          Some(11),
          Some(11),
          Some(11),
          Some(11),
          Some(11)
        )
      )
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveRentalsAndRaRAllowances(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveRentalsAndRaRAllowances(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "update sba section for rentals and rent a room" should {
    val validRequestBody: JsValue = Json.parse("""{
                                                 | "claimStructureBuildingAllowance" : true,
                                                 | "structureBuildingFormGroup": [
                                                 |            {
                                                 |                "structureBuildingQualifyingDate" : "2020-04-04",
                                                 |                "structureBuildingQualifyingAmount" : 12,
                                                 |                "structureBuildingAllowanceClaim" : 43,
                                                 |                "structuredBuildingAllowanceAddress" : {
                                                 |                    "buildingName" : "name12",
                                                 |                    "buildingNumber" : "123",
                                                 |                    "postCode" : "XX1 1XX"
                                                 |                }
                                                 |            },
                                                 |            {
                                                 |                "structureBuildingQualifyingDate" : "2023-01-22",
                                                 |                "structureBuildingQualifyingAmount" : 535,
                                                 |                "structureBuildingAllowanceClaim" : 54,
                                                 |                "structuredBuildingAllowanceAddress" : {
                                                 |                    "buildingName" : "235",
                                                 |                    "buildingNumber" : "3",
                                                 |                    "postCode" : "XX1 1XX"
                                                 |                }
                                                 |            },
                                                 |            {
                                                 |                "structureBuildingQualifyingDate" : "2024-02-12",
                                                 |                "structureBuildingQualifyingAmount" : 22,
                                                 |                "structureBuildingAllowanceClaim" : 23,
                                                 |                "structuredBuildingAllowanceAddress" : {
                                                 |                    "buildingName" : "12",
                                                 |                    "buildingNumber" : "2",
                                                 |                    "postCode" : "XX1 1XX"
                                                 |                }
                                                 |            }
                                                 |        ]
                                                 |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalsAndRaRSBA)
    "return no_content for valid request body" in {
      val sbaInfo = validRequestBody.as[SbaInfo]

      mockAuthorisation()

      mockSaveSBA(ctx, nino, sbaInfo, ().asRight[ServiceError])

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savRentalsAndRaRSBA(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "return BAD_REQUEST when BAD_REQUEST returns from Downstream Api" in {
      val scenarios = Table[ServiceError, Int](
        ("Error", "Expected Response"),
        (ApiServiceError(BAD_REQUEST), BAD_REQUEST),
        (ApiServiceError(CONFLICT), CONFLICT),
        (InvalidJsonFormatError("", "", Nil), INTERNAL_SERVER_ERROR)
      )

      forAll(scenarios) { (serviceError: ServiceError, expectedError: Int) =>
        mockAuthorisation()
        val sbaInfo = validRequestBody.as[SbaInfo]

        mockSaveSBA(
          ctx,
          nino,
          sbaInfo,
          Left(serviceError)
        )

        val request = fakePostRequest.withJsonBody(validRequestBody)
        val result = await(underTest.savRentalsAndRaRSBA(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savRentalsAndRaRSBA(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update property rentals and rent a room adjustments section" should {

    val propertyRentalAdjustmentsJs: JsValue = Json.parse("""
                                                            |{
                                                            |  "privateUseAdjustment": 12.34,
                                                            |  "balancingCharge": {
                                                            |    "isBalancingCharge": true,
                                                            |    "balancingChargeAmount": 108
                                                            |  },
                                                            |  "propertyIncomeAllowance": 34.56,
                                                            |  "renovationAllowanceBalancingCharge": {
                                                            |    "isRenovationAllowanceBalancingCharge": true,
                                                            |    "renovationAllowanceBalancingChargeAmount": 92
                                                            |  },
                                                            |  "residentialFinanceCost": 56.78,
                                                            |  "unusedResidentialFinanceCost": 78.89,
                                                            |  "unusedLossesBroughtForward" : {
                                                            |        "isUnusedLossesBroughtForward" : true,
                                                            |        "unusedLossesBroughtForwardAmount" : 12.56
                                                            |    },
                                                            |    "whenYouReportedTheLoss" : "y2018to2019"
                                                            |}
        """.stripMargin)
    val ctx = JourneyContext(taxYear, incomeSourceId, mtditid, JourneyName.RentalsAndRaRAdjustments)

    "return no_content for valid request body" in {

      mockAuthorisation()
      mockSavePropertyRentalAdjustments(
        ctx,
        nino,
        PropertyRentalAdjustments(
          BigDecimal(12.34),
          BalancingCharge(isBalancingCharge = true, Some(108)),
          Some(BigDecimal(34.56)),
          RenovationAllowanceBalancingCharge(
            isRenovationAllowanceBalancingCharge = true,
            renovationAllowanceBalancingChargeAmount = Some(92)
          ),
          BigDecimal(56.78),
          Some(BigDecimal(78.89)),
          UnusedLossesBroughtForward(isUnusedLossesBroughtForward = true, Some(12.56)),
          Some(y2018to2019)
        )
      )

      val request = fakePostRequest.withJsonBody(propertyRentalAdjustmentsJs)
      val result = await(underTest.saveRentalsAndRaRAdjustments(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body does not have property rental adjustments and is empty" in {
      mockAuthorisation()
      val result = underTest.saveRentalsAndRaRAdjustments(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update esba section for combined journey" should {
    val validRequestBody: JsValue =
      Json.parse("""{
                   | "claimEnhancedStructureBuildingAllowance" : true,
                   | "enhancedStructureBuildingAllowances": [
                   |            {
                   |                "enhancedStructureBuildingAllowanceQualifyingDate" : "2020-04-04",
                   |                "enhancedStructureBuildingAllowanceQualifyingAmount" : 12,
                   |                "enhancedStructureBuildingAllowanceClaim" : 43,
                   |                "enhancedStructureBuildingAllowanceAddress" : {
                   |                    "buildingName" : "name12",
                   |                    "buildingNumber" : "123",
                   |                    "postCode" : "XX1 1XX"
                   |                }
                   |            },
                   |            {
                   |                "enhancedStructureBuildingAllowanceQualifyingDate" : "2023-01-22",
                   |                "enhancedStructureBuildingAllowanceQualifyingAmount" : 535,
                   |                "enhancedStructureBuildingAllowanceClaim" : 54,
                   |                "enhancedStructureBuildingAllowanceAddress" : {
                   |                    "buildingName" : "235",
                   |                    "buildingNumber" : "3",
                   |                    "postCode" : "XX1 1XX"
                   |                }
                   |            },
                   |            {
                   |                "enhancedStructureBuildingAllowanceQualifyingDate" : "2024-02-12",
                   |                "enhancedStructureBuildingAllowanceQualifyingAmount" : 22,
                   |                "enhancedStructureBuildingAllowanceClaim" : 23,
                   |                "enhancedStructureBuildingAllowanceAddress" : {
                   |                    "buildingName" : "12",
                   |                    "buildingNumber" : "2",
                   |                    "postCode" : "XX1 1XX"
                   |                }
                   |            }
                   |        ],
                   |        "enhancedStructureBuildingAllowanceClaims" : false
                   |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalsAndRaRESBA)
    "return no_content for valid request body" in {
      val esbaInfo = validRequestBody.as[EsbaInfo]
      mockAuthorisation()

      mockSaveEsbas(
        ctx,
        nino,
        esbaInfo,
        ().asRight[ServiceError]
      )

      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveRentalsAndRaREsba(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "return BAD_REQUEST when BAD_REQUEST returns from Downstream Api" in {
      val scenarios = Table[ServiceError, Int](
        ("Error", "Expected Response"),
        (ApiServiceError(BAD_REQUEST), BAD_REQUEST),
        (ApiServiceError(CONFLICT), CONFLICT),
        (InvalidJsonFormatError("", "", Nil), INTERNAL_SERVER_ERROR)
      )

      forAll(scenarios) { (serviceError: ServiceError, expectedError: Int) =>
        mockAuthorisation()
        val esbaInfo = validRequestBody.as[EsbaInfo]

        mockSaveEsbas(
          ctx,
          nino,
          esbaInfo,
          Left(serviceError)
        )
        val request = fakePostRequest.withJsonBody(validRequestBody)
        val result = await(underTest.saveRentalsAndRaREsba(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveRentalsAndRaREsba(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

}
