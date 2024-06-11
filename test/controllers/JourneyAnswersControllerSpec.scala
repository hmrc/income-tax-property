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
import models.common.JourneyName.About
import models.common._
import models.errors.{ApiServiceError, InvalidJsonFormatError, RepositoryError, ServiceError}
import models.request._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba.{ClaimEnhancedStructureBuildingAllowance, EsbaClaims, EsbaInfo, EsbaInfoToSave}
import models.request.sba._
import models.responses._
import org.apache.pekko.util.Timeout
import org.scalatest.time.{Millis, Span}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers._
import utils.ControllerUnitTest
import utils.mocks.{MockAuthorisedAction, MockMongoJourneyAnswersRepository, MockPropertyService}
import utils.providers.FakeRequestProvider

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class JourneyAnswersControllerSpec
  extends ControllerUnitTest with MockPropertyService with MockMongoJourneyAnswersRepository with MockAuthorisedAction
    with FakeRequestProvider with ScalaCheckPropertyChecks {

  private val underTest = new JourneyAnswersController(
    mockPropertyService,
    journeyStatusService,
    mockAuthorisedAction,
    cc
  )

  val taxYear: TaxYear = TaxYear(2024)
  val incomeSourceId: IncomeSourceId = IncomeSourceId("incomeSourceId")
  val incomeSubmissionId: SubmissionId = SubmissionId("submissionId")
  val nino: Nino = Nino("nino")
  val mtditid: Mtditid = Mtditid("1234567890")

  "create or update property about section" should {

    val validRequestBody: JsValue = Json.parse(
      """
        |{
        |   "totalIncome": "over",
        |   "ukProperty": ["property.rentals"]
        |}
        |""".stripMargin)
    val ctx: JourneyContext = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(About)

    "should return no_content for valid request body" in {

      mockAuthorisation()
      mockPersistAnswers(ctx, PropertyAbout("over", Seq("property.rentals"), Some(true)))
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyAbout(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyAbout(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update property rentals about section" should {

    val validRequestBody: JsValue = Json.parse(
      """
        |{
        |   "toexpensesLessThan1000": true,
        |   "claimPropertyIncomeAllowance": true
        |}
        |""".stripMargin)
    val ctx: JourneyContext = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(About)

    "should return no_content for valid request body" in {

      mockAuthorisation()
      mockPersistAnswers(ctx, PropertyRentalsAbout(true, true))
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

  "Update journey status for rent-a-room" should {

    val journeyStatusJs: JsValue = Json.parse(
      """
        |{
        | "status": "inProgress"
        |}
        |""".stripMargin)

    val journeyStatusErrorJs: JsValue = Json.parse(
      """
        |{
        | "foo": "completed"
        |}
        |""".stripMargin)

    val ctx = JourneyContext(
      taxYear = TaxYear(2023),
      incomeSourceId = IncomeSourceId("incomeSourceId"),
      mtditid = Mtditid("1234567890"),
      journey = JourneyName.RentARoomExpenses
    )

    "should return no_content for valid request body where a field named status is present in the body request" in {

      mockAuthorisation()

      val request = fakePostRequest.withJsonBody(journeyStatusJs)
      val result = await(underTest.setStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), "rent-a-room-expenses")(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request when a field named status is not present in the request body" in {
      mockAuthorisation()
      val request = fakePostRequest.withJsonBody(journeyStatusErrorJs)
      val result = await(underTest.setStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), "rent-a-room-expenses")(request))
      result.header.status shouldBe BAD_REQUEST
    }
  }

  "create or update property rental adjustments section" should {

    val propertyRentalAdjustmentsJs: JsValue = Json.parse(
      """
        |{
        |  "privateUseAdjustment": 12.34,
        |  "balancingCharge": {
        |    "balancingChargeYesNo": true,
        |    "balancingChargeAmount": 108
        |  },
        |  "propertyIncomeAllowance": 34.56,
        |  "renovationAllowanceBalancingCharge": {
        |    "renovationAllowanceBalancingChargeYesNo": true,
        |    "renovationAllowanceBalancingChargeAmount": 92
        |  },
        |  "residentialFinanceCost": 56.78,
        |  "unusedResidentialFinanceCost": 78.89
        |}
        """.stripMargin)
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino)

    "return no_content for valid request body" in {

      mockAuthorisation()
      mockSavePropertyRentalAdjustments(
        ctx,
        PropertyRentalAdjustments(
          BigDecimal(12.34),
          BalancingCharge(balancingChargeYesNo = true, Some(108)),
          BigDecimal(34.56),
          RenovationAllowanceBalancingCharge(
            renovationAllowanceBalancingChargeYesNo = true,
            renovationAllowanceBalancingChargeAmount = Some(92)
          ),
          BigDecimal(56.78),
          BigDecimal(78.89)
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

    val validRequestBody: JsValue = Json.parse(
      """
        |{
        |  "annualInvestmentAllowance": 11,
        |  "electricChargePointAllowance": {
        |    "electricChargePointAllowanceYesOrNo": true,
        |    "electricChargePointAllowanceAmount": 11
        |  },
        |  "zeroEmissionCarAllowance": 11,
        |  "zeroEmissionGoodsVehicleAllowance": 11,
        |  "businessPremisesRenovationController": 11,
        |  "replacementOfDomesticGoodsController": 11,
        |  "otherCapitalAllowance": 11
        |}
        """.stripMargin)
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino)

    "should return no_content for valid request body" in {

      mockAuthorisation()
      mockSavePropertyRentalAllowances(
        ctx,
        RentalAllowances(
          Some(11),
          ElectricChargePointAllowance(electricChargePointAllowanceYesOrNo = true, Some(11)),
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
      val result = underTest.savePropertyAbout(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create property income section" should {
    val validRequestBody: JsValue = Json.parse(
      """{
        |   "ukOtherPropertyIncome": {
        |        "premiumsOfLeaseGrant":52.64,
        |        "reversePremiums":34,
        |        "periodAmount":4,
        |        "otherIncome":76,
        |        "ukOtherRentARoom": {
        |          "rentsReceived":45
        |        }
        |   },
        |   "incomeToSave": {
        |        "isNonUKLandlord" : true,
        |        "incomeFromPropertyRentals" : 45,
        |        "leasePremiumPayment" : true,
        |        "reversePremiumsReceived" : {
        |            "reversePremiumsReceived" : true
        |        },
        |        "calculatedFigureYourself" : {
        |            "calculatedFigureYourself" : false
        |        },
        |        "yearLeaseAmount" : 4
        |    }
        |}""".stripMargin)

    val ctx: JourneyContext =
      JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalIncome)

    "return created for valid request body" in {

      mockAuthorisation()
      val saveIncomeRequest = validRequestBody.as[SaveIncome]
      mockSaveIncome(
        nino,
        incomeSourceId,
        taxYear,
        ctx,
        saveIncomeRequest.incomeToSave,
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

    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentARoomExpenses)

    val rentARoomNonConsolidatedRequest: JsValue = Json.parse(
      """{
        |  "consolidatedExpenses": {
        |     "consolidatedExpensesYesOrNo": false
        |  },
        |  "rentsRatesAndInsurance": 12,
        |  "repairsAndMaintenanceCosts": 23,
        |  "legalManagementOtherFee": 34,
        |  "costsOfServicesProvided": 45,
        |  "residentialPropertyFinanceCosts": 56,
        |  "unusedResidentialPropertyFinanceCostsBroughtFwd": 67,
        |  "otherPropertyExpenses": 78
        |}
        |""".stripMargin
    )

    val rentARoomConsolidatedRequest: JsValue = Json.parse(
      """
        |{
        |  "consolidatedExpenses": {
        |     "consolidatedExpensesYesOrNo": true,
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
      mockSaveExpenses(ctx, nino, rentARoomNonConsolidatedRequestBody, Some(PeriodicSubmissionId("1")).asRight[ServiceError])
      val requestNonConsolidated = fakePostRequest.withJsonBody(rentARoomNonConsolidatedRequest)
      val resultNonConsolidated = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(requestNonConsolidated))
      resultNonConsolidated.header.status shouldBe CREATED


      mockAuthorisation()
      mockSaveExpenses(ctx, nino, rentARoomConsolidatedRequestBody, Some(PeriodicSubmissionId("1")).asRight[ServiceError])
      val requestConsolidated = fakePostRequest.withJsonBody(rentARoomConsolidatedRequest)
      val resultConsolidated = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(requestConsolidated))
      resultConsolidated.header.status shouldBe CREATED

      mockAuthorisation()
      mockSaveExpenses(ctx, nino, rentARoomConsolidatedMoreThan85KBody, Some(PeriodicSubmissionId("1")).asRight[ServiceError])
      val requestConsolidatedMoreThan85 = fakePostRequest.withJsonBody(rentARoomConsolidatedMoreThan85KRequest)
      val resultConsolidatedMoreThan85 = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(requestConsolidatedMoreThan85))
      resultConsolidatedMoreThan85.header.status shouldBe CREATED
    }


    "should return bad request error when request body is empty when saving an expense" in {
      mockAuthorisation()
      val result = underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }


    "should return a conflict error when the downstream API returns a conflict error when updating an expense" in {
      mockAuthorisation()
      mockSaveExpenses(ctx, nino, rentARoomNonConsolidatedRequestBody, ApiServiceError(CONFLICT).asLeft[Option[PeriodicSubmissionId]])

      val request = fakePostRequest.withJsonBody(rentARoomNonConsolidatedRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(request))
      result.header.status shouldBe CONFLICT
    }


    "should return internal server error when the downstream API returns internal server error when updating an expense" in {
      mockAuthorisation()
      mockSaveExpenses(ctx, nino, rentARoomNonConsolidatedRequestBody, ApiServiceError(INTERNAL_SERVER_ERROR).asLeft[Option[PeriodicSubmissionId]])

      val request = fakePostRequest.withJsonBody(rentARoomNonConsolidatedRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rent-a-room-expenses")(request))
      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }

  }

  "create or update property expenses section" should {

    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalExpenses)

    val createOrUpdateUIRequest: JsValue = Json.parse(
      """{
        |  "consolidatedExpenses": {
        |    "consolidatedExpensesYesOrNo": false
        |  },
        |  "rentsRatesAndInsurance": 100,
        |  "repairsAndMaintenanceCosts": 200,
        |  "loanInterest": 300,
        |  "otherProfessionalFee": 400,
        |  "costsOfServicesProvided": 500,
        |  "otherAllowablePropertyExpenses": 600,
        |  "propertyBusinessTravelCost": 700
        |}""".stripMargin)

    val createOrUpdateRequestBody: Expenses = createOrUpdateUIRequest.as[Expenses]

    "return created for valid request body" in {

      mockAuthorisation()

      mockSaveExpenses(ctx, nino, createOrUpdateRequestBody, Some(PeriodicSubmissionId("1")).asRight[ServiceError])

      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rental-expenses")(request))
      result.header.status shouldBe CREATED
    }

    "should return no_content for valid request body" in {
      mockAuthorisation()

      mockSaveExpenses(ctx, nino, createOrUpdateRequestBody, Some(PeriodicSubmissionId("1")).asRight[ServiceError])

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
      mockSaveExpenses(ctx, nino, createOrUpdateRequestBody, ApiServiceError(CONFLICT).asLeft[Option[PeriodicSubmissionId]])

      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rental-expenses")(request))
      result.header.status shouldBe CONFLICT
    }
    "should return internal server error when the downstream API returns internal server error when updating an expense" in {
      mockAuthorisation()
      mockSaveExpenses(ctx, nino, createOrUpdateRequestBody, ApiServiceError(INTERNAL_SERVER_ERROR).asLeft[Option[PeriodicSubmissionId]])

      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino, "rental-expenses")(request))
      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }

  }

  "update esba section" should {
    val validRequestBody: JsValue = Json.parse(
      """{
        | "claimEnhancedStructureBuildingAllowance" : true,
        | "esbas": [
        |            {
        |                "esbaQualifyingDate" : "2020-04-04",
        |                "esbaQualifyingAmount" : 12,
        |                "esbaClaim" : 43,
        |                "esbaAddress" : {
        |                    "buildingName" : "name12",
        |                    "buildingNumber" : "123",
        |                    "postCode" : "XX1 1XX"
        |                }
        |            },
        |            {
        |                "esbaQualifyingDate" : "2023-01-22",
        |                "esbaQualifyingAmount" : 535,
        |                "esbaClaim" : 54,
        |                "esbaAddress" : {
        |                    "buildingName" : "235",
        |                    "buildingNumber" : "3",
        |                    "postCode" : "XX1 1XX"
        |                }
        |            },
        |            {
        |                "esbaQualifyingDate" : "2024-02-12",
        |                "esbaQualifyingAmount" : 22,
        |                "esbaClaim" : 23,
        |                "esbaAddress" : {
        |                    "buildingName" : "12",
        |                    "buildingNumber" : "2",
        |                    "postCode" : "XX1 1XX"
        |                }
        |            }
        |        ],
        |        "esbaClaims" : false
        |}""".stripMargin)

    val ctx: JourneyContext = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(About)

    import esba.EsbaInfoExtensions._
    "return no_content for valid request body" in {

      mockAuthorisation()
      val esbaInfo = validRequestBody.as[EsbaInfo]
      mockCreateOrUpdateAnnualSubmissions(
        taxYear,
        incomeSourceId,
        nino,
        PropertyAnnualSubmission.fromEsbas(esbaInfo.toEsba),
        Right("")
      )

      mockPersistAnswers(
        ctx,
        EsbaInfoToSave(
          ClaimEnhancedStructureBuildingAllowance(true),
          EsbaClaims(false)
        )
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

        mockCreateOrUpdateAnnualSubmissions(
          taxYear,
          incomeSourceId,
          nino,
          PropertyAnnualSubmission.fromEsbas(esbaInfo.toEsba),
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
    val validRequestBody: JsValue = Json.parse(
      """{
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
        |        "sbaClaims" : false
        |}""".stripMargin)

    val ctx: JourneyContext = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(About)

    import sba.SbaInfoExtensions._
    "return no_content for valid request body" in {

      mockAuthorisation()
      val sbaInfo = validRequestBody.as[SbaInfo]
      mockCreateOrUpdateAnnualSubmissions(
        taxYear,
        incomeSourceId,
        nino,
        PropertyAnnualSubmission.fromSbas(sbaInfo.toSba),
        Right("")
      )

      mockPersistAnswers(
        ctx,
        SbaInfoToSave(
          ClaimStructureBuildingAllowance(true),
          Array(
            StructureBuildingFormGroup(
              LocalDate.parse("2020-04-04"),
              12,
              43,
              Address(
                BuildingName("name12"),
                BuildingNumber("123"),
                Postcode("XX1 1XX")
              )
            )
          )
        )
      )
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

        mockCreateOrUpdateAnnualSubmissions(
          taxYear,
          incomeSourceId,
          nino,
          PropertyAnnualSubmission.fromSbas(sbaInfo.toSba),
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

  "fetch merged property data" should {
    "return success when service returns success " in {
      mockAuthorisation()
      val resultFromService = FetchedPropertyData(
        None,
        None,
        None,
        Some(
          EsbaInfo(
            ClaimEnhancedStructureBuildingAllowance(true),
            EsbaClaims(true),
            List()
          )
        )
      )
      mockGetFetchedPropertyDataMerged(taxYear, incomeSourceId, mtditid, resultFromService.asRight[ServiceError])
      val result = underTest.fetchPropertyData(taxYear, nino, incomeSourceId)(fakeGetRequest)

      status(result) shouldBe 200

      val timeout: Timeout = Timeout(Span(250, Millis))
      contentAsJson(result)(timeout) shouldBe Json.toJson(resultFromService)
    }
    "return failure when service returns failure " in {
      mockAuthorisation()
      mockGetFetchedPropertyDataMerged(taxYear, incomeSourceId, mtditid, RepositoryError.asLeft[FetchedPropertyData])
      val result = await(underTest.fetchPropertyData(taxYear, nino, incomeSourceId)(fakeGetRequest))
      result.header.status shouldBe 500
    }
  }
}
