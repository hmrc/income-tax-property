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
import models.request.esba.{ClaimEnhancedStructureBuildingAllowance, EsbaClaims, EsbaInfo, EsbaInfoToSave}
import models.request.sba._
import models.responses._
import org.apache.pekko.util.Timeout
import org.scalatest.time.{Millis, Span}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers.status
import utils.ControllerUnitTest
import utils.mocks.{MockAuthorisedAction, MockMongoJourneyAnswersRepository, MockPropertyService}
import utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.test.Helpers._

class JourneyAnswersControllerSpec
  extends ControllerUnitTest with MockPropertyService with MockMongoJourneyAnswersRepository with MockAuthorisedAction with FakeRequestProvider
    with ScalaCheckPropertyChecks {

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
      journey = JourneyName.RentARoom)


    "should return no_content for valid request body where a field named status is present in the body request" in {

      mockAuthorisation()

      val request = fakePostRequest.withJsonBody(journeyStatusJs)
      val result = await(underTest.setStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), "rent-a-room")(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request when a field named status is not present in the request body" in {
      mockAuthorisation()
      val request = fakePostRequest.withJsonBody(journeyStatusErrorJs)
      val result = await(underTest.setStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), "rent-a-room")(request))
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
        |  "residentialFinancialCost": 56.78,
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
        |    "electricChargePointAllowanceYesNo": true,
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
          ElectricChargePointAllowance(electricChargePointAllowanceYesNo = true, Some(11)),
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

    val ctx: JourneyContext = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(JourneyName.RentalIncome)


    "return created for valid request body" in {

      mockAuthorisation()
      val saveIncomeRequest = validRequestBody.as[SaveIncome]
      mockSaveIncome(
        nino,
        incomeSourceId,
        taxYear,
        ctx,
        saveIncomeRequest.incomeToSave,
        saveIncomeRequest.ukOtherPropertyIncome,
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

  "create or update property expenses section" should {

    val createOrUpdateUIRequest: JsValue = Json.parse(
      """{
        |    "rentsRatesAndInsurance": 100,
        |    "repairsAndMaintenanceCosts": 200,
        |    "loanInterest": 300,
        |    "otherProfessionalFee": 400,
        |    "costsOfServicesProvided": 500,
        |    "otherAllowablePropertyExpenses": 600,
        |    "propertyBusinessTravelCost": 700
        |}""".stripMargin)

    val createOrUpdateRequestBody: Expenses = createOrUpdateUIRequest.as[Expenses]
    import createOrUpdateRequestBody._

    "should return created for valid request body" in {

      mockAuthorisation()
      mockCreatePeriodicSubmissions(
        nino.value,
        "incomeSourceId",
        taxYear.endYear,
        PropertyPeriodicSubmissionRequest(
          None,
          None,
          None,
          None,
          Some(
            UkOtherProperty(
              UkOtherPropertyIncome(
                Some(0),
                None,
                None,
                None,
                None,
                None
              ),
              UkOtherPropertyExpenses(
                premisesRunningCosts = rentsRatesAndInsurance,
                repairsAndMaintenance = repairsAndMaintenanceCosts,
                financialCosts = loanInterest,
                professionalFees = otherProfessionalFee,
                travelCosts = propertyBusinessTravelCost,
                costOfServices = costsOfServicesProvided,
                other = otherAllowablePropertyExpenses,
                None,
                None,
                None,
                None
              )
            )
          )
        ),
        Some(PeriodicSubmissionId("submissionId")).asRight[ServiceError]
      )

      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe CREATED
    }

    "should return no_content for valid request body" in {
      mockAuthorisation()
      mockUpdatePeriodicSubmissions(
        nino.value,
        incomeSourceId.value,
        taxYear.endYear,
        incomeSubmissionId.value,
        PropertyPeriodicSubmissionRequest(
          None,
          None,
          None,
          None,
          Some(
            UkOtherProperty(
              UkOtherPropertyIncome(
                Some(0),
                None,
                None,
                None,
                None,
                None
              ),
              UkOtherPropertyExpenses(
                premisesRunningCosts = rentsRatesAndInsurance,
                repairsAndMaintenance = repairsAndMaintenanceCosts,
                financialCosts = loanInterest,
                professionalFees = otherProfessionalFee,
                travelCosts = propertyBusinessTravelCost,
                costOfServices = costsOfServicesProvided,
                other = otherAllowablePropertyExpenses,
                None,
                None,
                None,
                None
              )
            )
          )
        ),
        Right("")
      )

      val request = fakePutRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.updateExpenses(taxYear, incomeSourceId, nino, incomeSubmissionId)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty when saving an expense" in {
      mockAuthorisation()
      val result = underTest.saveExpenses(taxYear, incomeSourceId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }

    "should return a conflict error when the downstream API returns a conflict error when updating an expense" in {
      mockAuthorisation()
      mockCreateOrUpdateAnnualSubmissionsNew2(
        nino.value,
        "incomeSourceId",
        taxYear.endYear,
        incomeSubmissionId.value,
        PropertyPeriodicSubmissionRequest.fromExpenses(createOrUpdateRequestBody),
        Left(ApiServiceError(CONFLICT))
      )
      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.updateExpenses(taxYear, incomeSourceId, nino, incomeSubmissionId)(request))
      result.header.status shouldBe CONFLICT
    }

    "should return internal server error when the downstream API returns internal server error when updating an expense" in {
      mockAuthorisation()
      mockCreateOrUpdateAnnualSubmissionsNew2(
        nino.value,
        "incomeSourceId",
        taxYear.endYear,
        incomeSubmissionId.value,
        PropertyPeriodicSubmissionRequest.fromExpenses(createOrUpdateRequestBody),
        Left(ApiServiceError(INTERNAL_SERVER_ERROR))
      )
      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.updateExpenses(taxYear, incomeSourceId, nino, incomeSubmissionId)(request))
      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }

    "should return a conflict error when the downstream API returns a conflict error when saving an expense" in {
      mockAuthorisation()
      mockCreateOrUpdateAnnualSubmissionsNew(
        nino.value,
        "incomeSourceId",
        taxYear.endYear,
        PropertyPeriodicSubmissionRequest.fromExpenses(createOrUpdateRequestBody),
        Left(ApiServiceError(CONFLICT))
      )
      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe CONFLICT
    }

    "should return internal server error when the downstream API returns internal server error when saving an expense" in {
      mockAuthorisation()
      mockCreateOrUpdateAnnualSubmissionsNew(
        nino.value,
        "incomeSourceId",
        taxYear.endYear,
        PropertyPeriodicSubmissionRequest.fromExpenses(createOrUpdateRequestBody),
        Left(ApiServiceError(INTERNAL_SERVER_ERROR))
      )
      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, incomeSourceId, nino)(request))
      result.header.status shouldBe INTERNAL_SERVER_ERROR
    }
  }

  "update property income section" should {
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

    val ctx: JourneyContext = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(About)

    "return no_content for valid request body" in {

      mockAuthorisation()
      val saveIncomeRequest = validRequestBody.as[SaveIncome]
      mockUpdatePeriodicSubmissions(
        nino.value,
        "incomeSourceId",
        taxYear.endYear,
        "submissionId",
        PropertyPeriodicSubmissionRequest(
          None,
          None,
          None,
          None,
          Some(
            UkOtherProperty(
              saveIncomeRequest.ukOtherPropertyIncome,
              UkOtherPropertyExpenses(
                Some(0),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
              )
            )
          )
        ),
        Right("")
      )

      mockPersistAnswers(
        ctx,
        Income(
          isNonUKLandlord = true,
          50,
          leasePremiumPayment = true,
          ReversePremiumsReceived(true),
          Some(DeductingTax(false)),
          Some(CalculatedFigureYourself(false)),
          Some(5),
          Some(PremiumsGrantLease(true))
        )
      )
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.updateIncome(taxYear, incomeSourceId, nino, SubmissionId("submissionId"))(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.updateIncome(taxYear, incomeSourceId, nino, SubmissionId(""))(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
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
      val result = underTest.updateIncome(taxYear, incomeSourceId, nino, SubmissionId(""))(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "update sba section" should {
    val validRequestBody: JsValue = Json.parse(
      """{
        | "claimStructureBuildingAllowance" : true,
        | "sbas": [
        |            {
        |                "sbaQualifyingDate" : "2020-04-04",
        |                "sbaQualifyingAmount" : 12,
        |                "sbaClaim" : 43,
        |                "sbaAddress" : {
        |                    "buildingName" : "name12",
        |                    "buildingNumber" : "123",
        |                    "postCode" : "XX1 1XX"
        |                }
        |            },
        |            {
        |                "sbaQualifyingDate" : "2023-01-22",
        |                "sbaQualifyingAmount" : 535,
        |                "sbaClaim" : 54,
        |                "sbaAddress" : {
        |                    "buildingName" : "235",
        |                    "buildingNumber" : "3",
        |                    "postCode" : "XX1 1XX"
        |                }
        |            },
        |            {
        |                "sbaQualifyingDate" : "2024-02-12",
        |                "sbaQualifyingAmount" : 22,
        |                "sbaClaim" : 23,
        |                "sbaAddress" : {
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
          SbaClaims(false)
        )
      )
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveSba(taxYear, incomeSourceId, nino)(request))
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
        val result = await(underTest.saveSba(taxYear, incomeSourceId, nino)(request))
        result.header.status shouldBe expectedError
      }
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.updateIncome(taxYear, incomeSourceId, nino, SubmissionId(""))(fakePostRequest)
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
        Some(EsbaInfo(
          ClaimEnhancedStructureBuildingAllowance(true),
          EsbaClaims(true),
          List()
        ))
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
