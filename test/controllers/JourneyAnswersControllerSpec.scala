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

import models.common.JourneyName.About
import models.common._
import models.request._
import models.responses.{PeriodicSubmissionId, PropertyPeriodicSubmission, UkOtherProperty, UkOtherPropertyExpenses, UkOtherPropertyIncome, UkPropertyExpenses}
import play.api.http.Status.{BAD_REQUEST, CREATED, NO_CONTENT}
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers.status
import models.request.{ElectricChargePointAllowance, PropertyAbout, RentalAllowances}
import models.common.{BusinessId, JourneyContext, JourneyContextWithNino, Mtditid, Nino, TaxYear}
import utils.ControllerUnitTest
import utils.mocks.{MockAuthorisedAction, MockPropertyService}
import utils.providers.FakeRequestProvider

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class JourneyAnswersControllerSpec extends ControllerUnitTest
  with MockPropertyService
  with MockAuthorisedAction
  with FakeRequestProvider {


  private val underTest = new JourneyAnswersController(
    mockPropertyService,
    mockAuthorisedAction,
    cc
  )

  val taxYear: TaxYear = TaxYear(2024)
  val businessId: BusinessId = BusinessId("someBusinessId")
  val incomeSourceId = IncomeSourceId("incomeSourceId")
  val incomeSubmissionId = SubmissionId("submissionId")
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
    val ctx: JourneyContext = JourneyContextWithNino(taxYear, businessId, mtditid, nino).toJourneyContext(About)


    "should return no_content for valid request body" in {

      mockAuthorisation()
      mockPersistAnswers(ctx, PropertyAbout("over", Seq("property.rentals"), Some(true)))
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyAbout(taxYear, businessId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyAbout(taxYear, businessId, nino)(fakePostRequest)
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
    val ctx = JourneyContextWithNino(taxYear, businessId, mtditid, nino)


    "should return no_content for valid request body" in {

      mockAuthorisation()
      mockSavePropertyRentalAllowances(ctx, RentalAllowances(
        Some(11),
        ElectricChargePointAllowance(electricChargePointAllowanceYesNo = true, Some(11)),
        Some(11),
        Some(11),
        Some(11),
        Some(11),
        Some(11)
      ))
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyRentalAllowances(taxYear, businessId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyAbout(taxYear, businessId, nino)(fakePostRequest)
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

    val ctx: JourneyContext = JourneyContextWithNino(taxYear, businessId, mtditid, nino).toJourneyContext(About)


    "should return created for valid request body" in {

      mockAuthorisation()
      val saveIncomeRequest = validRequestBody.as[SaveIncome]
      mockCreatePeriodicSubmissions(
        nino.value,
        "incomeSourceId",
        taxYear.endYear,
        Some(Json.toJson(
          PropertyPeriodicSubmission(
            None,
            LocalDate.now(),
            LocalDate.now(),
            None,
            None,
            None,
            Some(
              UkOtherProperty(
                saveIncomeRequest.ukOtherPropertyIncome,
                UkPropertyExpenses(
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
          )
        )
        ), Right(PeriodicSubmissionId("submissionId")))

      mockPersistAnswers(ctx, Income(
        true,
        50,
        true,
        ReversePremiumsReceived(true),
        Some(DeductingTax(false)),
        Some(CalculatedFigureYourself(false)),
        Some(5),
        Some(PremiumsGrantLease(true))
      ))
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.saveIncome(taxYear, businessId, nino, IncomeSourceId("incomeSourceId"))(request))
      result.header.status shouldBe CREATED
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveIncome(taxYear, businessId, nino, IncomeSourceId(""))(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
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

    val ctx: JourneyContext = JourneyContextWithNino(taxYear, businessId, mtditid, nino).toJourneyContext(About)


    "should return no_content for valid request body" in {

      mockAuthorisation()
      val saveIncomeRequest = validRequestBody.as[SaveIncome]
      mockUpdatePeriodicSubmissions(
        nino.value,
        "incomeSourceId",
        taxYear.endYear,
        "submissionId",
        Some(Json.toJson(
          PropertyPeriodicSubmission(
            None,
            LocalDate.now(),
            LocalDate.now(),
            None,
            None,
            None,
            Some(
              UkOtherProperty(
                saveIncomeRequest.ukOtherPropertyIncome,
                UkPropertyExpenses(
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
          )
        )
        ), Right(""))

      mockPersistAnswers(ctx, Income(
        true,
        50,
        true,
        ReversePremiumsReceived(true),
        Some(DeductingTax(false)),
        Some(CalculatedFigureYourself(false)),
        Some(5),
        Some(PremiumsGrantLease(true))
      ))
      val request = fakePutRequest.withJsonBody(validRequestBody)
      val result = await(underTest.updateIncome(taxYear, businessId, nino, IncomeSourceId("incomeSourceId"), SubmissionId("submissionId"))(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.updateIncome(taxYear, businessId, nino, IncomeSourceId(""), SubmissionId(""))(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

  "create or update property expenses section" should {

    val createOrUpdateUIRequest: JsValue = Json.parse(
      """{
        |    "RentsRatesAndInsurance": 100,
        |    "RepairsAndMaintenanceCosts": 200,
        |    "loanInterest": 300,
        |    "otherProfessionalFee": 400,
        |    "costsOfServicesProvided": 500,
        |    "otherAllowablePropertyExpenses": 600,
        |    "propertyBusinessTravelCost": 700
        |}""".stripMargin)

    val createOrUpdateRequestBody = createOrUpdateUIRequest.as[Expenses]
    import createOrUpdateRequestBody._

    "should return created for valid request body" in {

      mockAuthorisation()
      mockCreatePeriodicSubmissions(
        nino.value,
        "incomeSourceId",
        taxYear.endYear,
        Some(Json.toJson(
          PropertyPeriodicSubmission(
            None,
            LocalDate.now(),
            LocalDate.now(),
            None,
            None,
            None,
            Some(
              UkOtherProperty(
                UkOtherPropertyIncome(
                  None,
                  None,
                  None,
                  None,
                  None,
                  None
                ),
                UkPropertyExpenses(
                  premisesRunningCosts = RentsRatesAndInsurance,
                  repairsAndMaintenance = RepairsAndMaintenanceCosts,
                  financialCosts = loanInterest,
                  professionalFees = otherProfessionalFee,
                  travelCosts = propertyBusinessTravelCost,
                  costOfServices = costsOfServicesProvided,
                  other = otherAllowablePropertyExpenses,
                  None
                )
              )
            )
          )
        )
        ), Right(PeriodicSubmissionId("submissionId")))

      val request = fakePostRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.saveExpenses(taxYear, businessId, nino, incomeSourceId)(request))
      result.header.status shouldBe CREATED
    }

    "should return no_content for valid request body" in {
      mockAuthorisation()
      mockUpdatePeriodicSubmissions(
        nino.value,
        incomeSourceId.value,
        taxYear.endYear,
        incomeSubmissionId.value,
        Some(Json.toJson(
          PropertyPeriodicSubmission(
            None,
            LocalDate.now(),
            LocalDate.now(),
            None,
            None,
            None,
            Some(
              UkOtherProperty(
                UkOtherPropertyIncome(
                  None,
                  None,
                  None,
                  None,
                  None,
                  None
                ),
                UkPropertyExpenses(
                  premisesRunningCosts = RentsRatesAndInsurance,
                  repairsAndMaintenance = RepairsAndMaintenanceCosts,
                  financialCosts = loanInterest,
                  professionalFees = otherProfessionalFee,
                  travelCosts = propertyBusinessTravelCost,
                  costOfServices = costsOfServicesProvided,
                  other = otherAllowablePropertyExpenses,
                  None
                )
            )
          )
        ))), Right(""))

      val request = fakePutRequest.withJsonBody(createOrUpdateUIRequest)
      val result = await(underTest.updateExpenses(taxYear, businessId, nino, incomeSourceId, incomeSubmissionId)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.saveExpenses(taxYear, businessId, nino, incomeSourceId)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }
}