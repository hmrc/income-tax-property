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

import config.AppConfig
import connectors.ConnectorIntegrationSpec
import models.LossType.UKProperty
import models.RentalsAndRaRAbout
import models.common.TaxYear
import models.common.TaxYear.asTyBefore24
import models.domain._
import models.request.WhenYouReportedTheLoss.y2018to2019
import models.request._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba._
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import org.scalatest.concurrent.{ScalaFutures, IntegrationPatience}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.http.Status.{NO_CONTENT, OK}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import repositories.MongoJourneyAnswersRepository
import support.stubs.AuthStub._
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.mongo.test.CleanMongoCollectionSupport
import utils.AppConfigStub

import java.time._
import java.time.temporal.ChronoUnit
import scala.concurrent.ExecutionContext.Implicits.global

class JourneyAnswersIntegrationSpec
    extends AnyWordSpec with Matchers with ScalaFutures with IntegrationPatience with GuiceOneServerPerSuite
    with ConnectorIntegrationSpec with CleanMongoCollectionSupport  {

  private val taxableEntityId = "some-taxable-entity-id"
  private val incomeSourceId = "some-income-source-id"

  private val submissionId1 = "some-submission-id-1"
  private val submissionId2 = "some-submission-id-2"

  val validCreateUKPropertyPeriodicSubmissionRequest: CreateUKPropertyPeriodicSubmissionRequest =
    CreateUKPropertyPeriodicSubmissionRequest(
      LocalDate.now(),
      LocalDate.now(),
      None
    )

  val esbaDate: LocalDate = LocalDate.parse("2024-01-01")
  val qualifyingAmountExpenditure = 35
  val amount = 25
  val address1 = "name1"
  val address2 = "name2"
  val postcode = "AB1 XY2"
  val aPropertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(
    submittedOn = Some(LocalDateTime.now),
    None,
    Some(
      AnnualUkOtherProperty(
        Some(
          UkOtherAdjustments(
            Some(41),
            Some(32),
            Some(23),
            Some(14),
            Some(true),
            Some(UkRentARoom(true)),
            None,
            Some(WhenYouReportedTheLoss.y2018to2019)
          )
        ),
        Some(
          UkOtherAllowances(
            Some(1),
            Some(2),
            Some(3),
            Some(4),
            Some(5),
            None,
            None,
            Some(
              Seq(
                Esba(
                  amount,
                  Some(
                    StructuredBuildingAllowanceDate(esbaDate, qualifyingAmountExpenditure)
                  ),
                  StructuredBuildingAllowanceBuilding(
                    Some(address1),
                    Some(address2),
                    postcode
                  )
                )
              )
            ),
            Some(7),
            Some(8)
          )
        )
      )
    )
  )

  val httpResponse: HttpResponse = HttpResponse(OK, Json.toJson(aPropertyAnnualSubmission).toString())
  val httpResponseOk: HttpResponse = HttpResponse(NO_CONTENT, "")
  val taxYear = 2021
  private val wiremockPort = 11111
  val mockAppConfig: AppConfig = new AppConfigStub().config()
  private val instant = Instant.now.truncatedTo(ChronoUnit.MILLIS)
  private val stubClock: Clock = Clock.fixed(instant, ZoneId.systemDefault)
  override def fakeApplication(): Application = GuiceApplicationBuilder()
    .configure("microservice.services.integration-framework.host" -> "localhost")
    .configure("microservice.services.integration-framework.port" -> wiremockPort)
    .configure("microservice.services.auth.port" -> wiremockPort)
    .configure("integration-framework.host" -> "localhost")
    .configure("integration-framework.port" -> wiremockPort)
    .overrides(
      bind[MongoJourneyAnswersRepository].toInstance(new MongoJourneyAnswersRepository(mongoComponent, mockAppConfig, stubClock))
    )
    .build()

  private val baseUrl =
    s"http://localhost:$port/income-tax-property/property/2021/income/$taxableEntityId/$incomeSourceId"
  val NinoUser: String =
    """
      |{
      |	"nino": "taxableEntityId",
      |	"affinityGroup": "Individual",
      |	"internalId": "Int-8612ba91-5581-411d-9d32-fb2de937a565",
      | "confidenceLevel": 250,
      | "allEnrolments": [
      |  {
      |     "key": "HMRC-MTD-IT",
      |     "identifiers": [{
      |       "key": "MTDITID",
      |       "value": "1234567890"
      |      }],
      |     "state": "Activated"
      |  },
      |  {
      |     "key": "HMRC-NI",
      |     "identifiers": [{
      |       "key": "NINO",
      |       "value": "1234567890"
      |      }],
      |     "state": "Activated"
      |  }
      | ]
      |}
      |""".stripMargin

  "Income Tax Property" should {
    "get success when downstream returns success" in {
      userLoggedInITPUser(NinoUser)
      // Annual
      stubGetHttpClientCall(
        s"/income-tax/business/property/annual\\?" +
          s"taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId",
        httpResponse
      )
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel(submissionId1, LocalDate.parse("2020-04-06"), LocalDate.parse("2021-04-05")),
        PeriodicSubmissionIdModel(submissionId2, LocalDate.parse("2021-04-06"), LocalDate.parse("2022-04-05"))
      )

      val httpResponsePeriodicSubmissionIdModel = HttpResponse(OK, Json.toJson(aPeriodicSubmissionModel).toString())

      stubGetHttpClientCall(
        s"/income-tax/business/property/$taxableEntityId/$incomeSourceId/period\\?taxYear=2020-21",
        httpResponsePeriodicSubmissionIdModel
      )
      val aPropertyPeriodicSubmission = PropertyPeriodicSubmission(
        None,
        submittedOn = Some(LocalDateTime.now),
        fromDate = TaxYear.startDate(2021),
        toDate = TaxYear.endDate(2021),
        None,
        Some(
          UkOtherProperty(
            Some(
              UkOtherPropertyIncome(
                Some(1),
                Some(2),
                Some(3),
                Some(11),
                Some(12),
                Some(
                  RentARoomIncome(
                    7
                  )
                )
              )
            ),
            Some(
              UkOtherPropertyExpenses(
                Some(1),
                Some(2),
                Some(3),
                Some(11),
                Some(12),
                Some(13),
                Some(14),
                Some(21),
                Some(34.56),
                Some(
                  UkRentARoomExpense(
                    44
                  )
                ),
                Some(25)
              )
            )
          )
        )
      )
      val httpResponsePeriodicSubmission = HttpResponse(OK, Json.toJson(aPropertyPeriodicSubmission).toString())

      stubGetHttpClientCall(
        s"/income-tax/business/property/periodic\\?" +
          s"taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId&submissionId=$submissionId1",
        httpResponsePeriodicSubmission
      )
      val wsClient = app.injector.instanceOf[WSClient]
      val requestHeaders =
        Map("Content-Type" -> "application/json", "Authorization" -> "Bearer 123", "mtditid" -> "1234567890")

      val response =
        wsClient
          .url(s"$baseUrl")
          .addHttpHeaders(requestHeaders.toSeq: _*)
          .get()
          .futureValue

      val fetchedPropertyData = response.json.as[FetchedData]
      response.status shouldBe 200
      fetchedPropertyData shouldBe
        FetchedData(
          propertyData = FetchedPropertyData(
        foreignPropertyData = FetchedForeignPropertyData(None, None, None, None,None,None, None),
        ukPropertyData = FetchedUKPropertyData(
          capitalAllowancesForACar = None,
          propertyAbout = None,
          propertyRentalsAbout = None,
          adjustments = Some(
            PropertyRentalAdjustments(
              23,
              BalancingCharge(isBalancingCharge = true, Some(32)),
              Some(0),
              RenovationAllowanceBalancingCharge(isRenovationAllowanceBalancingCharge = true, Some(14)),
              21,
              Some(34.56),
              UnusedLossesBroughtForward(isUnusedLossesBroughtForward = true, Some(41)),
              Some(y2018to2019)
            )
          ),
          allowances = Some(
            RentalAllowances(
              Some(CapitalAllowancesForACar(isCapitalAllowancesForACar = true, Some(4))),
              Some(1),
              Some(7),
              Some(2),
              Some(3),
              Some(5),
              Some(4)
            )
          ),
          esbasWithSupportingQuestions = Some(
            EsbaInfo(
              claimEnhancedStructureBuildingAllowance = true,
              enhancedStructureBuildingAllowanceClaims = Some(false),
              List(
                EsbaInUpstream(
                  LocalDate.parse("2024-01-01"),
                  35,
                  25,
                  Address(BuildingName("name1"), BuildingNumber("name2"), Postcode("AB1 XY2"))
                )
              )
            )
          ),
          sbasWithSupportingQuestions = None,
          propertyRentalsIncome = Some(
            PropertyRentalsIncome(
              isNonUKLandlord = false,
              3,
              12,
              Some(DeductingTax(isTaxDeducted = true, Some(11))),
              None,
              None,
              None,
              Some(PremiumsGrantLease(premiumsGrantLeaseReceived = true, Some(1))),
              Some(ReversePremiumsReceived(reversePremiumsReceived = true, Some(2)))
            )
          ),
          propertyRentalsExpenses = Some(
            PropertyRentalsExpense(
              Some(ConsolidatedExpenses(isConsolidatedExpenses = true, Some(25))),
              Some(1),
              Some(2),
              Some(3),
              Some(11),
              Some(13),
              Some(12),
              Some(14)
            )
          ),
          raRAbout = Some(
            RaRAbout(isJointlyLet = true, 7, ClaimExpensesOrRelief(isClaimExpensesOrRelief = true, Some(44)))
          ),
          rarExpenses = Some(
            RentARoomExpenses(
              Some(ConsolidatedExpenses(isConsolidatedExpenses = true, Some(25))),
              Some(1),
              Some(2),
              Some(11),
              Some(13),
              Some(14)
            )
          ),
          raRAdjustments =
            Some(RaRAdjustments(
              Some(BalancingCharge(isBalancingCharge = true, Some(32))),
              Some(34.56),
              Some(UnusedLossesBroughtForward(isUnusedLossesBroughtForward = true, Some(41))),
              Some(WhenYouReportedTheLoss.y2018to2019))
            ),
          rentARoomAllowances = Some(
            RentARoomAllowances(
              Some(CapitalAllowancesForACar(isCapitalAllowancesForACar = true, Some(4))),
              Some(7),
              Some(2),
              Some(5),
              Some(4)
            )
          ),
          rentalsAndRaRAbout = Some(
            RentalsAndRaRAbout(
              isJointlyLet = true,
              7,
              isClaimPropertyIncomeAllowance = false,
              3,
              ClaimExpensesOrRelief(isClaimExpensesOrRelief = true, Some(44))
            )
          ),
          rentalsAndRaRAdjustments = Some(
            PropertyRentalAdjustments(
              23,
              BalancingCharge(isBalancingCharge = true, Some(32)),
              None,
              RenovationAllowanceBalancingCharge(isRenovationAllowanceBalancingCharge = true, Some(14)),
              21,
              Some(34.56),
              UnusedLossesBroughtForward(isUnusedLossesBroughtForward = true, Some(41)),
              Some(y2018to2019)
            )
          ),
          rentalsAndRaRAllowances = Some(
            RentalAllowances(
              Some(CapitalAllowancesForACar(isCapitalAllowancesForACar = true, Some(4))),
              Some(1),
              Some(7),
              Some(2),
              Some(3),
              Some(5),
              Some(4)
            )
          ),
          rentalsAndRaREsbasWithSupportingQuestions = Some(
            EsbaInfo(
              claimEnhancedStructureBuildingAllowance = true,
              enhancedStructureBuildingAllowanceClaims = Some(false),
              List(
                EsbaInUpstream(
                  LocalDate.parse("2024-01-01"),
                  35,
                  25,
                  Address(BuildingName("name1"), BuildingNumber("name2"), Postcode("AB1 XY2"))
                )
              )
            )
          ),
          rentalsAndRaRSbasWithSupportingQuestions = None,
          rentalsAndRaRIncome = Some(
            RentalsAndRaRIncome(
              isNonUKLandlord = false,
              12,
              Some(DeductingTax(isTaxDeducted = true, Some(11))),
              None,
              None,
              None,
              Some(PremiumsGrantLease(premiumsGrantLeaseReceived = true, Some(1))),
              Some(ReversePremiumsReceived(reversePremiumsReceived = true, Some(2)))
            )
          ),
          rentalsAndRaRExpenses = Some(
            PropertyRentalsExpense(
              Some(ConsolidatedExpenses(isConsolidatedExpenses = true, Some(25))),
              Some(1),
              Some(2),
              Some(3),
              Some(11),
              Some(13),
              Some(12),
              Some(14)
            )
          ),
          journeyStatuses = List(),
          foreignPropertySelectCountry = None
        ),
        ukAndForeignPropertyData = FetchedUkAndForeignPropertyData(
          ukAndForeignAbout = None
        )
      ),
          incomeData = FetchedForeignIncomeData(None, List())
        )

    }
    "the downstream receives the expected payload when upload happens" in {
      val whenYouReportedTheLoss = WhenYouReportedTheLoss.y2018to2019
      val lossesBroughtForwardAmount = 22.47
      val rentARoomAdjustments = RaRAdjustments(
        Some(
          BalancingCharge(
            isBalancingCharge = true,
            Some(12.34)
          )
        ),
        Some(34.56),
        Some(UnusedLossesBroughtForward(isUnusedLossesBroughtForward = true, Some(lossesBroughtForwardAmount))),
        Some(whenYouReportedTheLoss)
      )

      userLoggedInITPUser(NinoUser)
      // Annual
      stubGetHttpClientCall(
        s"/income-tax/business/property/annual\\?" +
          s"taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId",
        httpResponse
      )

      stubPutHttpClientCall(
        s"/income-tax/business/property/annual\\?" +
          s"taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId",
        Json
          .toJson(
            PropertyAnnualSubmission
              .fromRaRAdjustments(aPropertyAnnualSubmission.copy(submittedOn = None), rentARoomAdjustments)
          )
          .toString(),
        httpResponseOk
      )

      // Brought Forward Loss
      val taxYearStr = asTyBefore24(WhenYouReportedTheLoss.toTaxYear(whenYouReportedTheLoss))
      val nino = taxableEntityId
      val businessId = incomeSourceId
      val lossId = "some-loss-id"
      val broughtForwardLossId: BroughtForwardLossId = BroughtForwardLossId(
        lossId = lossId
      )
      val broughtForwardLossRequest = BroughtForwardLossRequest(
        businessId = businessId,
        typeOfLoss = UKProperty,
        lossAmount = lossesBroughtForwardAmount,
        taxYearBroughtForwardFrom = taxYearStr
      )
      val broughtForwardLossResponse: BroughtForwardLossResponse = BroughtForwardLossResponse(
        businessId = businessId,
        typeOfLoss = UKProperty,
        lossAmount = lossesBroughtForwardAmount,
        taxYearBroughtForwardFrom = taxYearStr,
        lastModified = LocalDate.now().toString
      )
      val broughtForwardLossResponseWithId: BroughtForwardLossResponseWithId = BroughtForwardLossResponseWithId(
        lossId = broughtForwardLossId.lossId,
        businessId = broughtForwardLossResponse.businessId,
        typeOfLoss = broughtForwardLossResponse.typeOfLoss,
        lossAmount = broughtForwardLossResponse.lossAmount,
        taxYearBroughtForwardFrom = broughtForwardLossResponse.taxYearBroughtForwardFrom,
        lastModified = broughtForwardLossResponse.lastModified
      )
      val broughtForwardLossesResponse: BroughtForwardLosses = BroughtForwardLosses(losses = Seq(broughtForwardLossResponseWithId))
      val broughtForwardLossAmountRequest: BroughtForwardLossAmount = BroughtForwardLossAmount(lossAmount = lossesBroughtForwardAmount)

      val broughtForwardLossIdHttpResponse: HttpResponse = HttpResponse(OK, Json.toJson(broughtForwardLossId).toString)
      val broughtForwardLossHttpResponse: HttpResponse = HttpResponse(OK, Json.toJson(broughtForwardLossResponse).toString)
      val broughtForwardLossesHttpResponse: HttpResponse = HttpResponse(OK, Json.toJson(broughtForwardLossesResponse).toString)
      stubPostHttpClientCall(
        s"/individuals/losses/$nino/brought-forward-losses/$taxYearStr",
        Json.toJson(broughtForwardLossRequest).toString,
        broughtForwardLossIdHttpResponse
      )
      stubGetHttpClientCall(
        s"/individuals/losses/$nino/brought-forward-losses/$lossId",
        broughtForwardLossHttpResponse
      )
      stubGetHttpClientCall(
        s"/individuals/losses/$nino/brought-forward-losses/tax-year/$taxYearStr" +
      s"?businessId=$incomeSourceId&typeOfLoss=$UKProperty",
        broughtForwardLossesHttpResponse
      )
      stubPutHttpClientCall(
        s"/individuals/losses/$nino/brought-forward-losses/$lossId/tax-year/$taxYearStr/change-loss-amount",
        Json.toJson(broughtForwardLossAmountRequest).toString,
        broughtForwardLossHttpResponse
      )

      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel(submissionId1, LocalDate.parse("2020-04-06"), LocalDate.parse("2021-04-05")),
        PeriodicSubmissionIdModel(submissionId2, LocalDate.parse("2021-02-02"), LocalDate.parse("2022-12-12"))
      )

      val httpResponsePeriodicSubmissionIdModel = HttpResponse(OK, Json.toJson(aPeriodicSubmissionModel).toString())

      stubGetHttpClientCall(
        s"/income-tax/business/property/$taxableEntityId/$incomeSourceId/period\\?taxYear=2020-21",
        httpResponsePeriodicSubmissionIdModel
      )
      val aPropertyPeriodicSubmission = PropertyPeriodicSubmission(
        Some(PeriodicSubmissionId(submissionId1)),
        submittedOn = Some(LocalDateTime.now),
        fromDate = TaxYear.startDate(taxYear),
        toDate = TaxYear.endDate(taxYear),
        None,
        Some(
          UkOtherProperty(
            Some(
              UkOtherPropertyIncome(
                Some(1),
                Some(2),
                Some(3),
                Some(11),
                Some(12),
                Some(
                  RentARoomIncome(
                    7
                  )
                )
              )
            ),
            Some(
              UkOtherPropertyExpenses(
                Some(1),
                Some(2),
                Some(3),
                Some(11),
                Some(12),
                Some(13),
                Some(14),
                Some(21),
                Some(34.56),
                Some(
                  UkRentARoomExpense(
                    44
                  )
                ),
                Some(25)
              )
            )
          )
        )
      )
      val httpResponsePeriodicSubmission = HttpResponse(OK, Json.toJson(aPropertyPeriodicSubmission).toString())

      stubGetHttpClientCall(
        s"/income-tax/business/property/periodic\\?" +
          s"taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId&submissionId=$submissionId1",
        httpResponsePeriodicSubmission
      )
      val wsClient = app.injector.instanceOf[WSClient]
      val requestHeaders =
        Map("Content-Type" -> "application/json", "Authorization" -> "Bearer 123", "mtditid" -> "1234567890")
      val baseUrl =
        s"http://localhost:$port/income-tax-property/uk-property/2021/$incomeSourceId/rent-a-room-adjustments/$taxableEntityId/answers"

      val response =
        wsClient
          .url(s"$baseUrl")
          .addHttpHeaders(requestHeaders.toSeq: _*)
          .post(Json.toJson(rentARoomAdjustments))
          .futureValue
      response.status shouldBe 201

    }
  }
}
