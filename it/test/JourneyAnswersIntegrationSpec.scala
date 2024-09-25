/*
 * Copyright 2024 HM Revenue & Customs
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

import connectors.ConnectorIntegrationTest
import models.request._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba._
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.http.Status.{NO_CONTENT, OK}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import support.stubs.AuthStub._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId}

import java.time.{LocalDate, LocalDateTime}
class JourneyAnswersIntegrationSpec
    extends AnyWordSpec with Matchers with ScalaFutures with IntegrationPatience with GuiceOneServerPerSuite
    with ConnectorIntegrationTest {

  private val nino = "some-nino"
  private val taxableEntityId = "some-taxable-entity-id"
  private val incomeSourceId = "some-income-source-id"
  private val submissionId = "some-submission-id"
  private val submissionId1 = "some-submission-id-1"
  private val submissionId2 = "some-submission-id-2"
  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))

  val validCreatePropertyPeriodicSubmissionRequest = CreatePropertyPeriodicSubmissionRequest(
    LocalDate.now(),
    LocalDate.now(),
    None,
    None
  )

  val esbaDate = LocalDate.parse("2024-01-01")
  val qualifyingAmountExpenditure = 35
  val amount = 25
  val address1 = "name1"
  val address2 = "name2"
  val postcode = "AB1 XY2"
  val aPropertyAnnualSubmission = PropertyAnnualSubmission(
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
            Some(UkRentARoom(true))
          )
        ),
        Some(
          UkOtherAllowances(
            Some(1),
            Some(2),
            Some(3),
            Some(4),
            Some(5),
            Some(6),
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

  val httpResponse = HttpResponse(OK, Json.toJson(aPropertyAnnualSubmission).toString())
  val httpResponseOk = HttpResponse(NO_CONTENT, "")
  val taxYear = 2021
  private val wiremockPort = 11111

  override def fakeApplication(): Application = GuiceApplicationBuilder()
    .configure("microservice.services.integration-framework.host" -> "localhost")
    .configure("microservice.services.integration-framework.port" -> wiremockPort)
    .configure("microservice.services.auth.port" -> wiremockPort)
    .configure("integration-framework.host" -> "localhost")
    .configure("integration-framework.port" -> wiremockPort)
    .build()

  private val baseUrl =
    s"http://localhost:$port/income-tax-property/property/2021/income/$taxableEntityId/$incomeSourceId"
  val NinoUser =
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
        PeriodicSubmissionIdModel(submissionId1, LocalDate.parse("2020-01-01"), LocalDate.parse("2021-11-11")),
        PeriodicSubmissionIdModel(submissionId2, LocalDate.parse("2021-02-02"), LocalDate.parse("2022-12-12"))
      )

      val httpResponsePeriodicSubmissionIdModel = HttpResponse(OK, Json.toJson(aPeriodicSubmissionModel).toString())

      stubGetHttpClientCall(
        s"/income-tax/business/property/$taxableEntityId/$incomeSourceId/period\\?taxYear=2020-21",
        httpResponsePeriodicSubmissionIdModel
      )
      val aPropertyPeriodicSubmission = PropertyPeriodicSubmission(
        None,
        submittedOn = Some(LocalDateTime.now),
        fromDate = LocalDate.now.minusDays(1),
        toDate = LocalDate.now,
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

      val fetchedPropertyData = response.json.as[FetchedPropertyData]
      response.status shouldBe 200
      fetchedPropertyData shouldBe FetchedPropertyData(
        capitalAllowancesForACar = None,
        propertyAbout = None,
        propertyRentalsAbout = None,
        adjustments = Some(
          PropertyRentalAdjustments(
            23,
            BalancingCharge(true, Some(32)),
            None,
            RenovationAllowanceBalancingCharge(true, Some(14)),
            21,
            Some(34.56)
          )
        ),
        allowances = Some(
          RentalAllowances(
            Some(1),
            Some(ElectricChargePointAllowance(true, Some(6))),
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
            esbaClaims = Some(false),
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
            false,
            3,
            12,
            Some(DeductingTax(true, Some(11))),
            None,
            None,
            None,
            Some(PremiumsGrantLease(true, Some(1))),
            Some(ReversePremiumsReceived(true, Some(2)))
          )
        ),
        propertyRentalsExpenses = Some(
          PropertyRentalsExpense(
            Some(ConsolidatedExpenses(true, Some(25))),
            Some(1),
            Some(2),
            Some(3),
            Some(11),
            Some(13),
            Some(12),
            Some(14)
          )
        ),
        raRAbout = Some(RaRAbout(true, 7, ClaimExpensesOrRelief(true, Some(44)))),
        rarExpenses = Some(
          RentARoomExpenses(
            Some(ConsolidatedExpenses(true, Some(25))),
            Some(1),
            Some(2),
            Some(11),
            Some(13),
            Some(14)
          )
        ),
        raRAdjustments = Some(RaRAdjustments(Some(BalancingCharge(true, Some(32))), Some(34.56))),
        rentARoomAllowances = Some(
          RentARoomAllowances(
            Some(CapitalAllowancesForACar(true, Some(4))),
            Some(6),
            Some(7),
            Some(2),
            Some(5),
            Some(4)
          )
        )
      )
    }
    "the downstream receives the expected payload when upload happens" in {

      val rentARoomAdjustments = RaRAdjustments(
        Some(
          BalancingCharge(
            true,
            Some(12.34)
          )
        ),
        Some(34.56)
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
          .toJson(PropertyAnnualSubmission.fromRaRAdjustments(aPropertyAnnualSubmission, rentARoomAdjustments))
          .toString(),
        httpResponseOk
      )
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel(submissionId1, LocalDate.parse("2020-01-01"), LocalDate.parse("2021-11-11")),
        PeriodicSubmissionIdModel(submissionId2, LocalDate.parse("2021-02-02"), LocalDate.parse("2022-12-12"))
      )

      val httpResponsePeriodicSubmissionIdModel = HttpResponse(OK, Json.toJson(aPeriodicSubmissionModel).toString())

      stubGetHttpClientCall(
        s"/income-tax/business/property/$taxableEntityId/$incomeSourceId/period\\?taxYear=2020-21",
        httpResponsePeriodicSubmissionIdModel
      )
      val aPropertyPeriodicSubmission = PropertyPeriodicSubmission(
        None,
        submittedOn = Some(LocalDateTime.now),
        fromDate = LocalDate.now.minusDays(1),
        toDate = LocalDate.now,
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
        s"http://localhost:$port/income-tax-property/property/2021/$incomeSourceId/rent-a-room-adjustments/$taxableEntityId/answers"

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
