import connectors.ConnectorIntegrationTest
import models.request.PropertyPeriodicSubmissionRequest
import models.responses.{AnnualUkOtherProperty, Esba, PropertyAnnualSubmission, StructuredBuildingAllowanceBuilding, StructuredBuildingAllowanceDate, UkOtherAllowances}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.http.Status.OK
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId}

import java.time.{LocalDate, LocalDateTime}
import support.stubs.AuthStub._
class JourneyAnswersIntegrationSpec
  extends AnyWordSpec
    with Matchers
    with ScalaFutures
    with IntegrationPatience
    with GuiceOneServerPerSuite
    with ConnectorIntegrationTest {

  private val nino = "some-nino"
  private val taxableEntityId = "some-taxable-entity-id"
  private val incomeSourceId = "some-income-source-id"
  private val submissionId = "some-submission-id"
  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))


  val validPropertyPeriodicSubmissionRequest = PropertyPeriodicSubmissionRequest(
    None,
    None,
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
    None, None, None, Some(AnnualUkOtherProperty(None, Some(UkOtherAllowances(
      None, None, None, None, None, None, None, Some(
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
      None,
      None
    )
    )
    )
    )
  )

  val httpResponse = HttpResponse(OK, Json.toJson(aPropertyAnnualSubmission).toString())

  val taxYear = 2021
  private val wiremockPort = 11111

  override def fakeApplication(): Application = GuiceApplicationBuilder()
    .configure("microservice.services.integration-framework.host" -> "localhost")
    .configure("microservice.services.integration-framework.port" -> wiremockPort)
    .configure("microservice.services.auth.port" -> wiremockPort)
    .configure("integration-framework.host" -> "localhost")
    .configure("integration-framework.port" -> wiremockPort)
    .build()

  private val baseUrl  = s"http://localhost:$port/income-tax-property/property/2024/income/AC180000A/income-source-1"
  val NinoUser =
    """
      |{
      |	"nino": "AC100000B",
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
    "get error when downstream returns error" in {
      userLoggedInITPUser(NinoUser)
      stubGetHttpClientCall(s"/income-tax/business/property/annual\\?" +
        s"taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId", httpResponse)
      val wsClient = app.injector.instanceOf[WSClient]
      val requestHeaders  = Map("Content-Type" -> "application/json", "Authorization" -> "Bearer 123", "mtditid" -> "1234567890")

      val response =
        wsClient
          .url(s"$baseUrl")
          .addHttpHeaders(requestHeaders.toSeq: _*)
          .get()
          .futureValue

      response.status shouldBe 500
    }
  }
}
