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
import models.domain._
import models.errors.{RepositoryError, ServiceError}
import models.request.esba.EsbaInfo
import models.request.foreign.{ForeignPropertySelectCountry, TotalIncome}
import org.apache.pekko.util.Timeout
import org.scalatest.time.{Millis, Span}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsString, JsValue, Json}
import play.api.test.Helpers._
import utils.ControllerUnitTest
import utils.mocks.{MockAuthorisedAction, MockMongoJourneyAnswersRepository, MockPropertyService}
import utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global


class PropertyControllerSpec
  extends ControllerUnitTest with MockPropertyService with MockMongoJourneyAnswersRepository with MockAuthorisedAction
    with FakeRequestProvider with ScalaCheckPropertyChecks {

  private val underTest = new PropertyController(
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

    "should return no_content for valid request body where a field named status is present in the body request" in {

      mockAuthorisation()

      val request = fakePostRequest.withJsonBody(journeyStatusJs)
      val result =
        await(underTest.setStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), "rent-a-room-expenses")(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request when a field named status is not present in the request body" in {
      mockAuthorisation()
      val request = fakePostRequest.withJsonBody(journeyStatusErrorJs)
      val result =
        await(underTest.setStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), "rent-a-room-expenses")(request))
      result.header.status shouldBe BAD_REQUEST
    }

    "should return bad request when no JSON is provided" in {
      mockAuthorisation()
      val request = fakePostRequest
      val result =
        await(underTest.setStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), "rent-a-room-expenses")(request))
      result.header.status shouldBe BAD_REQUEST
    }
  }

  "Update journey status for foreign property" should {
    val countryCode = "ESP"
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

    "should return no_content for valid request body where a field named status is present in the body request" in {

      mockAuthorisation()

      val request = fakePostRequest.withJsonBody(journeyStatusJs)
      val result =
        await(underTest.setForeignStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), JourneyName.ForeignPropertyExpenses.entryName, countryCode)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request when a field named status is not present in the request body" in {
      mockAuthorisation()
      val request = fakePostRequest.withJsonBody(journeyStatusErrorJs)
      val result =
        await(underTest.setForeignStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), JourneyName.ForeignPropertyExpenses.entryName, countryCode)(request))
      result.header.status shouldBe BAD_REQUEST
    }

    "should return bad request when no JSON is provided" in {
      mockAuthorisation()
      val request = fakePostRequest
      val result =
        await(underTest.setForeignStatus(TaxYear(2023), IncomeSourceId("incomeSourceId"), JourneyName.ForeignPropertyExpenses.entryName, countryCode)(request))
      result.header.status shouldBe BAD_REQUEST
    }
  }

  "fetch merged property data" should {
    "return success when service returns success " in {
      mockAuthorisation()
      val uKPropertyData = FetchedUKPropertyData(
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        Some(
          EsbaInfo(
            claimEnhancedStructureBuildingAllowance = true,
            enhancedStructureBuildingAllowanceClaims = Some(true),
            List()
          )
        ),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        List(),
        Some(ForeignPropertySelectCountry(TotalIncome.Under, Some(false), None, None, None))
      )
      val foreignPropertyData = FetchedForeignPropertyData(
        None,
        None,
        None,
        None,
        None,
        None,
        None
      )
      val ukAndForeignPropertyData = FetchedUkAndForeignPropertyData(
        None
      )
      val foreignIncomeData = FetchedForeignIncomeData(None, List())
      val resultFromService = FetchedData(
        propertyData = FetchedPropertyData(
          ukPropertyData = uKPropertyData,
          foreignPropertyData = foreignPropertyData,
          ukAndForeignPropertyData = ukAndForeignPropertyData
        ),
        incomeData = foreignIncomeData
      )
      mockGetFetchedPropertyDataMerged(taxYear, incomeSourceId, mtditid, resultFromService.asRight[ServiceError])
      val result = underTest.fetchPropertyData(taxYear, nino, incomeSourceId)(fakeGetRequest)

      status(result) shouldBe 200

      val timeout: Timeout = Timeout(Span(250, Millis))
      contentAsJson(result)(timeout) shouldBe Json.toJson(resultFromService)
    }
    "return failure when service returns failure " in {
      mockAuthorisation()
      mockGetFetchedPropertyDataMerged(taxYear, incomeSourceId, mtditid, RepositoryError.asLeft[FetchedData])
      val result = await(underTest.fetchPropertyData(taxYear, nino, incomeSourceId)(fakeGetRequest))
      result.header.status shouldBe 500
    }
  }

}
