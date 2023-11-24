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

package uk.gov.hmrc.incometaxproperty.connectors

import org.scalamock.scalatest.MockFactory
import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId}
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiError, SingleErrorBody}
import uk.gov.hmrc.incometaxproperty.models.responses.{PeriodicSubmissionIdModel, PeriodicSubmissionModel}
import uk.gov.hmrc.incometaxproperty.utils.builders.IncomeSourceDetailsBuilder.anIncomeSourceDetails

import scala.concurrent.ExecutionContext.Implicits.global

class IntegrationFrameworkConnectorISpec extends ConnectorIntegrationTest
  with MockFactory {

  private val nino = "some-nino"
  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))

  private val underTest = new IntegrationFrameworkConnector(httpClient, appConfigStub)

  ".getBusinessDetails" when {
    "when we call the IF" should {
      "return correct IF data when correct parameters are passed" in {

        val httpResponse = HttpResponse(OK, Json.toJson(anIncomeSourceDetails).toString())

        stubGetHttpClientCall(s"/registration/business-details/nino/$nino", httpResponse)

        await(underTest.getBusinessDetails(nino)(hc)) shouldBe Right(Some(anIncomeSourceDetails))
      }

      "return IF error when Left is returned" in {
        val httpResponse = HttpResponse(INTERNAL_SERVER_ERROR, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())

        stubGetHttpClientCall(s"/registration/business-details/nino/$nino", httpResponse)

        await(underTest.getBusinessDetails(nino)(hc)) shouldBe
          Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("some-code", "some-reason")))
      }
    }
  }


  val aPeriodicSubmissionModel: PeriodicSubmissionModel = PeriodicSubmissionModel(List(
    PeriodicSubmissionIdModel("1", "2021-01-01", "2021-11-11"),
    PeriodicSubmissionIdModel("2", "2022-02-02", "2022-12-12")
  ))

  "Given a need to get Periodic Submission Data" when {
    "a call is made to the backend API it" should {

      "return correct submissions data for the APIs used before 2024" in {
        val httpResponse = HttpResponse(OK, Json.toJson(aPeriodicSubmissionModel).toString())

        val taxYear = 2021
        val taxableEntityId = "some-taxable-entity-id"
        val incomeSourceId = "some-income-source-id"

        stubGetHttpClientCall(s"/income-tax/business/property/$taxableEntityId/$incomeSourceId/period\\?taxYear=2020-21", httpResponse)

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Right(aPeriodicSubmissionModel)
      }

      "return correct submissions data for 2024 onwards" in {

        val httpResponse = HttpResponse(OK, Json.toJson(aPeriodicSubmissionModel).toString())

        val taxYear = 2024
        val taxableEntityId = "some-taxable-entity-id"
        val incomeSourceId = "some-income-source-id"

        stubGetHttpClientCall(s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/period", httpResponse)

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Right(aPeriodicSubmissionModel)
      }

      "return Data Not Found from Upstream" in {
        val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(List.empty[PeriodicSubmissionModel]).toString())

        val taxYear = 2024
        val taxableEntityId = "some-taxable-entity-id"
        val incomeSourceId = "some-income-source-id"

        stubGetHttpClientCall(s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/period", httpResponse)

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Right(PeriodicSubmissionModel(List.empty))
      }

      "return Service Unavailable Error from Upstream" in {
        val httpResponse = HttpResponse(SERVICE_UNAVAILABLE, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())

        val taxYear = 2024
        val taxableEntityId = "some-taxable-entity-id"
        val incomeSourceId = "some-income-source-id"

        stubGetHttpClientCall(s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/period", httpResponse)

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Left(ApiError(
          SERVICE_UNAVAILABLE,
          SingleErrorBody("GetPeriodicSubmissionResponse", "{\"code\":\"some-code\",\"reason\":\"some-reason\"}")))
      }

      "handle Any Other Error from Upstream" in {
        val httpResponse = HttpResponse(BAD_GATEWAY, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())

        val taxYear = 2024
        val taxableEntityId = "some-taxable-entity-id"
        val incomeSourceId = "some-income-source-id"

        stubGetHttpClientCall(s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/period", httpResponse)

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Left(ApiError(
          BAD_GATEWAY,
          SingleErrorBody("GetPeriodicSubmissionResponse", "{\"code\":\"some-code\",\"reason\":\"some-reason\"}")))
      }
    }
  }
}
