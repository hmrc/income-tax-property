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

package connectors


import models.IncomeSourceType
import models.IncomeSourceType.UKPropertyFHL
import models.common.TaxYear.asTys
import models.common.{IncomeSourceId, Nino}
import models.errors.{ApiError, SingleErrorBody}
import models.request.WhenYouReportedTheLoss
import models.request.WhenYouReportedTheLoss.{toTaxYear, y2021to2022}
import models.request.HipPropertyBFLRequest
import models.responses.BroughtForwardLossId
import models.responses.HipPropertyBFLResponse
import org.scalamock.scalatest.MockFactory
import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId}

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class HipConnectorSpec extends ConnectorIntegrationSpec with MockFactory {

  private val nino = Nino("test-nino")
  private val lossId = "test-loss-id"
  private val incomeSourceId = IncomeSourceId("test-income-source-id")
  private val incomeSourceType: IncomeSourceType = UKPropertyFHL
  private val lossAmount: BigDecimal = BigDecimal(100.01)
  private val broughtForwardLossTaxYear: WhenYouReportedTheLoss = y2021to2022

  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))

  private val underTest = new HipConnector(httpClientV2, appConfigStub)

  ".createPropertyBroughtForwardLoss" should {
    "return the Loss ID" when {
      "successfully created Property BF Loss" in {
        val requestBody = Json.toJson(Requests.validCreateBFLRequest).toString()
        val taxYear: String = asTys(toTaxYear(broughtForwardLossTaxYear))
        val httpResponse = HttpResponse(OK, Json.toJson(BroughtForwardLossId("test-loss-id")).toString())

        stubPostHttpClientCall(
          s"/income-sources/brought-forward-losses/$nino\\?taxYear=$taxYear",
          requestBody,
          httpResponse
        )

        val expectedResult = Right(BroughtForwardLossId("test-loss-id"))

        await(
          underTest.createPropertyBroughtForwardLoss(
            nino,
            incomeSourceId,
            incomeSourceType,
            lossAmount,
            broughtForwardLossTaxYear
          )(hc)
        ) shouldBe expectedResult
      }
    }
    "return a API error from upstream" when {
      "a NOT_FOUND' error is returned from the API" in {
        val taxYear: String = asTys(toTaxYear(broughtForwardLossTaxYear))
        val requestBody = Json.toJson(Requests.validCreateBFLRequest).toString()
        val apiError = SingleErrorBody("code", "reason")
        val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(apiError).toString())

        stubPostHttpClientCall(
          s"/income-sources/brought-forward-losses/$nino\\?taxYear=$taxYear",
          requestBody,
          httpResponse
        )

        val expectedResult = Left(ApiError(NOT_FOUND, apiError))

        await(
          underTest.createPropertyBroughtForwardLoss(
            nino,
            incomeSourceId,
            incomeSourceType,
            lossAmount,
            broughtForwardLossTaxYear
          )(hc)
        ) shouldBe expectedResult
      }
      "a Service Error is returned from the API" in {
        val taxYear: String = asTys(toTaxYear(broughtForwardLossTaxYear))
        val requestBody = Json.toJson(Requests.validCreateBFLRequest).toString()
        val apiError = SingleErrorBody("code", "reason")
        val apiServiceErrors = Seq(BAD_REQUEST, UNPROCESSABLE_ENTITY, INTERNAL_SERVER_ERROR, SERVICE_UNAVAILABLE)

        apiServiceErrors.foreach { apiErrorCode =>
          val httpResponse = HttpResponse(apiErrorCode, Json.toJson(apiError).toString())

          stubPostHttpClientCall(
            s"/income-sources/brought-forward-losses/$nino\\?taxYear=$taxYear",
            requestBody,
            httpResponse
          )

          val expectedResult = Left(ApiError(apiErrorCode, apiError))

          await(
            underTest.createPropertyBroughtForwardLoss(
              nino,
              incomeSourceId,
              incomeSourceType,
              lossAmount,
              broughtForwardLossTaxYear
            )(hc)
          ) shouldBe expectedResult
        }
      }
      "another unexpected error is returned from the API" in {
        val taxYear: String = asTys(toTaxYear(broughtForwardLossTaxYear))
        val requestBody = Json.toJson(Requests.validCreateBFLRequest).toString()
        val apiError = SingleErrorBody("code", "reason")
        val httpResponse = HttpResponse(INSUFFICIENT_STORAGE, Json.toJson(apiError).toString())

        stubPostHttpClientCall(
          s"/income-sources/brought-forward-losses/$nino\\?taxYear=$taxYear",
          requestBody,
          httpResponse
        )
        val expectedResult = Left(ApiError(INTERNAL_SERVER_ERROR, apiError))
        await(
          underTest.createPropertyBroughtForwardLoss(
            nino,
            incomeSourceId,
            incomeSourceType,
            lossAmount,
            broughtForwardLossTaxYear
          )(hc)
        ) shouldBe expectedResult

      }
    }
  }

  ".getPropertyBroughtForwardLoss" should {
    "return a Brought Forward Loss response" when {
      "the loss ID is found" in {

        val httpResponse = HttpResponse(OK, Json.toJson(Responses.hipPropertyBFLResponse).toString())

        stubGetHttpClientCall(
          s"/income-sources/brought-forward-losses/$nino/$lossId",
          httpResponse
        )

        await(
          underTest.getPropertyBroughtForwardLoss(
            nino,
            lossId
          )(hc)
        ) shouldBe Right(Responses.hipPropertyBFLResponse)
      }
    }

    "return an API error from upstream" when {
      "a NOT_FOUND' error is returned from the API" in {
        val apiError = SingleErrorBody("code", "reason")
        val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(apiError).toString())
        stubGetHttpClientCall(
          s"/income-sources/brought-forward-losses/$nino/$lossId",
          httpResponse
        )
        val expectedResult = Left(ApiError(NOT_FOUND, apiError))
        await(
          underTest.getPropertyBroughtForwardLoss(
            nino,
            lossId
          )(hc)
        ) shouldBe expectedResult
      }
      "a Service Error is returned from the API" in {
        val apiError = SingleErrorBody("code", "reason")
        val apiServiceErrors = Seq(BAD_REQUEST, UNPROCESSABLE_ENTITY, INTERNAL_SERVER_ERROR, SERVICE_UNAVAILABLE)

        apiServiceErrors.foreach { apiErrorCode =>
          val httpResponse = HttpResponse(apiErrorCode, Json.toJson(apiError).toString())
          stubGetHttpClientCall(
            s"/income-sources/brought-forward-losses/$nino/$lossId",
            httpResponse
          )
          val expectedResult = Left(ApiError(apiErrorCode, apiError))
          await(
            underTest.getPropertyBroughtForwardLoss(
              nino,
              lossId
            )(hc)
          ) shouldBe expectedResult
        }
      }
      "another unexpected error is returned from the API" in {
        val apiError = SingleErrorBody("code", "reason")
        val httpResponse = HttpResponse(INSUFFICIENT_STORAGE, Json.toJson(apiError).toString())
        stubGetHttpClientCall(
          s"/income-sources/brought-forward-losses/$nino/$lossId",
          httpResponse
        )
        val expectedResult = Left(ApiError(INTERNAL_SERVER_ERROR, apiError))
        await(
          underTest.getPropertyBroughtForwardLoss(
            nino,
            lossId
          )(hc)
        ) shouldBe expectedResult
      }
    }
  }

  object Requests {
    val validCreateBFLRequest: HipPropertyBFLRequest = HipPropertyBFLRequest(
      incomeSourceId = incomeSourceId,
      incomeSourceType = incomeSourceType,
      broughtForwardLossAmount = lossAmount,
      taxYearBroughtForwardFrom = toTaxYear(broughtForwardLossTaxYear).endYear
    )
  }
  object Responses {
    val hipPropertyBFLResponse: HipPropertyBFLResponse = HipPropertyBFLResponse(
      incomeSourceId = incomeSourceId.toString,
      incomeSourceType = incomeSourceType,
      broughtForwardLossAmount = lossAmount,
      taxYearBroughtForwardFrom = toTaxYear(broughtForwardLossTaxYear).endYear,
      lossId = lossId,
      submissionDate = LocalDate.now()
    )
  }
}
