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

package controllers

import models.prePopulation.PrePopulationResponse
import play.api.http.Status.{FORBIDDEN, INTERNAL_SERVER_ERROR, NOT_FOUND, OK}
import play.api.libs.json.Json
import play.api.libs.ws.{WSRequest, WSResponse}
import play.api.test.Helpers.AUTHORIZATION
import support.ControllerIntegrationTest
import support.stubs.AuthStub._
import support.stubs.WireMockStubs
import uk.gov.hmrc.http.HttpResponse
import utils.builders.IncomeSourceDetailsBuilder._

class PrePopulationControllerISpec extends ControllerIntegrationTest
  with WireMockStubs
  {

  trait Test {
    val nino: String = "AA123123A"
    val taxYear: Int = 2024
    val mtdItId: String = "555555555"
    val ifTaxYearParam = s"${(taxYear - 1).toString.takeRight(2)}-${taxYear.toString.takeRight(2)}"

    val ifUrl: String = s"/registration/business-details/nino/$nino"

    def request(): WSRequest = {
      authorised()
      buildRequest(s"/income-tax-property/property/pre-population/$nino")
        .withFollowRedirects(false)
        .withHttpHeaders(
          (AUTHORIZATION, "Bearer 123"),
          ("mtditid", mtdItId)
        )
    }
  }

  "/property/pre-population/:nino" when {
    val notFoundHttpResponse: HttpResponse = HttpResponse(NOT_FOUND, "404 not found")

    "IF returns a non-404 error when retrieving a user's Property" should {
      "return an INTERNAL SERVER ERROR response" in new Test {
        stubGetHttpClientCall(ifUrl, HttpResponse(FORBIDDEN, "FORBIDDEN"))

        val result: WSResponse = await(request().get())
        result.status shouldBe INTERNAL_SERVER_ERROR
      }
    }

    "IF returns a 404 error when retrieving a user's Property" should {
      "return an empty pre-pop response" in new Test {
        stubGetHttpClientCall(ifUrl, notFoundHttpResponse)

        val result: WSResponse = await(request().get())
        result.status shouldBe OK
        result.json shouldBe Json.toJson(PrePopulationResponse.noPrePop)
      }
    }

    "IF returns relevant data when retrieving a user's Property" should {
      "return the appropriate pre-population response when only uk-property data exists" in new Test {
        val httpResponse: HttpResponse = HttpResponse(OK, Json.toJson(ukPropertyDetails).toString())
        stubGetHttpClientCall(ifUrl, httpResponse)

        val result: WSResponse = await(request().get())
        result.status shouldBe OK
        result.json shouldBe Json.toJson(PrePopulationResponse(
          hasUkPropertyPrePop = true,
          hasForeignPropertyPrePop = false
        ))
      }

      "return the appropriate pre-population response when only foreign-property data exists" in new Test {
        val httpResponse: HttpResponse = HttpResponse(OK, Json.toJson(foreignPropertyDetails).toString())
        stubGetHttpClientCall(ifUrl, httpResponse)

        val result: WSResponse = await(request().get())
        result.status shouldBe OK
        result.json shouldBe Json.toJson(PrePopulationResponse(
          hasUkPropertyPrePop = false,
          hasForeignPropertyPrePop = true
        ))
      }

      "return the appropriate pre-population response for a mixed scenario" in new Test {
        val httpResponse: HttpResponse = HttpResponse(OK, Json.toJson(mixedPropertyDetails).toString())

        stubGetHttpClientCall(ifUrl, httpResponse)

        val result: WSResponse = await(request().get())
        result.status shouldBe OK
        result.json shouldBe Json.toJson(PrePopulationResponse(
          hasUkPropertyPrePop = true,
          hasForeignPropertyPrePop = true
        ))
      }

      "return the appropriate pre-population response for a no-data scenario" in new Test {
        val httpResponse: HttpResponse = HttpResponse(OK, Json.toJson(nonePropertyDetails).toString())

        stubGetHttpClientCall(ifUrl, httpResponse)

        val result: WSResponse = await(request().get())
        result.status shouldBe OK
        result.json shouldBe Json.toJson(PrePopulationResponse.noPrePop)
      }
    }
  }
}
