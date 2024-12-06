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

package connectors

import models.errors.{ApiError, SingleErrorBody}
import org.scalamock.scalatest.MockFactory
import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId}
import utils.builders.IncomeSourceDetailsBuilder.anIncomeSourceDetails

import scala.concurrent.ExecutionContext.Implicits.global

class BusinessDetailsConnectorSpec extends ConnectorIntegrationSpec with MockFactory {

  private val nino = "some-nino"
  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))

  private val underTest = new BusinessDetailsConnector(httpClientV2, appConfigStub)

  ".getBusinessDetails" when {
    "when we call the IF" should {
      "return correct IF data when correct parameters are passed" in {
        val httpResponse = HttpResponse(OK, Json.toJson(anIncomeSourceDetails).toString())

        stubGetHttpClientCall(s"/registration/business-details/nino/$nino", httpResponse)

        await(underTest.getBusinessDetails(nino)(hc)) shouldBe Right(Some(anIncomeSourceDetails))
      }

      "return IF error when Left is returned" in {
        val httpResponse =
          HttpResponse(INTERNAL_SERVER_ERROR, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())

        stubGetHttpClientCall(s"/registration/business-details/nino/$nino", httpResponse)

        await(underTest.getBusinessDetails(nino)(hc)) shouldBe
          Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("some-code", "some-reason")))
      }
    }
  }

}
