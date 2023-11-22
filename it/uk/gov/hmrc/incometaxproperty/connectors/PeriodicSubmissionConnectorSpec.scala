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
import play.api.http.Status.OK
import play.api.libs.json.Json
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId}
import uk.gov.hmrc.incometaxproperty.models.responses.{PeriodicSubmissionIdModel, PeriodicSubmissionModel}
import uk.gov.hmrc.incometaxproperty.support.ConnectorIntegrationTest

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class PeriodicSubmissionConnectorSpec
  extends ConnectorIntegrationTest
  with MockFactory {

  private val taxYear = LocalDate.now.getYear.toString
  private val taxableEntityId = "some-taxable-entity-id"
  private val incomeSourceId = "some-income-source-id"
  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))

  private val underTest = new PeriodicSubmissionConnector(httpClient, appConfigStub)

  val aPeriodicSubmissionModel = PeriodicSubmissionModel(Some(List(
    PeriodicSubmissionIdModel("1", "2021-01-01", "2021-11-11"),
    PeriodicSubmissionIdModel("2", "2022-02-02", "2022-12-12")
  )))

  "Given a need to get Periodic Submission Data" when {
    "a call is made to the backend API it" should {
      "return correct IF data when correct parameters are passed" in {

        val httpResponse = HttpResponse(OK, Json.toJson(aPeriodicSubmissionModel).toString())

        stubGetHttpClientCall(s"/income-tax/business/property/$taxYear/$taxableEntityId/$incomeSourceId/period", httpResponse)

        await(underTest.getPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Right(aPeriodicSubmissionModel)
      }
    }
  }
}
