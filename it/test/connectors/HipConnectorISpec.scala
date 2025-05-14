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

import models.common.{IncomeSourceId, Nino}
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.{HeaderCarrier, SessionId}
import scala.concurrent.ExecutionContext.Implicits.global

class HipConnectorISpec extends ConnectorIntegrationSpec with MockFactory {

  private val nino = Nino("some-nino")
  private val incomeSourceId = IncomeSourceId("some-income-source-id")
  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))

  private val underTest = new HipConnector(httpClientV2, appConfigStub)

  ".createPropertyBroughtForwardLoss" should {
    "do something" in {
      succeed
    }
  }
}
