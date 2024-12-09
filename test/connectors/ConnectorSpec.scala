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

import org.mockito.Mockito._
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.funsuite.AnyFunSuite
import uk.gov.hmrc.http.HeaderCarrier

class ConnectorSpec extends AnyFunSuite with MockitoSugar {

  test("hcWithCorrelationId should return the same HeaderCarrier if CorrelationId is not present") {
    val mockHeaderCarrier = mock[HeaderCarrier]
    when(mockHeaderCarrier.otherHeaders).thenReturn(Seq.empty)
    val result = Connector.hcWithCorrelationId(mockHeaderCarrier)
    assert(!result.otherHeaders.exists(_._1 == "CorrelationId"))
  }
}
