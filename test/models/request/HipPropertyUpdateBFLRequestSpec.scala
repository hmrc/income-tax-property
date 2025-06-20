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

package models.request

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

class HipPropertyUpdateBFLRequestSpec extends AnyWordSpec with Matchers {
  private val lossAmount: BigDecimal = BigDecimal(123.45)

  "HipPropertyUpdateBFLRequest" should {
    "serialize correctly to JSON" in {
      val request = HipPropertyUpdateBFLRequest(
        updatedBroughtForwardLossAmount = lossAmount
      )

      val expectedJson = Json.obj(
        "updatedBroughtForwardLossAmount"  -> 123.45
      )

      Json.toJson(request) shouldBe expectedJson
    }
  }
}
