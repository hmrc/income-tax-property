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

import models.IncomeSourceType
import models.IncomeSourceType.UKPropertyOther
import models.common.IncomeSourceId
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

class HipPropertyBFLRequestSpec extends AnyWordSpec with Matchers {
  private val incomeSourceId: IncomeSourceId = IncomeSourceId("test-income-source-id")
  private val incomeSourceType: IncomeSourceType = UKPropertyOther
  private val lossAmount: BigDecimal = BigDecimal(123.45)
  private val taxYear: Int = 2024

  "HipPropertyBFLRequest" should {
    "serialize correctly to JSON" in {
      val request = HipPropertyBFLRequest(
        incomeSourceId = incomeSourceId,
        incomeSourceType = incomeSourceType,
        broughtForwardLossAmount = lossAmount,
        taxYearBroughtForwardFrom = taxYear
      )

      val expectedJson = Json.obj(
        "incomeSourceId"            -> "test-income-source-id",
        "incomeSourceType"          -> "02",
        "broughtForwardLossAmount"  -> 123.45,
        "taxYearBroughtForwardFrom" -> 2024
      )

      Json.toJson(request) shouldBe expectedJson
    }
  }
}
