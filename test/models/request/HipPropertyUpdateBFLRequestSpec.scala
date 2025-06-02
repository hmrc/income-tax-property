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
import models.responses.BroughtForwardLossId
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

import java.time.LocalDate

class HipPropertyUpdateBFLRequestSpec extends AnyWordSpec with Matchers {
  private val incomeSourceId: IncomeSourceId = IncomeSourceId("test-income-source-id")
  private val incomeSourceType: IncomeSourceType = UKPropertyOther
  private val lossAmount: BigDecimal = BigDecimal(123.45)
  private val taxYear: Int = 2024
  private val lossID: BroughtForwardLossId = BroughtForwardLossId("AT0000000000001")
  private val submissionDate: LocalDate = LocalDate.now

  "HipPropertyUpdateBFLRequest" should {
    "serialize correctly to JSON" in {
      val request = HipPropertyUpdateBFLRequest(
        incomeSourceId = incomeSourceId,
        incomeSourceType = incomeSourceType,
        broughtForwardLossAmount = lossAmount,
        taxYearBroughtForwardFrom = taxYear,
        lossID = lossID.toString,
        submissionDate = submissionDate.toString
      )

      val expectedJson = Json.obj(
        "incomeSourceId"            -> "test-income-source-id",
        "incomeSourceType"          -> "02",
        "broughtForwardLossAmount"  -> 123.45,
        "taxYearBroughtForwardFrom" -> 2024,
        "lossID"                    -> "AT0000000000001",
        "submissionDate"            -> LocalDate.now.toString
      )

      Json.toJson(request) shouldBe expectedJson
    }
  }
}
