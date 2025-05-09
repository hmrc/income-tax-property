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

package models.request.ukandforeign

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json._

class TotalPropertyIncomeSpec extends AnyWordSpec with Matchers {

  "TotalPropertyIncome" should {

    "write and read correctly" in {
      val lessThanJson = Json.toJson(TotalPropertyIncome.LessThan.toString)
      val maximumJson = Json.toJson(TotalPropertyIncome.Maximum.toString)

      lessThanJson shouldBe JsString("lessThan")
      maximumJson shouldBe JsString("maximum")

      lessThanJson.as[TotalPropertyIncome] shouldBe TotalPropertyIncome.LessThan
      maximumJson.as[TotalPropertyIncome] shouldBe TotalPropertyIncome.Maximum
    }

    "return the correct values" in {
      TotalPropertyIncome.values should contain theSameElementsAs Seq(
        TotalPropertyIncome.LessThan,
        TotalPropertyIncome.Maximum
      )
    }

    "be enumerable" in {
      TotalPropertyIncome.enumerable.withName("lessThan") shouldBe Some(TotalPropertyIncome.LessThan)
      TotalPropertyIncome.enumerable.withName("maximum") shouldBe Some(TotalPropertyIncome.Maximum)
      TotalPropertyIncome.enumerable.withName("invalid") shouldBe None
    }
  }
}