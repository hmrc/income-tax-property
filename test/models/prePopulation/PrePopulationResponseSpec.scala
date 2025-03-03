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

package models.prePopulation

import models.{BusinessDetailsResponse, PropertyDetails}
import play.api.libs.json.Json
import utils.UnitTest

class PrePopulationResponseSpec extends UnitTest {

  "PrePopulationResponse" when {
    "written to JSON" should {
      "return the expected JsValue" in {
        val prePopulationResponse = PrePopulationResponse(
          hasProperty = true
        )

        Json.toJson(prePopulationResponse) shouldBe Json.parse(
          """
            |{
            |  "hasProperty": true
            |}
        """.stripMargin
        )
      }
    }

    "fromData" should {
      "return hasPropertyData as 'false' when Business returns no Property data" in {
        PrePopulationResponse.fromData(BusinessDetailsResponse(Seq())) shouldBe PrePopulationResponse(false)
      }

      "return hasPropertyData as 'true' when uk-property data exists for Business" in {
        val someUkProperty = List(PropertyDetails(Some("uk-property"), None, None, "XYIS00000451267"))
        PrePopulationResponse.fromData(BusinessDetailsResponse(someUkProperty)) shouldBe PrePopulationResponse(true)
      }

      "return hasPropertyData as 'true' when foreign-property data exists for Business" in {
        val someForeignProperty = List(PropertyDetails(Some("foreign-property"), None, None, "XYIS00000451267"))
        PrePopulationResponse.fromData(BusinessDetailsResponse(someForeignProperty)) shouldBe PrePopulationResponse(true)
      }

      "return hasPropertyData as 'true' when both uk-property and foreign-property data exists for Business" in {
        val someProperty = List(
          PropertyDetails(Some("uk-property"), None, None, "XYIS00000451267"),
          PropertyDetails(Some("foreign-property"), None, None, "XYIS00000451268")
        )
        PrePopulationResponse.fromData(BusinessDetailsResponse(someProperty)) shouldBe PrePopulationResponse(true)
      }
    }
  }
}
