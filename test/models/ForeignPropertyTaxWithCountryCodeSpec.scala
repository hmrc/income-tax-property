/*
 * Copyright 2024 HM Revenue & Customs
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

package models

import models.request.foreign.{ForeignIncomeTax, ForeignPropertyTaxWithCountryCode}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsSuccess, Json}

class ForeignPropertyTaxWithCountryCodeSpec extends AnyFreeSpec with Matchers with OptionValues {

  val jsonWithAllFields: String =
    """{
      |  "countryCode": "BRA",
      |  "foreignIncomeTax": {
      |    "isForeignIncomeTax": true,
      |    "foreignTaxPaidOrDeducted": 65
      |  },
      |  "isForeignTaxCreditRelief": false
      |}""".stripMargin

  "ForeignPropertyTaxWithCountryCode" - {

    "must serialize to JSON correctly" in {
      val model = ForeignPropertyTaxWithCountryCode(
        countryCode = "BRA",
        foreignIncomeTax = Some(ForeignIncomeTax(isForeignIncomeTax = true, Some(BigDecimal(65)))),
        isForeignTaxCreditRelief = Some(false)
      )

      val expectedJson = Json.parse(jsonWithAllFields)
      Json.toJson(model) mustBe expectedJson
    }

    "must deserialize from JSON correctly" in {
      val expectedModel = ForeignPropertyTaxWithCountryCode(
        countryCode = "BRA",
        foreignIncomeTax = Some(ForeignIncomeTax(isForeignIncomeTax = true, Some(BigDecimal(65)))),
        isForeignTaxCreditRelief = Some(false)
      )

      Json.parse(jsonWithAllFields).validate[ForeignPropertyTaxWithCountryCode] mustBe JsSuccess(expectedModel)
    }

  }
}
