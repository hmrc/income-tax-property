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

package models

import models.request.foreignIncome.ForeignIncomeDividendsWithCountryCode
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsError, JsSuccess, Json}

class ForeignIncomeDividendsSpec extends PlaySpec {

  "ForeignIncomeDividends" should {

    "serialize to JSON correctly" in {
      val dividends = ForeignIncomeDividendsWithCountryCode(
        countryCode = "AUS",
        amountBeforeTax = Some(231.45),
        taxTakenOff = Some(321.54),
        specialWithholdingTax = Some(490.58),
        foreignTaxCreditRelief = true,
        taxableAmount = 80.80
      )

      val expectedJson = Json.parse(
        """
          {
          |  "countryCode": "AUS",
          |  "amountBeforeTax": 231.45,
          |  "taxTakenOff": 321.54,
          |  "specialWithholdingTax": 490.58,
          |  "foreignTaxCreditRelief": true,
          |  "taxableAmount": 80.80
          |}
          |""".stripMargin
      )

      Json.toJson(dividends) mustEqual expectedJson
    }

    "deserialize from JSON correctly" in {
      val json = Json.parse(
        """
         {
          |  "countryCode": "AUS",
          |  "amountBeforeTax": 231.45,
          |  "taxTakenOff": 321.54,
          |  "specialWithholdingTax": 490.58,
          |  "foreignTaxCreditRelief": true,
          |  "taxableAmount": 80.80
          |}
          |""".stripMargin
      )

      val expectedDividends = ForeignIncomeDividendsWithCountryCode(
        countryCode = "AUS",
        amountBeforeTax = Some(231.45),
        taxTakenOff = Some(321.54),
        specialWithholdingTax = Some(490.58),
        foreignTaxCreditRelief = true,
        taxableAmount = 80.80
      )

      json.validate[ForeignIncomeDividendsWithCountryCode] mustEqual JsSuccess(expectedDividends)
    }

    "return a JsError for invalid JSON" in {
      val invalidJson = Json.parse(
        """
          |{
          |  "countryCode": "AUS",
          |  "amountBeforeTax": "invalid value"
          |}
          |""".stripMargin
      )

      invalidJson.validate[ForeignIncomeDividendsWithCountryCode] mustBe a[JsError]
    }

    "handle optional fields correctly" in {
      val jsonWithMissingFields = Json.parse(
        """
          |{
          |  "countryCode": "AUS",
          |  "foreignTaxCreditRelief": true,
          |  "taxableAmount": 80.80
          |}
          |""".stripMargin
      )

      val expectedDividends = ForeignIncomeDividendsWithCountryCode(
        countryCode = "AUS",
        amountBeforeTax = None,
        taxTakenOff = None,
        specialWithholdingTax = None,
        foreignTaxCreditRelief = true,
        taxableAmount = 80.80
      )

      jsonWithMissingFields.validate[ForeignIncomeDividendsWithCountryCode] mustEqual JsSuccess(expectedDividends)
    }

  }
}
