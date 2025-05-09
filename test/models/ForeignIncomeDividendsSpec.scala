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

import models.request.foreignincome.{ForeignIncomeDividend, ForeignIncomeDividendsWithCountryCode}
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsError, JsSuccess, Json}

class ForeignIncomeDividendsSpec extends PlaySpec {

  "ForeignIncomeDividends" should {

    "serialize to JSON correctly" in {
      val dividends = ForeignIncomeDividend(
        countryCode = "AUS",
        incomeBeforeForeignTaxDeducted = 231.45,
        foreignTaxDeductedFromDividendIncome = true,
        howMuchForeignTaxDeductedFromDividendIncome = Some(321.54),
        claimForeignTaxCreditRelief = Some(true)
      )

      val foreignIncomeDividendsWithCountryCode = ForeignIncomeDividendsWithCountryCode(
        foreignIncomeDividends = Seq(dividends)
      )

      val expectedJson = Json.parse(
        """
          {
          | "foreignIncomeDividends": [
          |   {
          |     "countryCode": "AUS",
          |     "incomeBeforeForeignTaxDeducted": 231.45,
          |     "foreignTaxDeductedFromDividendIncome": true,
          |     "howMuchForeignTaxDeductedFromDividendIncome": 321.54,
          |     "claimForeignTaxCreditRelief": true
          |   }
          | ]
          |}
          |""".stripMargin
      )

      Json.toJson(foreignIncomeDividendsWithCountryCode) mustEqual expectedJson
    }

    "deserialize from JSON correctly" in {
      val json = Json.parse(
        """
         {
          | "foreignIncomeDividends": [
          |   {
          |     "countryCode": "AUS",
          |     "incomeBeforeForeignTaxDeducted": 231.45,
          |     "foreignTaxDeductedFromDividendIncome": true,
          |     "howMuchForeignTaxDeductedFromDividendIncome": 321.54,
          |     "claimForeignTaxCreditRelief": true
          |   }
          | ]
          |}
          |""".stripMargin
      )

      val expectedDividends = ForeignIncomeDividendsWithCountryCode(
        foreignIncomeDividends = Seq(
          ForeignIncomeDividend(
            countryCode = "AUS",
            incomeBeforeForeignTaxDeducted = 231.45,
            foreignTaxDeductedFromDividendIncome = true,
            howMuchForeignTaxDeductedFromDividendIncome = Some(321.54),
            claimForeignTaxCreditRelief = Some(true)
          )
        )
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
          | "foreignIncomeDividends": [
          |   {
          |    "countryCode": "AUS",
          |    "foreignTaxDeductedFromDividendIncome": true,
          |    "incomeBeforeForeignTaxDeducted": 80.80
          |  }
          | ]
          |}
          |""".stripMargin
      )

      val expectedDividends = ForeignIncomeDividendsWithCountryCode(
        foreignIncomeDividends = Seq(ForeignIncomeDividend(
          countryCode = "AUS",
          foreignTaxDeductedFromDividendIncome = true,
          incomeBeforeForeignTaxDeducted = 80.80,
          howMuchForeignTaxDeductedFromDividendIncome = None,
          claimForeignTaxCreditRelief = None
        ))
      )

      jsonWithMissingFields.validate[ForeignIncomeDividendsWithCountryCode] mustEqual JsSuccess(expectedDividends)
    }

  }
}
