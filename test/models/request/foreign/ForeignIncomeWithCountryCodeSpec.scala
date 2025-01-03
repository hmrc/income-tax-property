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

package models.request.foreign

import models.request.ReversePremiumsReceived
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsSuccess, Json}

class ForeignIncomeWithCountryCodeSpec extends AnyFreeSpec with Matchers with OptionValues {

  private val jsonWithAllFields =
    """
      | {
      |    "countryCode": "AUS",
      |    "rentIncome": 1,
      |    "premiumsGrantLeaseReceived": true,
      |    "reversePremiumsReceived": {
      |      "reversePremiumsReceived": true,
      |      "reversePremiums": 4
      |    },
      |    "otherPropertyIncome": 5,
      |    "calculatedPremiumLeaseTaxable": {
      |      "calculatedPremiumLeaseTaxable": false
      |    },
      |    "premiumsOfLeaseGrantAgreed": {
      |      "premiumsOfLeaseGrantAgreed": true,
      |      "premiumsOfLeaseGrant": 1.92
      |    },
      |    "receivedGrantLeaseAmount": 2,
      |    "twelveMonthPeriodsInLease": 3
      |  }
      |""".stripMargin

  "ForeignIncomeWithCountryCode" - {

    "must serialize the JSON supplied from the FE" in {
      val model: ForeignIncomeWithCountryCode = ForeignIncomeWithCountryCode(
        countryCode = "AUS",
        rentIncome = BigDecimal(1),
        premiumsGrantLeaseReceived = true,
        reversePremiumsReceived =
          ReversePremiumsReceived(reversePremiumsReceived = true, reversePremiums = Some(BigDecimal(4))),
        otherPropertyIncome = BigDecimal(5),
        calculatedPremiumLeaseTaxable =
          Some(CalculatedPremiumLeaseTaxable(calculatedPremiumLeaseTaxable = false, premiumsOfLeaseGrant = None)),
        receivedGrantLeaseAmount = Some(BigDecimal(2)),
        twelveMonthPeriodsInLease = Some(BigDecimal(3)),
        premiumsOfLeaseGrantAgreed = Some(
          PremiumsOfLeaseGrantAgreed(premiumsOfLeaseGrantAgreed = true, premiumsOfLeaseGrant = Some(BigDecimal(1.92)))
        )
      )

      val expectedJson = Json.parse(jsonWithAllFields)
      Json.toJson(model) mustBe expectedJson
    }

    "must deserialize the Object ForeignIncomeWithCountryCode" in {
      val foreignIncomeModel = ForeignIncomeWithCountryCode(
        countryCode = "AUS",
        rentIncome = BigDecimal(1),
        premiumsGrantLeaseReceived = true,
        reversePremiumsReceived =
          ReversePremiumsReceived(reversePremiumsReceived = true, reversePremiums = Some(BigDecimal(4))),
        otherPropertyIncome = BigDecimal(5),
        calculatedPremiumLeaseTaxable =
          Some(CalculatedPremiumLeaseTaxable(calculatedPremiumLeaseTaxable = false, premiumsOfLeaseGrant = None)),
        receivedGrantLeaseAmount = Some(BigDecimal(2)),
        twelveMonthPeriodsInLease = Some(BigDecimal(3)),
        premiumsOfLeaseGrantAgreed = Some(
          PremiumsOfLeaseGrantAgreed(premiumsOfLeaseGrantAgreed = true, premiumsOfLeaseGrant = Some(BigDecimal(1.92)))
        )
      )

      Json.parse(jsonWithAllFields).validate[ForeignIncomeWithCountryCode] mustBe JsSuccess(foreignIncomeModel)
    }
  }

}
