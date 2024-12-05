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

import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsError, JsSuccess, Json}
import models.request.foreign.expenses.{ConsolidatedExpenses, ForeignPropertyExpenses}

class ForeignPropertyExpensesSpec extends PlaySpec {

  "ForeignPropertyExpenses" should {

    "serialize to JSON correctly" in {
      val expenses = ForeignPropertyExpenses(
        countryCode = "US",
        consolidatedExpenses = Some(ConsolidatedExpenses(consolidatedOrIndividualExpensesYesNo = false, None)),
        premisesRunningCosts = Some(150.50),
        repairsAndMaintenance = Some(300.00),
        financialCosts = Some(100.75),
        professionalFees = Some(80.25),
        costOfServices = Some(50.00),
        other = Some(35)
      )

      val expectedJson = Json.parse(
        """
          |{
          |  "countryCode": "US",
          |  "consolidatedExpenses": { "consolidatedOrIndividualExpensesYesNo": false },
          |  "premisesRunningCosts": 150.50,
          |  "repairsAndMaintenance": 300.00,
          |  "financialCosts": 100.75,
          |  "professionalFees": 80.25,
          |  "costOfServices": 50.00,
          |  "other": 35
          |}
          |""".stripMargin
      )

      Json.toJson(expenses) mustEqual expectedJson
    }

    "deserialize from JSON correctly" in {
      val json = Json.parse(
        """
          |{
          |  "countryCode": "US",
          |  "consolidatedExpenses": { "consolidatedOrIndividualExpensesYesNo": false },
          |  "premisesRunningCosts": 150.50,
          |  "repairsAndMaintenance": 300.00,
          |  "financialCosts": 100.75,
          |  "professionalFees": 80.25,
          |  "costOfServices": 50.00,
          |  "other": 35
          |}
          |""".stripMargin
      )

      val expectedExpenses = ForeignPropertyExpenses(
        countryCode = "US",
        consolidatedExpenses = Some(ConsolidatedExpenses(consolidatedOrIndividualExpensesYesNo = false,None)),
        premisesRunningCosts = Some(150.50),
        repairsAndMaintenance = Some(300.00),
        financialCosts = Some(100.75),
        professionalFees = Some(80.25),
        costOfServices = Some(50.00),
        other = Some(35)
      )

      json.validate[ForeignPropertyExpenses] mustEqual JsSuccess(expectedExpenses)
    }

    "return a JsError for invalid JSON" in {
      val invalidJson = Json.parse(
        """
          |{
          |  "countryCode": "US",
          |  "consolidatedExpenses": "invalid value"
          |}
          |""".stripMargin
      )

      invalidJson.validate[ForeignPropertyExpenses] mustBe a[JsError]
    }

    "handle optional fields correctly" in {
      val jsonWithMissingFields = Json.parse(
        """
          |{
          |  "countryCode": "US"
          |}
          |""".stripMargin
      )

      val expectedExpenses = ForeignPropertyExpenses(
        countryCode = "US",
        consolidatedExpenses = None,
        premisesRunningCosts = None,
        repairsAndMaintenance = None,
        financialCosts = None,
        professionalFees = None,
        costOfServices = None,
        other = None
      )

      jsonWithMissingFields.validate[ForeignPropertyExpenses] mustEqual JsSuccess(expectedExpenses)
    }
  }
}
