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

import models.common.TaxYear
import models.errors.InternalError
import models.request.foreign.{CreateForeignPropertyPeriodicSubmissionRequest, ForeignPropertyTaxWithCountryCode}
import models.responses._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

import java.time.LocalDate

class CreateForeignPropertyPeriodicSubmissionRequestSpec extends AnyWordSpec with Matchers {

  "CreateForeignPropertyPeriodicSubmissionRequest" should {

    "serialize correctly to JSON" in {
      val request = CreateForeignPropertyPeriodicSubmissionRequest(
        fromDate = LocalDate.parse("2023-04-06"),
        toDate = LocalDate.parse("2024-04-05"),
        foreignProperty = Some(Seq(ForeignProperty("GB", None, None)))
      )

      val expectedJson = Json.obj(
        "fromDate" -> "2023-04-06",
        "toDate"   -> "2024-04-05",
        "foreignProperty" -> Json.arr(
          Json.obj(
            "countryCode" -> "GB"
          )
        )
      )

      Json.toJson(request) shouldBe expectedJson
    }

    "return a Right when converting from a valid ForeignPropertyTaxWithCountryCode entity" in {
      val taxYear = TaxYear(2024)
      val foreignPropertyIncome = ForeignPropertyIncome(
        rentIncome = Some(ForeignPropertyRentIncome(BigDecimal(100))),
        isForeignTaxCreditRelief = Some(true),
        premiumsOfLeaseGrant = Some(50),
        otherPropertyIncome = Some(200),
        foreignTaxPaidOrDeducted = Some(25),
        specialWithholdingTaxOrUkTaxPaid = Some(10)
      )

      val foreignPropertyExpenses = ForeignPropertyExpenses(
        premisesRunningCosts = Some(50),
        repairsAndMaintenance = Some(30),
        financialCosts = Some(20),
        professionalFees = Some(10),
        other = Some(5),
        travelCosts = Some(65),
        costOfServices = Some(99),
        residentialFinancialCost = Some(45),
        broughtFwdResidentialFinancialCost = Some(10),
        consolidatedExpense = None,
        consolidatedExpenseAmount = None
      )

      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        submissionId = Some(PeriodicSubmissionId("some-id")),
        submittedOn = None,
        fromDate = LocalDate.parse("2023-04-06"),
        toDate = LocalDate.parse("2024-04-05"),
        foreignProperty = Some(Seq(ForeignProperty("GB", Some(foreignPropertyIncome), Some(foreignPropertyExpenses)))),
        ukOtherProperty = None
      )

      val foreignPropertyTaxWithCountryCode = ForeignPropertyTaxWithCountryCode(
        countryCode = "GB",
        isForeignTaxCreditRelief = Some(true),
        foreignIncomeTax = None
      )

      val result = CreateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
        taxYear,
        Some(propertyPeriodicSubmission),
        foreignPropertyTaxWithCountryCode
      )

      result shouldBe a[Right[_, _]]
      result.toOption.get.foreignProperty.get.head.income.get.rentIncome shouldBe Some(ForeignPropertyRentIncome(100))
    }

    "return a Left when entity is invalid for conversion" in {
      val taxYear = TaxYear(2024)
      val result = CreateForeignPropertyPeriodicSubmissionRequest.fromEntity(
        taxYear,
        None,
        entity = "InvalidEntity"
      )

      result shouldBe a[Left[_, _]]
      result.left.get shouldBe InternalError(
        "No relevant entity found to convert from (to CreateForeignPropertyPeriodicSubmissionRequest)"
      )
    }

    "handle missing periodic submission gracefully" in {
      val taxYear = TaxYear(2024)
      val foreignPropertyTaxWithCountryCode = ForeignPropertyTaxWithCountryCode(
        countryCode = "GB",
        isForeignTaxCreditRelief = Some(true),
        foreignIncomeTax = None
      )

      val result = CreateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
        taxYear,
        None,
        foreignPropertyTaxWithCountryCode
      )

      result shouldBe a[Right[_, _]]
      val request = result.toOption.get

      request.fromDate shouldBe LocalDate.parse("2023-04-06")
      request.toDate shouldBe LocalDate.parse("2024-04-05")
      request.foreignProperty.get.head.countryCode shouldBe "GB"
    }

    "include default foreign property income when none is provided" in {
      val taxYear = TaxYear(2024)
      val foreignPropertyTaxWithCountryCode = ForeignPropertyTaxWithCountryCode(
        countryCode = "GB",
        isForeignTaxCreditRelief = Some(true),
        foreignIncomeTax = None
      )

      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        submissionId = Some(PeriodicSubmissionId("some-id")),
        submittedOn = None,
        fromDate = LocalDate.parse("2023-04-06"),
        toDate = LocalDate.parse("2024-04-05"),
        foreignProperty = Some(Seq(ForeignProperty("GB", None, None))),
        ukOtherProperty = None
      )

      val result = CreateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
        taxYear,
        Some(propertyPeriodicSubmission),
        foreignPropertyTaxWithCountryCode
      )

      result shouldBe a[Right[_, _]]
      val request = result.toOption.get

      request.foreignProperty.get.head.income.get.isForeignTaxCreditRelief shouldBe Some(true)
      request.foreignProperty.get.head.income.get.rentIncome shouldBe None
    }

  }
}
