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

import models.errors.InternalError
import models.request.foreign._
import models.responses._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsValue, Json}

import java.time.{LocalDate, LocalDateTime}

class UpdateForeignPropertyPeriodicSubmissionRequestSpec extends AnyWordSpec with Matchers with EitherValues {

  "UpdateForeignPropertyPeriodicSubmissionRequest" should {

    "serialize to JSON correctly" in {
      val request = UpdateForeignPropertyPeriodicSubmissionRequest(
        Some(
          Seq(
            ForeignProperty(
              "FR",
              Some(ForeignPropertyIncome(None, Some(true), None, None, Some(BigDecimal(543.00)), None)),
              Some(
                ForeignPropertyExpenses(None, None, None, None, None, None, None, None, None, Some(BigDecimal(123.00)))
              )
            )
          )
        )
      )

      val expectedJson: JsValue = Json.parse(
        """
          {
            "foreignProperty": [
              {
                "countryCode": "FR",
                "income": {
                  "foreignTaxCreditRelief": true,
                  "foreignTaxPaidOrDeducted": 543.00
                },
                "expenses": {
                  "consolidatedExpense": 123.00
                }
              }
            ]
          }
        """
      )

      Json.toJson(request) shouldBe expectedJson
    }

    "return InternalError when an unsupported entity is passed to fromEntity" in {
      val result = UpdateForeignPropertyPeriodicSubmissionRequest.fromEntity(None, "unsupportedEntity")

      result.left.value shouldBe an[InternalError]
      result.left.value.message should include("No relevant entity found to convert from")
    }

    "create a request successfully using fromForeignPropertyTax" in {
      val taxWithCountryCode = ForeignPropertyTaxWithCountryCode(
        countryCode = "FR",
        foreignTaxCreditRelief = Some(true),
        foreignIncomeTax = Some(ForeignIncomeTax(foreignIncomeTaxYesNo = true, Some(50.0)))
      )

      val periodicSubmission = Some(
        PropertyPeriodicSubmission(
          Some(PeriodicSubmissionId("some")),
          Some(LocalDateTime.now),
          LocalDate.of(1, 4, 6),
          LocalDate.of(1, 4, 5),
          Some(
            Seq(
              ForeignProperty(
                "FR",
                Some(ForeignPropertyIncome(None, Some(true), None, None, Some(BigDecimal(543.00)), None)),
                Some(
                  ForeignPropertyExpenses(None, None, None, None, None, None, None, None, None, Some(BigDecimal(123.00)))
                )
              )
            )
          ),
          None
        )
      )

      val result = UpdateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
        periodicSubmission,
        taxWithCountryCode
      )

      result.value.foreignProperty shouldBe defined
      val foreignProperty = result.value.foreignProperty.get.head

      foreignProperty.countryCode shouldBe "FR"
      foreignProperty.income shouldBe defined
      foreignProperty.income.get.foreignTaxCreditRelief shouldBe Some(true)
      foreignProperty.income.get.foreignTaxPaidOrDeducted shouldBe Some(50.0) // Default to 0 if not provided
      foreignProperty.expenses shouldBe defined
    }

    "handle None for periodicSubmission in fromForeignPropertyTax" in {
      val taxWithCountryCode = ForeignPropertyTaxWithCountryCode(
        countryCode = "FR",
        foreignTaxCreditRelief = Some(false),
        foreignIncomeTax = None
      )

      val result = UpdateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
        None,
        taxWithCountryCode
      )

      result.value.foreignProperty shouldBe defined
      val foreignProperty = result.value.foreignProperty.get.head

      foreignProperty.countryCode shouldBe "FR"
      foreignProperty.income shouldBe defined
      foreignProperty.income.get.foreignTaxCreditRelief shouldBe Some(false)
      foreignProperty.income.get.foreignTaxPaidOrDeducted shouldBe Some(0.0) // Default to 0 if not provided
      foreignProperty.expenses shouldBe None
    }
  }
}
