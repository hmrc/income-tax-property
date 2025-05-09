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

package models.request.foreignincome

import models.request.foreignincome.ForeignIncomeSubmission.emptyForeignIncomeSubmission
import utils.UnitTest

class ForeignIncomeSubmissionSpec extends UnitTest {

  "ForeignIncomeSubmission" should {
    "be generated from foreign income dividends" in {
      val germanySpecialWithholdingTax: Option[BigDecimal] = Some(3.40)
      val germanyTaxableAmount = 1
      val spainDividends = ForeignDividend(
        countryCode = "ESP",
        amountBeforeTax = Some(12.23),
        taxTakenOff = None,
        specialWithholdingTax = None,
        foreignTaxCreditRelief = Some(false),
        taxableAmount = 0
      )
      val germanyDividends = ForeignDividend(
        countryCode = "DEU",
        amountBeforeTax = Some(98.87),
        taxTakenOff = Some(1.20),
        specialWithholdingTax = germanySpecialWithholdingTax,
        foreignTaxCreditRelief = Some(false),
        taxableAmount = germanyTaxableAmount
      )
      val updatedGermanyDividendsFromUserAnswers = ForeignIncomeDividend(
        countryCode = "DEU",
        incomeBeforeForeignTaxDeducted = 100.00,
        howMuchForeignTaxDeductedFromDividendIncome = Some(1.20),
        claimForeignTaxCreditRelief = Some(false),
        foreignTaxDeductedFromDividendIncome = false
      )
      val usaDividendsFromUserAnswers = ForeignIncomeDividend(
        countryCode = "USA",
        incomeBeforeForeignTaxDeducted = 44.44,
        howMuchForeignTaxDeductedFromDividendIncome = Some(1.20),
        claimForeignTaxCreditRelief = Some(true),
        foreignTaxDeductedFromDividendIncome = true
      )
      val submissionFromDownstream = emptyForeignIncomeSubmission.copy(
        foreignDividend = Some(Seq(spainDividends, germanyDividends))
      )
      val userAnswers = ForeignIncomeDividendsWithCountryCode(
        foreignIncomeDividends = Seq(
          usaDividendsFromUserAnswers,
          updatedGermanyDividendsFromUserAnswers
        )
      )
      val result = ForeignIncomeSubmission.fromForeignIncomeDividends(submissionFromDownstream, userAnswers)
      val resultForeignDividends: Option[Seq[ForeignDividend]] = result.foreignDividend
      resultForeignDividends.flatMap(_.find(_.countryCode == "ESP")) shouldBe None
      resultForeignDividends.flatMap(_.find(_.countryCode == "USA")) shouldBe
        Some(usaDividendsFromUserAnswers.toForeignDividend)
      resultForeignDividends.flatMap(_.find(_.countryCode == "DEU")) shouldBe
        Some(updatedGermanyDividendsFromUserAnswers.toForeignDividend
          .copy(specialWithholdingTax = germanySpecialWithholdingTax, taxableAmount = germanyTaxableAmount))
    }
  }
}