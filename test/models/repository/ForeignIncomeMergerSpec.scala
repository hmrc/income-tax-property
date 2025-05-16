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

package models.repository

import models.repository.ForeignIncomeMerger.ForeignDividendsAnswersMerger
import models.{ForeignIncomeDividendsAnswers, ForeignIncomeDividendsStoreAnswers}
import models.repository.Merger._
import models.request.foreignincome.{GrossAmountWithReference, ForeignDividend, ForeignIncomeSubmission, ForeignDividendsAnswers}
import utils.UnitTest

class ForeignIncomeMergerSpec extends UnitTest {

  val countryCode = "USA"
  val amountBeforeTax = BigDecimal(12.34)
  val taxTakenOff = BigDecimal(56.78)
  val specialWithholdingTax = BigDecimal(90.12)
  val foreignTaxCreditRelief = true
  val taxableAmount = BigDecimal(34.56)
  val customerReference = "REFERENCE"
  val grossAmount = BigDecimal(78.90)
  val aForeignIncomeSubmission: ForeignIncomeSubmission = ForeignIncomeSubmission(
    foreignDividend = Some(
      Seq(
        ForeignDividend(
          countryCode = countryCode,
          amountBeforeTax = Some(amountBeforeTax),
          taxTakenOff = Some(taxTakenOff),
          specialWithholdingTax = Some(specialWithholdingTax),
          foreignTaxCreditRelief = Some(foreignTaxCreditRelief),
          taxableAmount = taxableAmount
        )
      )),
    dividendIncomeReceivedWhilstAbroad = Some(
      Seq(
        ForeignDividend(
          countryCode = countryCode,
          amountBeforeTax = Some(amountBeforeTax),
          taxTakenOff = Some(taxTakenOff),
          specialWithholdingTax = Some(specialWithholdingTax),
          foreignTaxCreditRelief = Some(foreignTaxCreditRelief),
          taxableAmount = taxableAmount
        )
      )),
    stockDividend = Some(
      GrossAmountWithReference(
        customerReference = Some(customerReference),
        grossAmount = grossAmount
      )
    ),
    redeemableShares = Some(
      GrossAmountWithReference(
        customerReference = Some(customerReference),
        grossAmount = grossAmount
      )
    ),
    bonusIssuesOfSecurities = Some(
      GrossAmountWithReference(
        customerReference = Some(customerReference),
        grossAmount = grossAmount
      )
    ),
    closeCompanyLoansWrittenOff = Some(
      GrossAmountWithReference(
        customerReference = Some(customerReference),
        grossAmount = grossAmount
      )
    )
  )

  "ForeignIncomeMerger" should {

    "merge foreign dividends from downstream response and from repo into response model" when {

      val fromDownstreamMaybe: Option[Map[String, ForeignDividend]] =
        for {
        foreignDividend <- aForeignIncomeSubmission.foreignDividend
      } yield foreignDividend.map(foreignDividend => foreignDividend.countryCode -> foreignDividend).toMap

      "store answers are available in the repo" in {
        val foreignIncomeDividendsAnswers = ForeignIncomeDividendsAnswers(countryCode, foreignTaxDeductedFromDividendIncome = true)
        val foreignIncomeDividendsStoreAnswers: Option[Map[String, ForeignIncomeDividendsStoreAnswers]] =
          Some(Map(countryCode -> ForeignIncomeDividendsStoreAnswers(Seq(foreignIncomeDividendsAnswers))))

        foreignIncomeDividendsStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignDividendsAnswers(
            amountBeforeTax = Some(amountBeforeTax),
            taxTakenOff = Some(taxTakenOff),
            specialWithholdingTax = Some(specialWithholdingTax),
            foreignTaxCreditRelief = Some(foreignTaxCreditRelief),
            taxableAmount = Some(taxableAmount)
          ))
        )
      }

      "store answers are not available in the repo" in {
        val foreignIncomeDividendsStoreAnswers: Option[Map[String, ForeignIncomeDividendsStoreAnswers]] = None
        foreignIncomeDividendsStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignDividendsAnswers(
            amountBeforeTax = Some(amountBeforeTax),
            taxTakenOff = Some(taxTakenOff),
            specialWithholdingTax = Some(specialWithholdingTax),
            foreignTaxCreditRelief = Some(foreignTaxCreditRelief),
            taxableAmount = Some(taxableAmount)
          ))
        )
      }
    }
  }
}
