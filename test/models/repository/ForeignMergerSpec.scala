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

package models.repository

import models.repository.ForeignMerger._
import models.repository.Merger._
import models.request.foreign.{ForeignIncomeTax, ForeignPropertyTax}
import models.responses._
import utils.UnitTest

import java.time.LocalDate

class ForeignMergerSpec extends UnitTest {
  "ForeignMerger" should {
    val countryCode = "USA"
    val foreignIncomeTaxYesOrNo = true
    val foreignTaxPaidOrDeducted = Some(BigDecimal(56.78))
    val foreignTaxCreditRelief = Some(true)
    val aPropertyPeriodicSubmission = PropertyPeriodicSubmission(
      submissionId = None,
      submittedOn = None,
      fromDate = LocalDate.now(),
      toDate = LocalDate.now(),
      foreignProperty = Some(Seq(
        ForeignProperty(
          countryCode = countryCode,
          income = Some(ForeignPropertyIncome(
            rentIncome = Some(ForeignPropertyRentIncome(rentAmount = BigDecimal(12.34))),
            foreignTaxCreditRelief = foreignTaxCreditRelief,
            premiumsOfLeaseGrant = Some(BigDecimal(13.34)),
            otherPropertyIncome = Some(BigDecimal(24.56)),
            foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted,
            specialWithholdingTaxOrUkTaxPaid = Some(BigDecimal(89.10))
          )),
          expenses = Some(ForeignPropertyExpenses(
            premisesRunningCosts = Some(BigDecimal(23.34)),
            repairsAndMaintenance = Some(BigDecimal(32.21)),
            financialCosts = Some(BigDecimal(54.32)),
            professionalFees = Some(BigDecimal(65.43)),
            travelCosts = Some(BigDecimal(22.22)),
            costOfServices = Some(BigDecimal(10.10)),
            residentialFinancialCost = Some(BigDecimal(11.11)),
            broughtFwdResidentialFinancialCost = Some(BigDecimal(23.22)),
            other = Some(BigDecimal(44.44)),
            consolidatedExpense = Some(BigDecimal(90.05)),
            None
          ))
        )
      )),
      ukOtherProperty = None
    )

    "merge foreign tax from downstream response and from repo into response model" in {
      val foreignPropertyTaxStoreAnswers: Option[Map[String, ForeignPropertyTaxStoreAnswers]] =
        Some(Map(countryCode -> ForeignPropertyTaxStoreAnswers(Some(foreignIncomeTaxYesOrNo))))

      val fromDownstreamMaybe: Option[Map[String, ForeignPropertyIncome]] = for {
        foreignProperties     <- aPropertyPeriodicSubmission.foreignProperty
        foreignProperty       <- foreignProperties.headOption
        foreignPropertyIncome <- foreignProperty.income
      } yield Map(countryCode -> foreignPropertyIncome)

      foreignPropertyTaxStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
        Map(countryCode -> ForeignPropertyTax(
          foreignIncomeTax = Some(ForeignIncomeTax(
            foreignIncomeTaxYesNo = foreignIncomeTaxYesOrNo,
            foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted
          )),
          foreignTaxCreditRelief = foreignTaxCreditRelief
        ))
      )
    }
  }
}
