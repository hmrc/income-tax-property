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

import models.ForeignPropertyExpensesStoreAnswers
import models.request.ReversePremiumsReceived
import models.request.foreign._
import models.request.foreign.expenses.ConsolidatedExpenses
import models.responses._

// T: to return (merge into)
// U: saved (extract from)
// X: from downstream

object ForeignMerger {

  implicit object ForeignPropertyTaxMerger
    extends Merger[Option[Map[String, ForeignPropertyTax]], Option[Map[String, ForeignPropertyTaxStoreAnswers]], Option[Map[String, ForeignPropertyIncome]]] {

    override def merge(
      extractedMaybe: Option[Map[String, ForeignPropertyTaxStoreAnswers]],
      fromDownstreamMaybe: Option[Map[String, ForeignPropertyIncome]]
    ): Option[Map[String, ForeignPropertyTax]] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extractedMap), Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignPropertyTax] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyIncome) => countryCode -> ForeignPropertyTax(
              foreignIncomeTax = Some(ForeignIncomeTax(
                foreignIncomeTaxYesNo = extractedMap.get(countryCode)
                  .flatMap(_.foreignIncomeTaxYesNo)
                  .getOrElse(foreignPropertyIncome.foreignTaxPaidOrDeducted.isDefined),
                foreignTaxPaidOrDeducted = foreignPropertyIncome.foreignTaxPaidOrDeducted
              )),
              foreignTaxCreditRelief = foreignPropertyIncome.foreignTaxCreditRelief
            )
          }
          Option.when(result.nonEmpty)(result)
        case (_, Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignPropertyTax] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyIncome) => countryCode -> ForeignPropertyTax(
              foreignIncomeTax = Some(ForeignIncomeTax(
                foreignIncomeTaxYesNo = foreignPropertyIncome.foreignTaxPaidOrDeducted.isDefined,
                foreignTaxPaidOrDeducted = foreignPropertyIncome.foreignTaxPaidOrDeducted
              )),
              foreignTaxCreditRelief = foreignPropertyIncome.foreignTaxCreditRelief
            )
          }
          Option.when(result.nonEmpty)(result)
        case _ => None
      }
  }

  implicit object ForeignPropertyIncomeMerger
    extends Merger[Option[Map[String, ForeignIncomeAnswers]], Option[Map[String, ForeignIncomeStoreAnswers]], Option[Map[String, ForeignPropertyIncome]]] {

    override def merge(
      extractedMaybe: Option[Map[String, ForeignIncomeStoreAnswers]],
      fromDownstreamMaybe: Option[Map[String, ForeignPropertyIncome]]
    ): Option[Map[String, ForeignIncomeAnswers]] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extractedMap), Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignIncomeAnswers] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyIncome) =>
              val storeAnswersMaybe = extractedMap.get(countryCode)
              countryCode -> ForeignIncomeAnswers(
                rentIncome = foreignPropertyIncome.rentIncome.map(_.rentAmount),
                premiumsGrantLeaseReceived = storeAnswersMaybe
                  .map(_.premiumsGrantLeaseReceived)
                  .getOrElse(foreignPropertyIncome.premiumsOfLeaseGrant.isDefined),
                reversePremiumsReceived = storeAnswersMaybe.map(answers =>
                  ReversePremiumsReceived(answers.reversePremiumsReceived, None)
                ),
                otherPropertyIncome = foreignPropertyIncome.otherPropertyIncome,
                calculatedPremiumLeaseTaxable = storeAnswersMaybe.map(storeAnswers =>
                  CalculatedPremiumLeaseTaxable(storeAnswers.calculatedPremiumLeaseTaxable, None)
                ),
                receivedGrantLeaseAmount = storeAnswersMaybe.flatMap(_.receivedGrantLeaseAmount),
                twelveMonthPeriodsInLease = storeAnswersMaybe.flatMap(_.twelveMonthPeriodsInLease),
                premiumsOfLeaseGrantAgreed = foreignPropertyIncome.premiumsOfLeaseGrant.map(premiumsOfLeaseGrant =>
                  PremiumsOfLeaseGrantAgreed(
                    premiumsOfLeaseGrantAgreed = true,
                    premiumsOfLeaseGrant = Some(premiumsOfLeaseGrant)
                  )
                )
            )
          }
          Option.when(result.nonEmpty)(result)
        case (_, Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignIncomeAnswers] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyIncome) => countryCode -> ForeignIncomeAnswers(
              rentIncome = foreignPropertyIncome.rentIncome.map(_.rentAmount),
              premiumsGrantLeaseReceived = foreignPropertyIncome.premiumsOfLeaseGrant.isDefined,
              reversePremiumsReceived = None,
              otherPropertyIncome = foreignPropertyIncome.otherPropertyIncome,
              calculatedPremiumLeaseTaxable = None,
              receivedGrantLeaseAmount = None,
              twelveMonthPeriodsInLease = None,
              premiumsOfLeaseGrantAgreed = foreignPropertyIncome.premiumsOfLeaseGrant.map(premiumsOfLeaseGrant =>
                PremiumsOfLeaseGrantAgreed(
                  premiumsOfLeaseGrantAgreed = true,
                  premiumsOfLeaseGrant = Some(premiumsOfLeaseGrant)
                )
              )
            )
          }
          Option.when(result.nonEmpty)(result)
        case _ => None
      }
  }

  implicit object ForeignPropertyExpensesMerger
    extends Merger[Option[Map[String, ForeignExpensesAnswers]], Option[Map[String, ForeignPropertyExpensesStoreAnswers]],
      Option[Map[String, ForeignPropertyExpenses]]] {
    override def merge(
                        extractedMaybe: Option[Map[String, ForeignPropertyExpensesStoreAnswers]],
                        fromDownstreamMaybe: Option[Map[String, ForeignPropertyExpenses]]
                      ): Option[Map[String, ForeignExpensesAnswers]] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extractedMap), Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignExpensesAnswers] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyExpenses) =>
              val storeAnswersMaybe = extractedMap.get(countryCode)
              countryCode -> ForeignExpensesAnswers(
                consolidatedExpenses = foreignPropertyExpenses.consolidatedExpense.map { consolidatedExpenseAmount =>
                  ConsolidatedExpenses(consolidatedOrIndividualExpensesYesNo = true,
                    consolidatedExpense = Some(consolidatedExpenseAmount))
                }.orElse(storeAnswersMaybe.map(ce =>
                  ConsolidatedExpenses(consolidatedOrIndividualExpensesYesNo = ce.consolidatedExpensesYesOrNo, None))),
                premisesRunningCosts = foreignPropertyExpenses.premisesRunningCosts,
                repairsAndMaintenance = foreignPropertyExpenses.repairsAndMaintenance,
                financialCosts = foreignPropertyExpenses.financialCosts,
                professionalFees = foreignPropertyExpenses.professionalFees,
                costOfServices = foreignPropertyExpenses.costOfServices,
                other = foreignPropertyExpenses.other
              )
          }
          Option.when(result.nonEmpty)(result)
        case (_, Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignExpensesAnswers] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyExpenses) =>
              countryCode -> ForeignExpensesAnswers(
                consolidatedExpenses = foreignPropertyExpenses.consolidatedExpense.map { consolidatedExpenseAmount =>
                  ConsolidatedExpenses(consolidatedOrIndividualExpensesYesNo = true,
                    consolidatedExpense = Some(consolidatedExpenseAmount))
                },
                premisesRunningCosts = foreignPropertyExpenses.premisesRunningCosts,
                repairsAndMaintenance = foreignPropertyExpenses.repairsAndMaintenance,
                financialCosts = foreignPropertyExpenses.financialCosts,
                professionalFees = foreignPropertyExpenses.professionalFees,
                costOfServices = foreignPropertyExpenses.costOfServices,
                other = foreignPropertyExpenses.other
              )
          }
          Option.when(result.nonEmpty)(result)
        case _ => None
      }
  }
}
