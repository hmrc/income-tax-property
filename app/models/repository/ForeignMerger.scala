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

import models.request.foreign._
import models.request.foreign.adjustments.{ForeignUnusedResidentialFinanceCost, UnusedLossesPreviousYears}
import models.request.foreign.allowances.{CapitalAllowancesForACar, ForeignAllowancesAnswers}
import models.request.foreign.expenses.ConsolidatedExpenses
import models.request.{BalancingCharge, ForeignSbaInfo}
import models.responses._
import models.{ForeignAdjustmentsStoreAnswers, ForeignAllowancesStoreAnswers, ForeignPropertyExpensesStoreAnswers}

import java.time.LocalDate

// T: to return (merge into)
// U: saved (extract from)
// X: from downstream

object ForeignMerger {

  implicit object ForeignPropertyTaxMerger
      extends Merger[Option[Map[String, ForeignPropertyTax]], Option[
        Map[String, ForeignPropertyTaxStoreAnswers]
      ], Option[Map[String, ForeignPropertyIncome]]] {

    override def merge(
      extractedMaybe: Option[Map[String, ForeignPropertyTaxStoreAnswers]],
      fromDownstreamMaybe: Option[Map[String, ForeignPropertyIncome]]
    ): Option[Map[String, ForeignPropertyTax]] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extractedMap), Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignPropertyTax] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyIncome) =>
              countryCode -> ForeignPropertyTax(
                foreignIncomeTax = Some(
                  ForeignIncomeTax(
                    isForeignIncomeTax = extractedMap
                      .get(countryCode)
                      .flatMap(_.isForeignIncomeTax)
                      .getOrElse(foreignPropertyIncome.foreignTaxPaidOrDeducted.isDefined),
                    foreignTaxPaidOrDeducted = foreignPropertyIncome.foreignTaxPaidOrDeducted
                  )
                ),
                foreignTaxCreditRelief = foreignPropertyIncome.foreignTaxCreditRelief
              )
          }
          Option.when(result.nonEmpty)(result)
        case (_, Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignPropertyTax] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyIncome) =>
              countryCode -> ForeignPropertyTax(
                foreignIncomeTax = Some(
                  ForeignIncomeTax(
                    isForeignIncomeTax = foreignPropertyIncome.foreignTaxPaidOrDeducted.isDefined,
                    foreignTaxPaidOrDeducted = foreignPropertyIncome.foreignTaxPaidOrDeducted
                  )
                ),
                foreignTaxCreditRelief = foreignPropertyIncome.foreignTaxCreditRelief
              )
          }
          Option.when(result.nonEmpty)(result)
        case _ => None
      }
  }

  implicit object ForeignPropertySbaMerger
      extends Merger[Option[Map[String, ForeignSbaInfo]], Option[Map[String, ForeignPropertySbaStoreAnswers]], Option[
        Map[String, Option[Seq[StructuredBuildingAllowance]]]
      ]] {
    override def merge(
      extracted: Option[Map[String, ForeignPropertySbaStoreAnswers]],
      fromDownstream: Option[Map[String, Option[Seq[StructuredBuildingAllowance]]]]
    ): Option[Map[String, ForeignSbaInfo]] =
      (extracted, fromDownstream) match {
        case (Some(extractedMap), Some(downStreamMap)) =>
          val result = downStreamMap.map { case (countryCode, maybeAllowances) =>
            countryCode -> ForeignSbaInfo(
              claimStructureBuildingAllowance = extractedMap.get(countryCode).exists(_.claimStructureBuildingAllowance),
              allowances = maybeAllowances
                .map(_.map(fromSbaDownstreamToUpstream))
            )
          }
          Option(result).filter(_.nonEmpty)

        case (_, Some(downStreamMap)) =>
          val result = downStreamMap.map { case (countryCode, maybeAllowances) =>
            countryCode -> ForeignSbaInfo(
              claimStructureBuildingAllowance = maybeAllowances.isDefined,
              allowances = maybeAllowances
                .map(_.map(fromSbaDownstreamToUpstream))
            )
          }
          Option(result).filter(_.nonEmpty)

        case _ => None
      }

    private def fromSbaDownstreamToUpstream(sba: StructuredBuildingAllowance): StructuredBuildingAllowance =
      StructuredBuildingAllowance(
        amount = sba.amount,
        firstYear = Some(
          StructuredBuildingAllowanceDate(
            sba.firstYear.map(_.qualifyingDate).getOrElse(LocalDate.now()),
            sba.firstYear.map(_.qualifyingAmountExpenditure).getOrElse(0)
          )
        ),
        building = StructuredBuildingAllowanceBuilding(
          name = Some(sba.building.name.getOrElse("")),
          number = Some(sba.building.number.getOrElse("")),
          postCode = sba.building.postCode
        )
      )
  }

  implicit object ForeignPropertyIncomeMerger
      extends Merger[Option[Map[String, ForeignIncomeAnswers]], Option[Map[String, ForeignIncomeStoreAnswers]], Option[
        Map[String, ForeignPropertyIncome]
      ]] {

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
            case (countryCode, foreignPropertyIncome) =>
              countryCode -> ForeignIncomeAnswers(
                rentIncome = foreignPropertyIncome.rentIncome.map(_.rentAmount),
                premiumsGrantLeaseReceived = foreignPropertyIncome.premiumsOfLeaseGrant.isDefined,
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
      extends Merger[Option[Map[String, ForeignExpensesAnswers]], Option[
        Map[String, ForeignPropertyExpensesStoreAnswers]
      ], Option[Map[String, ForeignPropertyExpenses]]] {
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
                consolidatedExpenses = foreignPropertyExpenses.consolidatedExpense
                  .map { consolidatedExpenseAmount =>
                    ConsolidatedExpenses(
                      isConsolidatedOrIndividualExpenses = true,
                      consolidatedExpense = Some(consolidatedExpenseAmount)
                    )
                  }
                  .orElse(
                    storeAnswersMaybe.map(ce =>
                      ConsolidatedExpenses(isConsolidatedOrIndividualExpenses = ce.isConsolidatedExpenses, None)
                    )
                  ),
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
                  ConsolidatedExpenses(
                    isConsolidatedOrIndividualExpenses = true,
                    consolidatedExpense = Some(consolidatedExpenseAmount)
                  )
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

  implicit object ForeignPropertyAdjustmentsMerger
      extends Merger[Option[Map[String, ForeignAdjustmentsAnswers]], Option[
        Map[String, ForeignAdjustmentsStoreAnswers]
      ], Option[Map[String, (ForeignPropertyAdjustments, Option[BigDecimal], ForeignPropertyExpenses)]]] {
    override def merge(
      extractedMaybe: Option[Map[String, ForeignAdjustmentsStoreAnswers]],
      fromDownstreamMaybe: Option[
        Map[String, (ForeignPropertyAdjustments, Option[BigDecimal], ForeignPropertyExpenses)]
      ]
    ): Option[Map[String, ForeignAdjustmentsAnswers]] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extractedMap), Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignAdjustmentsAnswers] = fromDownstreamMap.map {
            case (countryCode, (adjustments, maybePIA, expenses)) =>
              val storeAnswersMaybe = extractedMap.get(countryCode)
              countryCode -> ForeignAdjustmentsAnswers(
                privateUseAdjustment = adjustments.privateUseAdjustment,
                balancingCharge = adjustments.balancingCharge
                  .map { balancingCharge =>
                    BalancingCharge(isBalancingCharge = true, balancingChargeAmount = Some(balancingCharge))
                  }
                  .orElse(storeAnswersMaybe.map { storeAnswers =>
                    BalancingCharge(storeAnswers.isBalancingCharge, None)
                  }),
                residentialFinanceCost = expenses.residentialFinancialCost,
                unusedResidentialFinanceCost = expenses.broughtFwdResidentialFinancialCost
                  .map { unusedResidentialFinanceCost =>
                    ForeignUnusedResidentialFinanceCost(
                      isForeignUnusedResidentialFinanceCost = true,
                      foreignUnusedResidentialFinanceCostAmount = Some(unusedResidentialFinanceCost)
                    )
                  }
                  .orElse(
                    storeAnswersMaybe.flatMap(storeAnswers =>
                      storeAnswers.isForeignUnusedResidentialFinanceCost.map(isUnusedResidentialFinanceCost =>
                        ForeignUnusedResidentialFinanceCost(isUnusedResidentialFinanceCost, None)
                      )
                    )
                  ),
                propertyIncomeAllowanceClaim = maybePIA,
                unusedLossesPreviousYears = storeAnswersMaybe.map { storeAnswers =>
                  UnusedLossesPreviousYears(storeAnswers.isUnusedLossesPreviousYears, None)
                },
                whenYouReportedTheLoss = storeAnswersMaybe.flatMap(_.whenYouReportedTheLoss)
              )
          }
          Option.when(result.nonEmpty)(result)
        case (_, Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignAdjustmentsAnswers] = fromDownstreamMap.map {
            case (countryCode, (adjustments, maybePIA, expenses)) =>
              countryCode -> ForeignAdjustmentsAnswers(
                privateUseAdjustment = adjustments.privateUseAdjustment,
                balancingCharge = adjustments.balancingCharge.map { balancingCharge =>
                  BalancingCharge(isBalancingCharge = true, balancingChargeAmount = Some(balancingCharge))
                },
                residentialFinanceCost = expenses.residentialFinancialCost,
                unusedResidentialFinanceCost =
                  expenses.broughtFwdResidentialFinancialCost.map { unusedResidentialFinanceCost =>
                    ForeignUnusedResidentialFinanceCost(
                      isForeignUnusedResidentialFinanceCost = true,
                      foreignUnusedResidentialFinanceCostAmount = Some(unusedResidentialFinanceCost)
                    )
                  },
                unusedLossesPreviousYears = None,
                propertyIncomeAllowanceClaim = maybePIA,
                whenYouReportedTheLoss = None
              )
          }
          Option.when(result.nonEmpty)(result)
        case _ => None
      }
  }

  implicit object ForeignPropertyAllowancesMerger
    extends Merger[
      Option[Map[String, ForeignAllowancesAnswers]],
      Option[Map[String, ForeignAllowancesStoreAnswers]],
      Option[Map[String, ForeignPropertyAllowances]]] {
    override def merge(
                        extractedMaybe: Option[Map[String, ForeignAllowancesStoreAnswers]],
                        fromDownstreamMaybe: Option[Map[String, ForeignPropertyAllowances]]
                      ): Option[Map[String, ForeignAllowancesAnswers]] = (extractedMaybe, fromDownstreamMaybe) match {
      case (Some(extractedMap), Some(fromDownstreamMap)) =>
        val result: Map[String, ForeignAllowancesAnswers] = fromDownstreamMap.map {
          case (countryCode, allowances) =>
            val storeAnswersMaybe = extractedMap.get(countryCode)
            countryCode -> ForeignAllowancesAnswers(
              zeroEmissionsCarAllowance = allowances.zeroEmissionsCarAllowance,
              zeroEmissionsGoodsVehicleAllowance = allowances.zeroEmissionsGoodsVehicleAllowance,
              costOfReplacingDomesticItems = allowances.costOfReplacingDomesticItems,
              otherCapitalAllowance = allowances.otherCapitalAllowance,
              annualInvestmentAllowance = allowances.annualInvestmentAllowance,
              propertyAllowance = allowances.propertyAllowance,
              electricChargePointAllowance = allowances.electricChargePointAllowance,
              structuredBuildingAllowance = allowances.structuredBuildingAllowance,
              capitalAllowancesForACar = storeAnswersMaybe.map { foreignPropertyAllowancesStoreAnswers =>
                if (foreignPropertyAllowancesStoreAnswers.isCapitalAllowancesForACar.exists(identity)) {
                  CapitalAllowancesForACar(isCapitalAllowancesForACar = true, capitalAllowancesForACarAmount = allowances.otherCapitalAllowance)
                } else {
                  CapitalAllowancesForACar(isCapitalAllowancesForACar = false, capitalAllowancesForACarAmount = None)
                }
              }
            )
        }
        Option.when(result.nonEmpty)(result)
      case (_, Some(fromDownstreamMap)) =>
        val result: Map[String, ForeignAllowancesAnswers] = fromDownstreamMap.map {
          case (countryCode, allowances) =>
            countryCode -> ForeignAllowancesAnswers(
              zeroEmissionsCarAllowance = allowances.zeroEmissionsCarAllowance,
              zeroEmissionsGoodsVehicleAllowance = allowances.zeroEmissionsGoodsVehicleAllowance,
              costOfReplacingDomesticItems = allowances.costOfReplacingDomesticItems,
              otherCapitalAllowance = allowances.otherCapitalAllowance,
              annualInvestmentAllowance = allowances.annualInvestmentAllowance,
              propertyAllowance = allowances.propertyAllowance,
              electricChargePointAllowance = allowances.electricChargePointAllowance,
              structuredBuildingAllowance = allowances.structuredBuildingAllowance,
              capitalAllowancesForACar = None
            )
        }
        Option.when(result.nonEmpty)(result)
      case _ => None
    }
  }
}
