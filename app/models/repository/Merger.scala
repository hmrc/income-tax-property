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

import models._
import models.request._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba._
import models.request.sba.{Sba, SbaInfo, SbaInfoToSave}
import models.request.ukrentaroom.RaRAdjustments
import models.responses._

import java.time.LocalDate

// T: to return (merge into)
// U: saved (extract from)
// X: from downstream
trait Merger[T, U, X] {
  def merge(extracted: U, fromDownstream: X): T
}

object Merger {

  implicit object RaRAboutMerger
      extends Merger[Option[RaRAbout], Option[IsClaimExpensesOrRRR], (Option[Boolean], Option[UkOtherProperty])] {
    override def merge(
      extracted: Option[IsClaimExpensesOrRRR],
      fromDownstream: (Option[Boolean], Option[UkOtherProperty])
    ): Option[RaRAbout] =
      fromDownstream match {
        case (Some(jointlyLet), Some(ukOtherProperty)) =>
          val amountClaimedMaybe: Option[BigDecimal] =
            ukOtherProperty.expenses.flatMap(_.ukOtherRentARoom.map(_.amountClaimed))

          Some(
            RaRAbout(
              isJointlyLet = jointlyLet,
              totalIncomeAmount = ukOtherProperty.income.flatMap(_.ukOtherRentARoom.map(_.rentsReceived)).getOrElse(0),
              claimExpensesOrRelief = ClaimExpensesOrRelief(
                extracted.fold(amountClaimedMaybe.isDefined)(_.isClaimExpensesOrRRR),
                amountClaimedMaybe
              )
            )
          )

        case _ => None
      }
  }

  implicit object RentalsAndRaRAboutMerger
      extends Merger[
        Option[RentalsAndRaRAbout],
        (
          Option[IsClaimPropertyIncomeAllowance],
          Option[IsClaimExpensesOrRRR]
        ),
        (Option[Boolean], Option[UkOtherProperty])
      ] {
    override def merge(
      extracted: (Option[IsClaimPropertyIncomeAllowance], Option[IsClaimExpensesOrRRR]),
      fromDownstream: (Option[Boolean], Option[UkOtherProperty])
    ): Option[RentalsAndRaRAbout] = {
      val (isClaimPropertyIncomeAllowance, isClaimExpensesOrRRR) = extracted
      fromDownstream match {
        case (Some(jointlyLet), Some(ukOtherProperty)) =>
          val amountClaimedMaybe: Option[BigDecimal] =
            ukOtherProperty.expenses.flatMap(_.ukOtherRentARoom.map(_.amountClaimed))
          Some(
            RentalsAndRaRAbout(
              isJointlyLet = jointlyLet,
              totalIncomeAmount = ukOtherProperty.income.flatMap(_.ukOtherRentARoom.map(_.rentsReceived)).getOrElse(0),
              claimExpensesOrRelief = ClaimExpensesOrRelief(
                isClaimExpensesOrRRR.fold(amountClaimedMaybe.isDefined)(_.isClaimExpensesOrRRR),
                amountClaimedMaybe
              ),
              isClaimPropertyIncomeAllowance =
                isClaimPropertyIncomeAllowance.map(_.isClaimPropertyIncomeAllowance).getOrElse(false),
              propertyRentalIncome = ukOtherProperty.income.flatMap(x => x.periodAmount).getOrElse(0)
            )
          )
        case _ => None
      }
    }
  }

  implicit object RentalsExpensesMerger
      extends Merger[Option[PropertyRentalsExpense], Option[ExpensesStoreAnswers], Option[UkOtherPropertyExpenses]] {
    override def merge(
      extractedMaybe: Option[ExpensesStoreAnswers],
      fromDownstreamMaybe: Option[UkOtherPropertyExpenses]
    ): Option[PropertyRentalsExpense] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extracted), Some(fromDownstream)) =>
          Some(
            PropertyRentalsExpense(
              consolidatedExpenses = fromDownstream.consolidatedExpenses.map(ce => // Todo: Should Be Made Optional
                ConsolidatedExpenses(extracted.isConsolidatedExpenses, Some(ce))
              ),
              rentsRatesAndInsurance = fromDownstream.premisesRunningCosts,
              repairsAndMaintenanceCosts = fromDownstream.repairsAndMaintenance,
              loanInterestOrOtherFinancialCost = fromDownstream.financialCosts,
              otherProfessionalFees = fromDownstream.professionalFees,
              costsOfServicesProvided = fromDownstream.costOfServices,
              propertyBusinessTravelCosts = fromDownstream.travelCosts,
              otherAllowablePropertyExpenses = fromDownstream.other
            )
          )
        case (_, Some(fromDownstream)) =>
          Some(
            PropertyRentalsExpense(
              consolidatedExpenses = fromDownstream.consolidatedExpenses.map(ce =>
                ConsolidatedExpenses(isConsolidatedExpenses = true, Some(ce))
              ),
              rentsRatesAndInsurance = fromDownstream.premisesRunningCosts,
              repairsAndMaintenanceCosts = fromDownstream.repairsAndMaintenance,
              loanInterestOrOtherFinancialCost = fromDownstream.financialCosts,
              otherProfessionalFees = fromDownstream.professionalFees,
              costsOfServicesProvided = fromDownstream.costOfServices,
              propertyBusinessTravelCosts = fromDownstream.travelCosts,
              otherAllowablePropertyExpenses = fromDownstream.other
            )
          )
        case _ => None
      }
  }

  implicit object RentARoomExpensesMerger
      extends Merger[
        Option[RentARoomExpenses],
        Option[RentARoomExpensesStoreAnswers],
        Option[UkOtherPropertyExpenses]
      ] {
    override def merge(
      extractedMaybe: Option[RentARoomExpensesStoreAnswers],
      fromDownstreamMaybe: Option[UkOtherPropertyExpenses]
    ): Option[RentARoomExpenses] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extracted), Some(fromDownstream)) =>
          Some(
            RentARoomExpenses(
              consolidatedExpenses = fromDownstream.consolidatedExpenses.map(ce => // Todo: Should Be Made Optional
                ConsolidatedExpenses(extracted.isConsolidatedExpenses, Some(ce))
              ),
              rentsRatesAndInsurance = fromDownstream.premisesRunningCosts,
              repairsAndMaintenanceCosts = fromDownstream.repairsAndMaintenance,
              legalManagementOtherFee = fromDownstream.professionalFees,
              costOfServicesProvided = fromDownstream.costOfServices,
              otherPropertyExpenses = fromDownstream.other
            )
          )
        case (_, Some(fromDownstream)) =>
          Some(
            RentARoomExpenses(
              consolidatedExpenses = fromDownstream.consolidatedExpenses.map(ce => // Todo: Should Be Made Optional
                ConsolidatedExpenses(isConsolidatedExpenses = true, Some(ce))
              ),
              rentsRatesAndInsurance = fromDownstream.premisesRunningCosts,
              repairsAndMaintenanceCosts = fromDownstream.repairsAndMaintenance,
              legalManagementOtherFee = fromDownstream.professionalFees,
              costOfServicesProvided = fromDownstream.costOfServices,
              otherPropertyExpenses = fromDownstream.other
            )
          )
        case _ => None
      }
  }

  implicit object RentalsIncomeMerger
      extends Merger[Option[PropertyRentalsIncome], Option[StoredIncome], Option[UkOtherPropertyIncome]] {
    override def merge(
      extractedMaybe: Option[StoredIncome],
      fromDownstreamMaybe: Option[UkOtherPropertyIncome]
    ): Option[PropertyRentalsIncome] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extracted), Some(fromDownstream)) =>
          Some(
            PropertyRentalsIncome(
              isNonUKLandlord = extracted.isNonUKLandlord,
              propertyRentalIncome = fromDownstream.periodAmount.getOrElse(0),
              otherIncomeFromProperty = fromDownstream.otherIncome.getOrElse(0),
              deductingTax =
                fromDownstream.taxDeducted.map(amount => DeductingTax(isTaxDeducted = true, Some(amount))),
              calculatedFigureYourself = extracted.calculatedFigureYourself,
              yearLeaseAmount = extracted.receivedGrantLeaseAmount,
              receivedGrantLeaseAmount = extracted.yearLeaseAmount,
              premiumsGrantLease = fromDownstream.premiumsOfLeaseGrant.map(polg =>
                PremiumsGrantLease(premiumsGrantLeaseReceived = true, Some(polg))
              ),
              reversePremiumsReceived = fromDownstream.reversePremiums.map(rp =>
                ReversePremiumsReceived(reversePremiumsReceived = true, Some(rp))
              )
            )
          )
        case (None, Some(fromDownstream)) =>
          Some(
            PropertyRentalsIncome(
              isNonUKLandlord = false,
              propertyRentalIncome = fromDownstream.periodAmount.getOrElse(0),
              otherIncomeFromProperty = fromDownstream.otherIncome.getOrElse(0),
              deductingTax =
                fromDownstream.taxDeducted.map(amount => DeductingTax(isTaxDeducted = true, Some(amount))),
              calculatedFigureYourself = None,
              yearLeaseAmount = None,
              receivedGrantLeaseAmount = None,
              premiumsGrantLease = fromDownstream.premiumsOfLeaseGrant.map(polg =>
                PremiumsGrantLease(premiumsGrantLeaseReceived = true, Some(polg))
              ),
              reversePremiumsReceived = fromDownstream.reversePremiums.map(rp =>
                ReversePremiumsReceived(reversePremiumsReceived = true, Some(rp))
              )
            )
          )
        case _ => None
      }
  }

  implicit object RentalsAndRaRIncomeMerger
      extends Merger[Option[RentalsAndRaRIncome], Option[StoredIncome], Option[UkOtherPropertyIncome]] {
    override def merge(
      extractedMaybe: Option[StoredIncome],
      fromDownstreamMaybe: Option[UkOtherPropertyIncome]
    ): Option[RentalsAndRaRIncome] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extracted), Some(fromDownstream)) =>
          Some(
            RentalsAndRaRIncome(
              isNonUKLandlord = extracted.isNonUKLandlord,
              otherIncomeFromProperty = fromDownstream.otherIncome.getOrElse(0),
              deductingTax =
                fromDownstream.taxDeducted.map(amount => DeductingTax(isTaxDeducted = true, Some(amount))),
              calculatedFigureYourself = extracted.calculatedFigureYourself,
              yearLeaseAmount = extracted.receivedGrantLeaseAmount,
              receivedGrantLeaseAmount = extracted.yearLeaseAmount,
              premiumsGrantLease = fromDownstream.premiumsOfLeaseGrant.map(polg =>
                PremiumsGrantLease(premiumsGrantLeaseReceived = true, Some(polg))
              ),
              reversePremiumsReceived = fromDownstream.reversePremiums.map(rp =>
                ReversePremiumsReceived(reversePremiumsReceived = true, Some(rp))
              )
            )
          )
        case (None, Some(fromDownstream)) =>
          Some(
            RentalsAndRaRIncome(
              isNonUKLandlord = false,
              otherIncomeFromProperty = fromDownstream.otherIncome.getOrElse(0),
              deductingTax =
                fromDownstream.taxDeducted.map(amount => DeductingTax(isTaxDeducted = true, Some(amount))),
              calculatedFigureYourself = None,
              yearLeaseAmount = None,
              receivedGrantLeaseAmount = None,
              premiumsGrantLease = fromDownstream.premiumsOfLeaseGrant.map(polg =>
                PremiumsGrantLease(premiumsGrantLeaseReceived = true, Some(polg))
              ),
              reversePremiumsReceived = fromDownstream.reversePremiums.map(rp =>
                ReversePremiumsReceived(reversePremiumsReceived = true, Some(rp))
              )
            )
          )
        case _ => None
      }
  }

  implicit object AllowancesMerger
      extends Merger[Option[RentalAllowances], Option[RentalAllowancesStoreAnswers], Option[UkOtherAllowances]] {
    override def merge(
      extractedMaybe: Option[RentalAllowancesStoreAnswers],
      fromDownstreamMaybe: Option[UkOtherAllowances]
    ): Option[RentalAllowances] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extracted), Some(fromDownstream)) =>
          Some(
            RentalAllowances(
              capitalAllowancesForACar = Option.when(extracted.isCapitalAllowancesForACar)(
                CapitalAllowancesForACar(isCapitalAllowancesForACar = true,
                  capitalAllowancesForACarAmount = fromDownstream.otherCapitalAllowance)
              ),
              annualInvestmentAllowance = fromDownstream.annualInvestmentAllowance,
              zeroEmissionCarAllowance = fromDownstream.zeroEmissionsCarAllowance,
              zeroEmissionGoodsVehicleAllowance = fromDownstream.zeroEmissionGoodsVehicleAllowance,
              businessPremisesRenovationAllowance = fromDownstream.businessPremisesRenovationAllowance,
              replacementOfDomesticGoodsAllowance = fromDownstream.costOfReplacingDomesticGoods
                .orElse(fromDownstream.costOfReplacingDomesticItems),
              otherCapitalAllowance = fromDownstream.otherCapitalAllowance
            )
          )
        case (None, Some(fromDownstream)) =>
          Some(
            RentalAllowances(
              capitalAllowancesForACar = fromDownstream.otherCapitalAllowance.map(amount =>
                CapitalAllowancesForACar(isCapitalAllowancesForACar = true, Some(amount))
              ),
              annualInvestmentAllowance = fromDownstream.annualInvestmentAllowance,
              zeroEmissionCarAllowance = fromDownstream.zeroEmissionsCarAllowance,
              zeroEmissionGoodsVehicleAllowance = fromDownstream.zeroEmissionGoodsVehicleAllowance,
              businessPremisesRenovationAllowance = fromDownstream.businessPremisesRenovationAllowance,
              replacementOfDomesticGoodsAllowance = fromDownstream.costOfReplacingDomesticGoods
                .orElse(fromDownstream.costOfReplacingDomesticItems),
              otherCapitalAllowance = fromDownstream.otherCapitalAllowance
            )
          )
        case (Some(extracted), None) =>
          Some(
            RentalAllowances(
              capitalAllowancesForACar = Some(CapitalAllowancesForACar(extracted.isCapitalAllowancesForACar, None)),
              annualInvestmentAllowance = None,
              zeroEmissionCarAllowance = None,
              zeroEmissionGoodsVehicleAllowance = None,
              businessPremisesRenovationAllowance = None,
              replacementOfDomesticGoodsAllowance = None,
              otherCapitalAllowance = None
            )
          )
        case _ => None
      }
  }

  implicit object RaRAllowancesMerger
      extends Merger[Option[RentARoomAllowances], Option[RentARoomAllowancesStoreAnswers], Option[UkOtherAllowances]] {
    override def merge(
      extractedMaybe: Option[RentARoomAllowancesStoreAnswers],
      fromDownstreamMaybe: Option[UkOtherAllowances]
    ): Option[RentARoomAllowances] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (_, Some(fromDownstream)) =>
          Some(
            RentARoomAllowances(
              zeroEmissionCarAllowance = fromDownstream.zeroEmissionsCarAllowance,
              zeroEmissionGoodsVehicleAllowance = fromDownstream.zeroEmissionGoodsVehicleAllowance,
              replacementOfDomesticGoodsAllowance = fromDownstream.costOfReplacingDomesticGoods
                .orElse(fromDownstream.costOfReplacingDomesticItems),
              otherCapitalAllowance = fromDownstream.otherCapitalAllowance,
              capitalAllowancesForACar = fromDownstream.otherCapitalAllowance.map(amount =>
                CapitalAllowancesForACar(isCapitalAllowancesForACar = true, Some(amount))
              )
            )
          )
        case (Some(extracted), _) =>
          Some(
            RentARoomAllowances(
              capitalAllowancesForACar = Some(CapitalAllowancesForACar(extracted.isRaRCapitalAllowancesForACar, None)),
              zeroEmissionCarAllowance = None,
              zeroEmissionGoodsVehicleAllowance = None,
              replacementOfDomesticGoodsAllowance = None,
              otherCapitalAllowance = None
            )
          )
        case _ => None
      }
  }

  implicit object AdjustmentsMerger
      extends Merger[Option[PropertyRentalAdjustments], Option[AdjustmentStoreAnswers], Option[
        (UkOtherAdjustments, UkOtherPropertyExpenses)
      ]] {
    override def merge(
      extractedMaybe: Option[AdjustmentStoreAnswers],
      fromDownstreamMaybe: Option[(UkOtherAdjustments, UkOtherPropertyExpenses)]
    ): Option[PropertyRentalAdjustments] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (
              _,
              Some(
                (
                  fromDownstreamAdjustment,
                  UkOtherPropertyExpenses(
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    Some(residentialFinanceCost),
                    Some(residentialFinanceCostCarriedForward),
                    _,
                    _
                  )
                )
              )
            ) =>
          Some(
            PropertyRentalAdjustments(
              privateUseAdjustment = fromDownstreamAdjustment.privateUseAdjustment.get,
              balancingCharge = BalancingCharge(
                isBalancingCharge = extractedMaybe
                  .map(_.isBalancingCharge)
                  .getOrElse(!fromDownstreamAdjustment.balancingCharge.isEmpty),
                balancingChargeAmount = fromDownstreamAdjustment.balancingCharge
              ),
              propertyIncomeAllowance =
                None, // Todo: fromWhere? (Change case class to make it optional, present in one of Rentals / RentaRoom)
              renovationAllowanceBalancingCharge = RenovationAllowanceBalancingCharge(
                isRenovationAllowanceBalancingCharge = extractedMaybe
                  .map(_.isRenovationAllowanceBalancingCharge)
                  .getOrElse(
                    fromDownstreamAdjustment.businessPremisesRenovationAllowanceBalancingCharges.isDefined
                  ),
                renovationAllowanceBalancingChargeAmount =
                  fromDownstreamAdjustment.businessPremisesRenovationAllowanceBalancingCharges
              ),
              residentialFinanceCost = residentialFinanceCost,
              unusedResidentialFinanceCost = Some(residentialFinanceCostCarriedForward),
              unusedLossesBroughtForward = UnusedLossesBroughtForward(
                isUnusedLossesBroughtForward = extractedMaybe
                  .map(_.isUnusedLossesBroughtForward)
                  .getOrElse(!fromDownstreamAdjustment.lossBroughtForward.isEmpty),
                unusedLossesBroughtForwardAmount = fromDownstreamAdjustment.lossBroughtForward
              ),
              whenYouReportedTheLoss = fromDownstreamAdjustment.whenYouReportedTheLoss
            )
          )
        case _ => None
      }
  }

  implicit object RaRAdjustmentsMerger
      extends Merger[Option[RaRAdjustments], Option[RentARoomAdjustmentsStoreAnswers], Option[
        (UkOtherAdjustments, UkOtherPropertyExpenses)
      ]] {
    override def merge(
      extractedMaybe: Option[RentARoomAdjustmentsStoreAnswers],
      fromDownstreamMaybe: Option[(UkOtherAdjustments, UkOtherPropertyExpenses)]
    ): Option[RaRAdjustments] =
      (extractedMaybe, fromDownstreamMaybe) match {

        case (
              _,
              Some(
                (
                  fromDownstreamAdjustment,
                  UkOtherPropertyExpenses(
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    _,
                    residentialFinanceCostCarriedForward,
                    _,
                    _
                  )
                )
              )
            ) =>
          Some(
            RaRAdjustments(
              balancingCharge = Some(
                BalancingCharge(
                  isBalancingCharge = extractedMaybe
                    .flatMap(_.balancingCharge.map(_.isBalancingCharge))
                    .getOrElse(fromDownstreamAdjustment.balancingCharge.isDefined),
                  balancingChargeAmount = fromDownstreamAdjustment.balancingCharge
                )
              ),
              unusedResidentialPropertyFinanceCostsBroughtFwd = residentialFinanceCostCarriedForward,
              unusedLossesBroughtForward = Some(
                UnusedLossesBroughtForward(
                  isUnusedLossesBroughtForward = extractedMaybe
                    .flatMap(_.unusedLossesBroughtForward.map(_.isUnusedLossesBroughtForward))
                    .getOrElse(fromDownstreamAdjustment.lossBroughtForward.isDefined),
                  unusedLossesBroughtForwardAmount = fromDownstreamAdjustment.lossBroughtForward
                )
              ),
              whenYouReportedTheLoss = fromDownstreamAdjustment.whenYouReportedTheLoss
            )
          )
        case _ => None
      }
  }

  implicit object EsbaMerger extends Merger[Option[EsbaInfo], Option[EsbaInfoToSave], Option[List[EsbaInUpstream]]] {

    override def merge(
      extractedMaybe: Option[EsbaInfoToSave],
      fromDownstreamMaybe: Option[List[EsbaInUpstream]]
    ): Option[EsbaInfo] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extracted), Some(fromDownstream)) =>
          Some(
            EsbaInfo(
              claimEnhancedStructureBuildingAllowance = extracted.claimEnhancedStructureBuildingAllowance,
              enhancedStructureBuildingAllowanceClaims = extracted.esbaClaims,
              enhancedStructureBuildingAllowances = fromDownstream
            )
          )
        case (None, Some(fromDownstream)) => // Todo: How to act here???
          Some(
            EsbaInfo(
              claimEnhancedStructureBuildingAllowance = true,
              enhancedStructureBuildingAllowanceClaims = Some(false), // ToDo:???
              enhancedStructureBuildingAllowances = fromDownstream
            )
          )
        case (Some(extracted), _) => Some(EsbaInfo(
          claimEnhancedStructureBuildingAllowance = extracted.claimEnhancedStructureBuildingAllowance,
          enhancedStructureBuildingAllowanceClaims = extracted.esbaClaims,
          enhancedStructureBuildingAllowances = List.empty
        ))
        case _ => None
      }

    // Todo: Change Above after asking questiins!!!!

  }

  implicit object SbaMerger
      extends Merger[Option[SbaInfo], Option[SbaInfoToSave], Option[List[StructuredBuildingAllowance]]] {

    override def merge(
      extractedMaybe: Option[SbaInfoToSave],
      fromDownstreamMaybe: Option[List[StructuredBuildingAllowance]]
    ): Option[SbaInfo] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extracted), Some(fromDownstream)) =>
            Some(SbaInfo(
              claimStructureBuildingAllowance = extracted.claimStructureBuildingAllowance,
              structureBuildingFormGroup = fromDownstream.map(fromSbaDownstreamToUpstream)
            ))
        case (None, Some(fromDownstream)) =>
          Some(
            SbaInfo(
              claimStructureBuildingAllowance = true,
              structureBuildingFormGroup = fromDownstream.map(fromSbaDownstreamToUpstream)
            )
          )
        case (Some(extracted), _) => Some(SbaInfo(
          claimStructureBuildingAllowance = extracted.claimStructureBuildingAllowance,
          structureBuildingFormGroup = List.empty
        ))
        case _ => None
      }

    private def fromSbaDownstreamToUpstream(sba: StructuredBuildingAllowance): Sba =
      Sba(
        structureBuildingQualifyingDate = sba.firstYear.map(_.qualifyingDate).getOrElse(LocalDate.now()), // Todo
        structureBuildingQualifyingAmount = sba.amount,
        structureBuildingAllowanceClaim = sba.firstYear
          .map(_.qualifyingAmountExpenditure)
          .getOrElse(0), // ToDo: Check first optional and fail maybe?
        structuredBuildingAllowanceAddress = Address(
          BuildingName(sba.building.name.getOrElse("")),
          BuildingNumber(sba.building.number.getOrElse("")),
          Postcode(sba.building.postCode)
        )
      )
  }

  implicit class GeneralMerger[T, U, X](extracted: U) {

    def merge(
      fromDownstream: X
    )(implicit Merger: Merger[T, U, X]): T =
      Merger.merge(extracted, fromDownstream)
  }

}
