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

import models.RentalAllowancesStoreAnswers
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba._
import models.request.sba.{ClaimStructureBuildingAllowance, SbaInfo, SbaInfoToSave, StructureBuildingFormGroup}
import models.request._
import models.responses._

import java.time.LocalDate

// T: to return (merge into)
// U: saved (extract from)
// X: from downstream
trait Merger[T, U, X] {
  def merge(extracted: U, fromDownstream: X): T
}

object Merger {
  implicit object AllowancesMerger
      extends Merger[Option[Allowances], Option[RentalAllowancesStoreAnswers], Option[UkOtherAllowances]] {
    override def merge(
      extractedMaybe: Option[RentalAllowancesStoreAnswers],
      fromDownstreamMaybe: Option[UkOtherAllowances]
    ): Option[Allowances] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extracted), Some(fromDownstream)) =>
          Some(
            Allowances(
              fromDownstream.annualInvestmentAllowance,
              Some(ElectricChargePointAllowance(
                extracted.electricChargePointAllowance.fold(false)(_.electricChargePointAllowanceYesOrNo),
                fromDownstream.electricChargePointAllowance
              )),
              fromDownstream.zeroEmissionsCarAllowance,
              fromDownstream.zeroEmissionGoodsVehicleAllowance,
              fromDownstream.businessPremisesRenovationAllowance,
              fromDownstream.costOfReplacingDomesticGoods,
              extracted.capitalAllowancesForACar.map(_.capitalAllowancesForACarYesNo).map{
              value =>
                if(value){
                  CapitalAllowancesForACar(true, fromDownstream.otherCapitalAllowance)
                } else {
                  CapitalAllowancesForACar(false, None)
                }
              },
              fromDownstream.otherCapitalAllowance
            )
          )
        case (None, Some(fromDownstream)) =>
          Some(
            Allowances(
              fromDownstream.annualInvestmentAllowance,
              Some(ElectricChargePointAllowance(
                fromDownstream.electricChargePointAllowance.fold(false)(_ => true),
                fromDownstream.electricChargePointAllowance
              )),
              fromDownstream.zeroEmissionsCarAllowance,
              fromDownstream.zeroEmissionGoodsVehicleAllowance,
              fromDownstream.businessPremisesRenovationAllowance,
              fromDownstream.costOfReplacingDomesticGoods,
              //TODO What happens when to the allowanceForACar when we do not have a value from the extracted?
              None,
              fromDownstream.otherCapitalAllowance
            )
          )
        case _ => None
      }
  }
  implicit object AdjustmentsMerger
      extends Merger[Option[PropertyRentalAdjustments], Option[AdjustmentStoreAnswers], Option[
        (UkOtherAdjustments, UkOtherPropertyExpenses)
      ]]{
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
              fromDownstreamAdjustment.privateUseAdjustment.get,
              BalancingCharge(
                extractedMaybe.map(_.balancingChargeYesNo).getOrElse(!fromDownstreamAdjustment.balancingCharge.isEmpty),
                fromDownstreamAdjustment.balancingCharge
              ),
              0, // Todo: fromWhere?
              RenovationAllowanceBalancingCharge(
                extractedMaybe
                  .map(_.renovationAllowanceBalancingChargeYesNo)
                  .getOrElse(
                    !fromDownstreamAdjustment.businessPremisesRenovationAllowanceBalancingCharges.isEmpty
                  ),
                fromDownstreamAdjustment.businessPremisesRenovationAllowanceBalancingCharges
              ),
              residentialFinanceCost,
              residentialFinanceCostCarriedForward
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
              extracted.claimEnhancedStructureBuildingAllowance,
              extracted.esbaClaims,
              fromDownstream
            )
          )
        case (None, Some(fromDownstream)) => // Todo: How to act here???
          Some(
            EsbaInfo(
              ClaimEnhancedStructureBuildingAllowance(true),
              EsbaClaims(false), // ToDo:???
              fromDownstream
            )
          )
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
          Some(
            SbaInfo(
              extracted.claimStructureBuildingAllowance,
              fromDownstream.map(fromSbaDownstreamToUpstream(_))
            )
          )
        case (None, Some(fromDownstream)) =>
          Some(
            SbaInfo(
              ClaimStructureBuildingAllowance(true),
              fromDownstream.map(fromSbaDownstreamToUpstream(_))
            )
          )
        case _ => None
      }

    private def fromSbaDownstreamToUpstream(sba: StructuredBuildingAllowance): StructureBuildingFormGroup =
      StructureBuildingFormGroup(
        sba.firstYear.map(_.qualifyingDate).getOrElse(LocalDate.now()), // Todo
        sba.amount,
        sba.firstYear
          .map(_.qualifyingAmountExpenditure)
          .getOrElse(0), // ToDo: Check first optional and fail maybe?
        Address(
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
