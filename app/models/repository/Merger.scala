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

import models.request.esba._
import models.request.{BalancingCharge, PropertyRentalAdjustments, RenovationAllowanceBalancingCharge}
import models.responses.{AdjustmentStoreAnswers, UkOtherAdjustments, UkOtherPropertyExpenses}

// T: to return (merge into)
// U: saved (extract from)
// X: from downstream
trait Merger[T, U, X] {
  def merge(extracted: U, fromDownstream: X): T
}

object Merger {

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
              Some(extracted),
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
              BalancingCharge(extracted.balancingChargeYesNo, fromDownstreamAdjustment.balancingCharge),
              0, // Todo: fromWhere?
              RenovationAllowanceBalancingCharge(
                extracted.renovationAllowanceBalancingChargeYesNo,
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
              EsbaClaims(false),
              fromDownstream
            )
          )
        case _ => None
      }

    // Todo: Change Above after asking questiins!!!!

  }

  implicit class GeneralMerger[T, U, X](extracted: U) {

    def merge(
      fromDownstream: X
    )(implicit Merger: Merger[T, U, X]): T =
      Merger.merge(extracted, fromDownstream)
  }

}
