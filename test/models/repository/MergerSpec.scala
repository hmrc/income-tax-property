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

import models.repository.Merger._
import models.request.{BalancingCharge, PropertyRentalAdjustments, RenovationAllowanceBalancingCharge}
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba._
import models.responses._
import utils.UnitTest

import java.time.{LocalDate, LocalDateTime}

class MergerSpec extends UnitTest {
  "Merger" should {
    val esbaDate = LocalDate.parse("2024-01-01")
    val qualifyingAmountExpenditure = 35
    val amount = 25
    val address1 = "name1"
    val address2 = "name2"
    val postcode = "AB1 XY2"
    val claimEnhancedStructureBuildingAllowance = ClaimEnhancedStructureBuildingAllowance(true)
    val esbaClaims = EsbaClaims(false)
    val balancingCharge = 24
    val privateUseAdjustments = 34
    val businessPremisesRenovationAllowanceBalancingCharges = 44
    val aPropertyAnnualSubmission = PropertyAnnualSubmission(
      submittedOn = Some(LocalDateTime.now),
      None, None, None, Some(AnnualUkOtherProperty(
        Some(
          UkOtherAdjustments(
            None,
            Some(balancingCharge),
            Some(privateUseAdjustments),
            Some(businessPremisesRenovationAllowanceBalancingCharges),
            None,
            None
          )
        ),
        Some(UkOtherAllowances(
        None, None, None, None, None, None, None, Some(
          Seq(
            Esba(
              amount,
              Some(
                StructuredBuildingAllowanceDate(esbaDate, qualifyingAmountExpenditure)
              ),

              StructuredBuildingAllowanceBuilding(
                Some(address1),
                Some(address2),
                postcode
              )
            )
          )
        ),
        None,
        None
      )
      )
      )
      )
    )
    "merge esba from downstream response and from repo into response model" in {

      val esbasInUpstream: Option[List[EsbaInUpstream]] = convert(aPropertyAnnualSubmission)
      val maybeEsbaInfoToSave: Option[EsbaInfoToSave] = Some(EsbaInfoToSave(claimEnhancedStructureBuildingAllowance, esbaClaims))

      maybeEsbaInfoToSave.merge(esbasInUpstream) shouldBe Some(EsbaInfo(
        claimEnhancedStructureBuildingAllowance,
        esbaClaims,
        List(
          EsbaInUpstream(
            esbaDate,
            qualifyingAmountExpenditure,
            amount,
            Address(
              BuildingName(address1),
              BuildingNumber(address2),
              Postcode(postcode)
            )
          )
        )
      ))
    }
    "merge adjustments from downstream response and from repo into response model" in {
      val ukOtherAdjustmentsMaybe: Option[UkOtherAdjustments] = for {
        uop <- aPropertyAnnualSubmission.ukOtherProperty
        uopoa <- uop.ukOtherPropertyAnnualAdjustments
      } yield uopoa

      val balancingChargeYesNo = true
      val renovationAllowanceBalancingChargeYesNo = false
      val adjustmentStoreAnswers: Option[AdjustmentStoreAnswers] = Some(AdjustmentStoreAnswers(balancingChargeYesNo, renovationAllowanceBalancingChargeYesNo))
      val propertyRentalAdjustments: Option[PropertyRentalAdjustments] = adjustmentStoreAnswers.merge(ukOtherAdjustmentsMaybe)
      propertyRentalAdjustments shouldBe Some(
              PropertyRentalAdjustments(
                BigDecimal(privateUseAdjustments),
                BalancingCharge(balancingChargeYesNo, Some(balancingCharge)),
                0, //Todo: fromWhere?
                RenovationAllowanceBalancingCharge(renovationAllowanceBalancingChargeYesNo, Some(businessPremisesRenovationAllowanceBalancingCharges)),
                BigDecimal(0), //Todo: fromWhere?
                BigDecimal(0), //Todo: fromWhere?
              )
      )
    }
  }

  private def convert(propertyAnnualSubmission: PropertyAnnualSubmission): Option[List[EsbaInUpstream]] = {
    val esbasMaybe: Option[List[Esba]] = for {
      ukop <- propertyAnnualSubmission.ukOtherProperty
      ukopaa <- ukop.ukOtherPropertyAnnualAllowances
      esba <- ukopaa.enhancedStructuredBuildingAllowance
    } yield esba.toList

    val esbasInRequestMaybe = esbasMaybe.map(_.map(e => EsbaInUpstream(
      // Todo: Remove .get's, but again, they are mandatory on frontend.
      // Todo: What to do if None comes from downstream?
      e.firstYear.get.qualifyingDate,
      e.firstYear.get.qualifyingAmountExpenditure,
      e.amount,
      Address(
        BuildingName(e.building.name.get),
        BuildingNumber(e.building.number.get),
        Postcode(e.building.postCode)
      ))))
    esbasInRequestMaybe
  }
}
