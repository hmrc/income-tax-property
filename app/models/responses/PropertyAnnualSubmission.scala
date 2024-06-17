/*
 * Copyright 2023 HM Revenue & Customs
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

package models.responses

import models.request.esba.EsbaInfo
import models.request.sba.SbaInfo
import models.request.ukrentaroom.RaRAdjustments
import models.request.{CapitalAllowancesForACar, PropertyAbout, PropertyRentalAdjustments, RaRAbout, RentalAllowances}
import monocle.Optional
import monocle.macros.GenLens
import play.api.libs.json.{Json, OFormat}

import java.time.{LocalDate, LocalDateTime}

final case class FetchedPropertyData(
  capitalAllowancesForACar: Option[CapitalAllowancesForACar],
  propertyAbout: Option[PropertyAbout],
  adjustments: Option[PropertyRentalAdjustments],
  allowances: Option[RentalAllowances],
  esbasWithSupportingQuestions: Option[EsbaInfo],
  sbasWithSupportingQuestions: Option[SbaInfo]
)

object FetchedPropertyData {
  implicit val format = Json.format[FetchedPropertyData]
}

case class PropertyAnnualSubmission(
  submittedOn: Option[LocalDateTime],
  foreignFhlEea: Option[AnnualForeignFhlEea],
  foreignProperty: Option[Seq[AnnualForeignProperty]],
  ukFhlProperty: Option[AnnualUkFhlProperty],
  ukOtherProperty: Option[AnnualUkOtherProperty]
)

object PropertyAnnualSubmission {
  implicit val format: OFormat[PropertyAnnualSubmission] = Json.format[PropertyAnnualSubmission]
  val emptyPropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None, None, None)

  def fromEsbas(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    esbas: List[Esba]
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, _, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, _, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAllowancesLens: Optional[AnnualUkOtherProperty, UkOtherAllowances] =
      Optional[AnnualUkOtherProperty, UkOtherAllowances] {
        case AnnualUkOtherProperty(_, None) =>
          Some(UkOtherAllowances(None, None, None, None, None, None, None, None, None, None))
        case AnnualUkOtherProperty(_, uoa) => uoa
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAllowances = Some(uoa))
      }

    val esbasLens = GenLens[UkOtherAllowances](_.enhancedStructuredBuildingAllowance)

    val focusFromAnnualSubmissionOnEsbasLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(esbasLens)

    val resultWithEsbas: PropertyAnnualSubmission =
      focusFromAnnualSubmissionOnEsbasLens.replace(Some(esbas))(propertyAnnualSubmission)

    resultWithEsbas
  }

  def fromSbas(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    sbas: List[StructuredBuildingAllowance]
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, _, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, _, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAllowancesLens: Optional[AnnualUkOtherProperty, UkOtherAllowances] =
      Optional[AnnualUkOtherProperty, UkOtherAllowances] {
        case AnnualUkOtherProperty(_, None) =>
          Some(UkOtherAllowances(None, None, None, None, None, None, None, None, None, None))
        case AnnualUkOtherProperty(_, uoa) => uoa
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAllowances = Some(uoa))
      }

    val sbasLens = GenLens[UkOtherAllowances](_.structuredBuildingAllowance)

    val focusFromAnnualSubmissionOnSbasLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(sbasLens)

    val resultWithSbas: PropertyAnnualSubmission =
      focusFromAnnualSubmissionOnSbasLens.replace(Some(sbas))(propertyAnnualSubmission)

    resultWithSbas
  }

  def fromUkRentARoomAbout(
    ukRaRAbout: RaRAbout,
    request: PropertyAnnualSubmission
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, _, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, _, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }

    val ukOtherAdjustmentsLens: Optional[AnnualUkOtherProperty, UkOtherAdjustments] =
      Optional[AnnualUkOtherProperty, UkOtherAdjustments] {
        case AnnualUkOtherProperty(None, _) => Some(UkOtherAdjustments(None, None, None, None, None, None))
        case AnnualUkOtherProperty(uoa, _)  => uoa
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAdjustments = Some(uoa))
      }
    val ukRentARoomLens = GenLens[UkOtherAdjustments](_.ukOtherRentARoom)
    val focusFromRequestOnToUkRentARoomLens =
      ukOtherPropertyLens.andThen(ukOtherAdjustmentsLens).andThen(ukRentARoomLens)
    val resultWithUkRentARoom = focusFromRequestOnToUkRentARoomLens.replace(
      Some(UkRentARoom(ukRaRAbout.ukRentARoomJointlyLet))
    )(request)

    resultWithUkRentARoom
  }

  def fromPropertyRentalAdjustments(
    propertyRentalAdjustments: PropertyRentalAdjustments,
    request: PropertyAnnualSubmission
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, _, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, _, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAdjustmentsLens: Optional[AnnualUkOtherProperty, UkOtherAdjustments] =
      Optional[AnnualUkOtherProperty, UkOtherAdjustments] {
        case AnnualUkOtherProperty(None, _) => Some(UkOtherAdjustments(None, None, None, None, None, None))
        case AnnualUkOtherProperty(uoa, _)  => uoa
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAdjustments = Some(uoa))
      }

    val balancingChargeLens = GenLens[UkOtherAdjustments](_.balancingCharge)
    val privateUseAdjustmentLens = GenLens[UkOtherAdjustments](_.privateUseAdjustment)
    val renovationAllowanceBalancingChargeLens =
      GenLens[UkOtherAdjustments](_.businessPremisesRenovationAllowanceBalancingCharges)

    val focusFromRequestOnToBalancingChargeLens =
      ukOtherPropertyLens.andThen(ukOtherAdjustmentsLens).andThen(balancingChargeLens)
    val focusFromRequestOnToPrivateUseAdjustmentLens =
      ukOtherPropertyLens.andThen(ukOtherAdjustmentsLens).andThen(privateUseAdjustmentLens)
    val focusFromRequestOnToRenovationAllowanceBalancingChargeLens =
      ukOtherPropertyLens.andThen(ukOtherAdjustmentsLens).andThen(renovationAllowanceBalancingChargeLens)

    val resultWithBalancingCharge = focusFromRequestOnToBalancingChargeLens.replace(
      propertyRentalAdjustments.balancingCharge.balancingChargeAmount
    )(request)

    val resultWithBalancingChargeAndPrivateUseAdjustment = focusFromRequestOnToPrivateUseAdjustmentLens.replace(
      Some(propertyRentalAdjustments.privateUseAdjustment)
    )(resultWithBalancingCharge)

    val resultWithAllThree = focusFromRequestOnToRenovationAllowanceBalancingChargeLens.replace(
      propertyRentalAdjustments.renovationAllowanceBalancingCharge.renovationAllowanceBalancingChargeAmount
    )(resultWithBalancingChargeAndPrivateUseAdjustment)

    resultWithAllThree
  }

  def fromRaRAdjustments(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    raRAdjustments: RaRAdjustments
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, _, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, _, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAdjustmentsLens: Optional[AnnualUkOtherProperty, UkOtherAdjustments] =
      Optional[AnnualUkOtherProperty, UkOtherAdjustments] {
        case AnnualUkOtherProperty(None, _) => Some(UkOtherAdjustments(None, None, None, None, None, None))
        case AnnualUkOtherProperty(uoa, _)  => uoa
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAdjustments = Some(uoa))
      }

    val balancingChargeLens = GenLens[UkOtherAdjustments](_.balancingCharge)

    val focusFromRequestOnToBalancingChargeLens =
      ukOtherPropertyLens.andThen(ukOtherAdjustmentsLens).andThen(balancingChargeLens)

    val resultWithBalancingCharge = focusFromRequestOnToBalancingChargeLens.replace(
      raRAdjustments.raRBalancingCharge.flatMap(_.raRbalancingChargeAmount)
    )(propertyAnnualSubmission)

    resultWithBalancingCharge
  }

}

case class AnnualForeignFhlEea(adjustments: ForeignFhlAdjustments, allowances: ForeignFhlAllowances)

object AnnualForeignFhlEea {
  implicit val format: OFormat[AnnualForeignFhlEea] = Json.format[AnnualForeignFhlEea]
}

case class ForeignFhlAdjustments(
  privateUseAdjustment: BigDecimal,
  balancingCharge: BigDecimal,
  periodOfGraceAdjustment: Boolean
)

object ForeignFhlAdjustments {
  implicit val format: OFormat[ForeignFhlAdjustments] = Json.format[ForeignFhlAdjustments]
}

case class ForeignFhlAllowances(
  annualInvestmentAllowance: Option[BigDecimal],
  otherCapitalAllowance: Option[BigDecimal],
  electricChargePointAllowance: Option[BigDecimal],
  zeroEmissionsCarAllowance: Option[BigDecimal],
  propertyAllowance: Option[BigDecimal]
)

object ForeignFhlAllowances {
  implicit val format: OFormat[ForeignFhlAllowances] = Json.format[ForeignFhlAllowances]
}

case class AnnualForeignProperty(
  countryCode: String,
  adjustments: Option[ForeignPropertyAdjustments],
  allowances: Option[ForeignPropertyAllowances]
)

object AnnualForeignProperty {
  implicit val format: OFormat[AnnualForeignProperty] = Json.format[AnnualForeignProperty]
}

case class ForeignPropertyAdjustments(privateUseAdjustment: Option[BigDecimal], balancingCharge: Option[BigDecimal])

object ForeignPropertyAdjustments {
  implicit val format: OFormat[ForeignPropertyAdjustments] = Json.format[ForeignPropertyAdjustments]
}

case class ForeignPropertyAllowances(
  annualInvestmentAllowance: Option[BigDecimal],
  costOfReplacingDomesticItems: Option[BigDecimal],
  zeroEmissionsGoodsVehicleAllowance: Option[BigDecimal],
  otherCapitalAllowance: Option[BigDecimal],
  electricChargePointAllowance: Option[BigDecimal],
  structuredBuildingAllowance: Option[BigDecimal],
  zeroEmissionsCarAllowance: Option[BigDecimal],
  propertyAllowance: Option[BigDecimal]
)

object ForeignPropertyAllowances {
  implicit val format: OFormat[ForeignPropertyAllowances] = Json.format[ForeignPropertyAllowances]
}

case class AnnualUkFhlProperty(adjustments: UkFhlAdjustments, allowances: UkFhlAllowances)

object AnnualUkFhlProperty {
  implicit val format: OFormat[AnnualUkFhlProperty] = Json.format[AnnualUkFhlProperty]
}

case class UkFhlAdjustments(
  lossBroughtForward: Option[BigDecimal],
  privateUseAdjustment: Option[BigDecimal],
  balancingCharge: Option[BigDecimal],
  periodOfGraceAdjustment: Option[Boolean],
  businessPremisesRenovationAllowanceBalancingCharges: Option[BigDecimal],
  nonResidentLandlord: Boolean,
  ukFhlRentARoom: Option[UkRentARoom]
)

object UkFhlAdjustments {
  implicit val format: OFormat[UkFhlAdjustments] = Json.format[UkFhlAdjustments]
}

case class UkRentARoom(jointlyLet: Boolean)

object UkRentARoom {
  implicit val format: OFormat[UkRentARoom] = Json.format[UkRentARoom]
}

case class UkFhlAllowances(
  annualInvestmentAllowance: Option[BigDecimal],
  businessPremisesRenovationAllowance: Option[BigDecimal],
  otherCapitalAllowance: Option[BigDecimal],
  electricChargePointAllowance: Option[BigDecimal],
  zeroEmissionsCarAllowance: Option[BigDecimal],
  propertyIncomeAllowance: Option[BigDecimal]
)

object UkFhlAllowances {
  implicit val format: OFormat[UkFhlAllowances] = Json.format[UkFhlAllowances]
}

case class AnnualUkOtherProperty(
  ukOtherPropertyAnnualAdjustments: Option[UkOtherAdjustments],
  ukOtherPropertyAnnualAllowances: Option[UkOtherAllowances]
)

object AnnualUkOtherProperty {
  implicit val format: OFormat[AnnualUkOtherProperty] = Json.format[AnnualUkOtherProperty]
}

case class UkOtherAdjustments(
  lossBroughtForward: Option[BigDecimal],
  balancingCharge: Option[BigDecimal],
  privateUseAdjustment: Option[BigDecimal],
  businessPremisesRenovationAllowanceBalancingCharges: Option[BigDecimal],
  nonResidentLandlord: Option[Boolean],
  ukOtherRentARoom: Option[UkRentARoom]
)

object UkOtherAdjustments {
  implicit val format: OFormat[UkOtherAdjustments] = Json.format[UkOtherAdjustments]
}

case class UkOtherAllowances(
  annualInvestmentAllowance: Option[BigDecimal],
  zeroEmissionGoodsVehicleAllowance: Option[BigDecimal],
  businessPremisesRenovationAllowance: Option[BigDecimal],
  otherCapitalAllowance: Option[BigDecimal],
  costOfReplacingDomesticGoods: Option[BigDecimal],
  electricChargePointAllowance: Option[BigDecimal],
  structuredBuildingAllowance: Option[Seq[StructuredBuildingAllowance]],
  enhancedStructuredBuildingAllowance: Option[Seq[Esba]],
  zeroEmissionsCarAllowance: Option[BigDecimal],
  propertyIncomeAllowance: Option[BigDecimal]
)

object UkOtherAllowances {
  implicit val format: OFormat[UkOtherAllowances] = Json.format[UkOtherAllowances]
}

case class Esba(
  amount: BigDecimal,
  firstYear: Option[StructuredBuildingAllowanceDate],
  building: StructuredBuildingAllowanceBuilding
)

object Esba {
  implicit val format: OFormat[Esba] = Json.format[Esba]
}

case class StructuredBuildingAllowance(
  amount: BigDecimal,
  firstYear: Option[StructuredBuildingAllowanceDate],
  building: StructuredBuildingAllowanceBuilding
)

object StructuredBuildingAllowance {
  implicit val format: OFormat[StructuredBuildingAllowance] = Json.format[StructuredBuildingAllowance]
}

case class StructuredBuildingAllowanceDate(qualifyingDate: LocalDate, qualifyingAmountExpenditure: BigDecimal)

object StructuredBuildingAllowanceDate {
  implicit val format: OFormat[StructuredBuildingAllowanceDate] = Json.format[StructuredBuildingAllowanceDate]
}

case class StructuredBuildingAllowanceBuilding(name: Option[String], number: Option[String], postCode: String)

object StructuredBuildingAllowanceBuilding {
  implicit val format: OFormat[StructuredBuildingAllowanceBuilding] = Json.format[StructuredBuildingAllowanceBuilding]
}
