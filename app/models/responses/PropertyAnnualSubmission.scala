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

import models.{RentalsAndRaRAbout, Enumerable}
import models.request._
import models.request.foreign.{AnnualForeignProperty, WithName}
import models.request.ukrentaroom.RaRAdjustments
import monocle.Optional
import monocle.macros.GenLens
import play.api.libs.json.{OFormat, Writes, Json, JsValue}
import play.api.libs.ws.BodyWritable

import java.time.{LocalDateTime, LocalDate}

case class PropertyAnnualSubmission(
  submittedOn: Option[LocalDateTime],
  foreignProperty: Option[Seq[AnnualForeignProperty]],
  ukOtherProperty: Option[AnnualUkOtherProperty]
)

object PropertyAnnualSubmission {
  implicit val format: OFormat[PropertyAnnualSubmission] = Json.format[PropertyAnnualSubmission]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  val emptyPropertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)

  def fromEsbas(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    esbas: List[Esba]
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAllowancesLens: Optional[AnnualUkOtherProperty, UkOtherAllowances] =
      Optional[AnnualUkOtherProperty, UkOtherAllowances] {
        case AnnualUkOtherProperty(_, None) =>
          Some(UkOtherAllowances(None, None, None, None, None, None, None, None, None))
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
        case PropertyAnnualSubmission(_, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAllowancesLens: Optional[AnnualUkOtherProperty, UkOtherAllowances] =
      Optional[AnnualUkOtherProperty, UkOtherAllowances] {
        case AnnualUkOtherProperty(_, None) =>
          Some(UkOtherAllowances(None, None, None, None, None, None, None, None, None))
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

  def fromRentalsAndRentARoomAbout(
    rentalsAndRaRAbout: RentalsAndRaRAbout,
    request: PropertyAnnualSubmission
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }

    val ukOtherAdjustmentsLens: Optional[AnnualUkOtherProperty, UkOtherAdjustments] =
      Optional[AnnualUkOtherProperty, UkOtherAdjustments] {
        case AnnualUkOtherProperty(None, _) => Some(UkOtherAdjustments(None, None, None, None, Some(false), None, None))
        case AnnualUkOtherProperty(uoa, _)  => uoa.map(_.copy(rentARoom = None))
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAdjustments = Some(uoa))
      }
    val ukRentARoomLens = GenLens[UkOtherAdjustments](_.ukOtherRentARoom)

    val balancingChargeLens = GenLens[UkOtherAdjustments](_.balancingCharge)

    val focusFromRequestOnToUkRentARoomLens =
      ukOtherPropertyLens.andThen(ukOtherAdjustmentsLens).andThen(ukRentARoomLens)
    val resultWithUkRentARoom = focusFromRequestOnToUkRentARoomLens.replace(
      Some(UkRentARoom(rentalsAndRaRAbout.jointlyLetYesOrNo))
    )(request)

    ukOtherPropertyLens.andThen(ukOtherAdjustmentsLens).andThen(balancingChargeLens)

    resultWithUkRentARoom
  }

  def fromUkRentARoomAbout(
    ukRaRAbout: RaRAbout,
    request: PropertyAnnualSubmission
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }

    val ukOtherAdjustmentsLens: Optional[AnnualUkOtherProperty, UkOtherAdjustments] =
      Optional[AnnualUkOtherProperty, UkOtherAdjustments] {
        case AnnualUkOtherProperty(None, _) => Some(UkOtherAdjustments(None, None, None, None, Some(false), None, None))
        case AnnualUkOtherProperty(uoa, _)  => uoa.map(_.copy(rentARoom = None))
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAdjustments = Some(uoa))
      }
    val ukRentARoomLens = GenLens[UkOtherAdjustments](_.ukOtherRentARoom)
    val focusFromRequestOnToUkRentARoomLens =
      ukOtherPropertyLens.andThen(ukOtherAdjustmentsLens).andThen(ukRentARoomLens)
    val resultWithUkRentARoom = focusFromRequestOnToUkRentARoomLens.replace(
      Some(UkRentARoom(ukRaRAbout.jointlyLetYesOrNo))
    )(request)

    resultWithUkRentARoom
  }

  def fromPropertyRentalAdjustments(
    propertyRentalAdjustments: PropertyRentalAdjustments,
    request: PropertyAnnualSubmission
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAdjustmentsLens: Optional[AnnualUkOtherProperty, UkOtherAdjustments] =
      Optional[AnnualUkOtherProperty, UkOtherAdjustments] {
        case AnnualUkOtherProperty(None, _) => Some(UkOtherAdjustments(None, None, None, None, Some(false), None, None))
        case AnnualUkOtherProperty(uoa, _)  => uoa.map(_.copy(rentARoom = None))
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
        case PropertyAnnualSubmission(_, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAdjustmentsLens: Optional[AnnualUkOtherProperty, UkOtherAdjustments] =
      Optional[AnnualUkOtherProperty, UkOtherAdjustments] {
        case AnnualUkOtherProperty(None, _) => Some(UkOtherAdjustments(None, None, None, None, Some(false), None, None))
        case AnnualUkOtherProperty(uoa, _)  => uoa.map(_.copy(rentARoom = None))
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAdjustments = Some(uoa))
      }

    val balancingChargeLens = GenLens[UkOtherAdjustments](_.balancingCharge)

    val focusFromRequestOnToBalancingChargeLens =
      ukOtherPropertyLens.andThen(ukOtherAdjustmentsLens).andThen(balancingChargeLens)

    val resultWithBalancingCharge = focusFromRequestOnToBalancingChargeLens.replace(
      raRAdjustments.balancingCharge.flatMap(_.balancingChargeAmount)
    )(propertyAnnualSubmission)

    resultWithBalancingCharge
  }

  def fromRaRAllowances(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    rentARoomAllowances: RentARoomAllowances
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAllowancesLens: Optional[AnnualUkOtherProperty, UkOtherAllowances] =
      Optional[AnnualUkOtherProperty, UkOtherAllowances] {
        case AnnualUkOtherProperty(_, None) =>
          Some(UkOtherAllowances(None, None, None, None, None, None, None, None, None))
        case AnnualUkOtherProperty(_, uoa) => uoa
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAllowances = Some(uoa))
      }

    val annualInvestmentAllowanceLens = GenLens[UkOtherAllowances](_.annualInvestmentAllowance)
    val zeroEmissionGoodsVehicleAllowanceLens = GenLens[UkOtherAllowances](_.zeroEmissionGoodsVehicleAllowance)
    val zeroEmissionCarAllowanceLens = GenLens[UkOtherAllowances](_.zeroEmissionsCarAllowance)
    val otherCapitalAllowanceLens = GenLens[UkOtherAllowances](_.otherCapitalAllowance)
    val costOfReplacingDomesticGoodsLens = GenLens[UkOtherAllowances](_.costOfReplacingDomesticGoods)

    // Focuses
    val focusFromRequestOnTozeroEmissionCarAllowanceLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(zeroEmissionCarAllowanceLens)
    val focusFromRequestOnTozeroEmissionGoodsVehicleAllowanceLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(zeroEmissionGoodsVehicleAllowanceLens)
    val focusFromRequestOnTootherCapitalAllowanceLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(otherCapitalAllowanceLens)
    val focusFromRequestOnTocostOfReplacingDomesticGoodsLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(costOfReplacingDomesticGoodsLens)

    // Results
    val resultWithzeroEmissionCarAllowance = focusFromRequestOnTozeroEmissionCarAllowanceLens.replace(
      rentARoomAllowances.zeroEmissionGoodsVehicleAllowance
    )(propertyAnnualSubmission)

    val resultWithzeroEmissionGoodsVehicleAllowance = focusFromRequestOnTozeroEmissionGoodsVehicleAllowanceLens.replace(
      rentARoomAllowances.zeroEmissionGoodsVehicleAllowance
    )(resultWithzeroEmissionCarAllowance)

    val resultWithotherCapitalAllowance = focusFromRequestOnTootherCapitalAllowanceLens.replace(
      rentARoomAllowances.capitalAllowancesForACar
        .flatMap(_.capitalAllowancesForACarAmount)
        .fold(
          rentARoomAllowances.otherCapitalAllowance
        )(Some(_))
    )(resultWithzeroEmissionGoodsVehicleAllowance)

    val resultWithcostOfReplacingDomesticGoods = focusFromRequestOnTocostOfReplacingDomesticGoodsLens.replace(
      rentARoomAllowances.replacementOfDomesticGoodsAllowance
    )(resultWithotherCapitalAllowance)
    resultWithcostOfReplacingDomesticGoods
  }

  def fromRentalAllowances(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    rentalAllowances: RentalAllowances
  ): PropertyAnnualSubmission = {
    val ukOtherPropertyLens: Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] =
      Optional[PropertyAnnualSubmission, AnnualUkOtherProperty] {
        case PropertyAnnualSubmission(_, _, None) => Some(AnnualUkOtherProperty(None, None))
        case PropertyAnnualSubmission(_, _, auop) => auop
      } { auop => pas =>
        pas.copy(ukOtherProperty = Some(auop))
      }
    val ukOtherAllowancesLens: Optional[AnnualUkOtherProperty, UkOtherAllowances] =
      Optional[AnnualUkOtherProperty, UkOtherAllowances] {
        case AnnualUkOtherProperty(_, None) =>
          Some(UkOtherAllowances(None, None, None, None, None, None, None, None, None))
        case AnnualUkOtherProperty(_, uoa) => uoa
      } { uoa => auop =>
        auop.copy(ukOtherPropertyAnnualAllowances = Some(uoa))
      }

    val annualInvestmentAllowanceLens = GenLens[UkOtherAllowances](_.annualInvestmentAllowance)
    val zeroEmissionsCarAllowanceLens = GenLens[UkOtherAllowances](_.zeroEmissionsCarAllowance)
    val zeroEmissionGoodsVehicleAllowanceLens = GenLens[UkOtherAllowances](_.zeroEmissionGoodsVehicleAllowance)
    val otherCapitalAllowanceLens = GenLens[UkOtherAllowances](_.otherCapitalAllowance)
    val costOfReplacingDomesticGoodsLens = GenLens[UkOtherAllowances](_.costOfReplacingDomesticGoods)
    val businessPremisesRenovationAllowanceLens = GenLens[UkOtherAllowances](_.businessPremisesRenovationAllowance)

    // Focuses
    val focusFromRequestOnToannualInvestmentAllowanceLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(annualInvestmentAllowanceLens)
    val focusFromRequestOnTozeroEmissionGoodsVehicleAllowanceLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(zeroEmissionGoodsVehicleAllowanceLens)

    val focusFromRequestOnTozeroEmissionsCarAllowanceLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(zeroEmissionsCarAllowanceLens)

    val focusFromRequestOnTootherCapitalAllowanceLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(otherCapitalAllowanceLens)
    val focusFromRequestOnTocostOfReplacingDomesticGoodsLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(costOfReplacingDomesticGoodsLens)
    val focusFromRequestOnTobusinessPremisesRenovationAllowanceLens =
      ukOtherPropertyLens.andThen(ukOtherAllowancesLens).andThen(businessPremisesRenovationAllowanceLens)

    // Results

    val resultWithbusinessPremisesRenovationAllowance =
      focusFromRequestOnTobusinessPremisesRenovationAllowanceLens.replace(
        rentalAllowances.businessPremisesRenovationAllowance
      )(propertyAnnualSubmission)

    val resultWithzeroEmissionsCarAllowance = focusFromRequestOnTozeroEmissionsCarAllowanceLens.replace(
      rentalAllowances.zeroEmissionCarAllowance
    )(resultWithbusinessPremisesRenovationAllowance)

    val resultWithannualInvestmentAllowance = focusFromRequestOnToannualInvestmentAllowanceLens.replace(
      rentalAllowances.annualInvestmentAllowance
    )(resultWithzeroEmissionsCarAllowance)

    val resultWithzeroEmissionGoodsVehicleAllowance = focusFromRequestOnTozeroEmissionGoodsVehicleAllowanceLens.replace(
      rentalAllowances.zeroEmissionGoodsVehicleAllowance
    )(resultWithannualInvestmentAllowance)

    val resultWithotherCapitalAllowance = focusFromRequestOnTootherCapitalAllowanceLens.replace(
      rentalAllowances.otherCapitalAllowance
    )(resultWithzeroEmissionGoodsVehicleAllowance)

    val resultWithcostOfReplacingDomesticGoods = focusFromRequestOnTocostOfReplacingDomesticGoodsLens.replace(
      rentalAllowances.replacementOfDomesticGoodsAllowance
    )(resultWithotherCapitalAllowance)

    resultWithcostOfReplacingDomesticGoods
  }

}


case class UkRentARoom(jointlyLet: Boolean)

object UkRentARoom {
  implicit val format: OFormat[UkRentARoom] = Json.format[UkRentARoom]
}

sealed trait WhenReportedTheLoss

object WhenReportedTheLoss extends Enumerable.Implicits {

  case object y2018to2019 extends WithName("y2018to2019") with WhenReportedTheLoss
  case object y2019to2020 extends WithName("y2019to2020") with WhenReportedTheLoss
  case object y2020to2021 extends WithName("y2020to2021") with WhenReportedTheLoss
  case object y2021to2022 extends WithName("y2021to2022") with WhenReportedTheLoss
  case object y2022to2023 extends WithName("y2022to2023") with WhenReportedTheLoss

  val values: Seq[WhenReportedTheLoss] = Seq(
    y2018to2019, y2019to2020, y2020to2021, y2021to2022, y2022to2023
  )

  implicit val enumerable: Enumerable[WhenReportedTheLoss] =
    Enumerable(values.map(v => v.toString -> v): _*)
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
  ukOtherRentARoom: Option[UkRentARoom], // API#1598 (Get) expects ukOtherRentARoom
  rentARoom: Option[UkRentARoom],         // API#1805 (Get) expects rentARoom
  whenYouReportedTheLoss: Option[WhenYouReportedTheLoss]
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
