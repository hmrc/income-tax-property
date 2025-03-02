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
  submittedOn: Option[LocalDateTime] = None,
  foreignProperty: Option[Seq[AnnualForeignProperty]] = None,
  ukOtherProperty: Option[AnnualUkOtherProperty] = None
)

object PropertyAnnualSubmission {
  implicit val format: OFormat[PropertyAnnualSubmission] = Json.format[PropertyAnnualSubmission]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  def fromEsbas(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    esbas: List[Esba]
  ): PropertyAnnualSubmission = {
    val ukOtherAllowances = UkOtherAllowances().copy(enhancedStructuredBuildingAllowance = Some(esbas))
    PropertyAnnualSubmission().copy(
      ukOtherProperty = Some(AnnualUkOtherProperty().copy(
        ukOtherPropertyAnnualAllowances = Some(ukOtherAllowances)
      ))
    )
  }

  def fromSbas(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    sbas: List[StructuredBuildingAllowance]
  ): PropertyAnnualSubmission = {
    val ukOtherAllowances = UkOtherAllowances().copy(structuredBuildingAllowance = Some(sbas))
    PropertyAnnualSubmission().copy(
      ukOtherProperty = Some(AnnualUkOtherProperty().copy(
        ukOtherPropertyAnnualAllowances = Some(ukOtherAllowances)
      ))
    )
  }

  def fromRentalsAndRentARoomAbout(
    rentalsAndRaRAbout: RentalsAndRaRAbout,
    propertyAnnualSubmission: PropertyAnnualSubmission
  ): PropertyAnnualSubmission = {
    val maybeNonResidentLandlord: Option[Boolean] = for {
      ukOtherProperty <- propertyAnnualSubmission.ukOtherProperty
      ukOtherPropertyAnnualAdjustments <- ukOtherProperty.ukOtherPropertyAnnualAdjustments
      nonResidentLandlord <- ukOtherPropertyAnnualAdjustments.nonResidentLandlord
    } yield nonResidentLandlord

    val ukOtherAdjustments: UkOtherAdjustments = UkOtherAdjustments().copy(
      ukOtherRentARoom = Some(UkRentARoom(rentalsAndRaRAbout.jointlyLetYesOrNo)),
      nonResidentLandlord = maybeNonResidentLandlord.orElse(Some(false))
    )

    PropertyAnnualSubmission().copy(
      ukOtherProperty = Some(AnnualUkOtherProperty().copy(
        ukOtherPropertyAnnualAdjustments = Some(ukOtherAdjustments)
      ))
    )
  }

  def fromUkRentARoomAbout(
    ukRaRAbout: RaRAbout,
    propertyAnnualSubmission: PropertyAnnualSubmission
  ): PropertyAnnualSubmission = {
    val maybeNonResidentLandlord: Option[Boolean] = for {
      ukOtherProperty <- propertyAnnualSubmission.ukOtherProperty
      ukOtherPropertyAnnualAdjustments <- ukOtherProperty.ukOtherPropertyAnnualAdjustments
      nonResidentLandlord <- ukOtherPropertyAnnualAdjustments.nonResidentLandlord
    } yield nonResidentLandlord

    val ukOtherAdjustments: UkOtherAdjustments = UkOtherAdjustments().copy(
      ukOtherRentARoom = Some(UkRentARoom(ukRaRAbout.jointlyLetYesOrNo)),
      nonResidentLandlord = maybeNonResidentLandlord.orElse(Some(false))
    )

    PropertyAnnualSubmission().copy(
      ukOtherProperty = Some(AnnualUkOtherProperty().copy(
        ukOtherPropertyAnnualAdjustments = Some(ukOtherAdjustments)
      ))
    )
  }

  def fromPropertyRentalAdjustments(
    propertyRentalAdjustments: PropertyRentalAdjustments,
    propertyAnnualSubmission: PropertyAnnualSubmission
  ): PropertyAnnualSubmission = {
    val maybeNonResidentLandlord: Option[Boolean] = for {
      ukOtherProperty <- propertyAnnualSubmission.ukOtherProperty
      ukOtherPropertyAnnualAdjustments <- ukOtherProperty.ukOtherPropertyAnnualAdjustments
      nonResidentLandlord <- ukOtherPropertyAnnualAdjustments.nonResidentLandlord
    } yield nonResidentLandlord

    val ukOtherAdjustments: UkOtherAdjustments = UkOtherAdjustments().copy(
      nonResidentLandlord = maybeNonResidentLandlord.orElse(Some(false)),
      balancingCharge = propertyRentalAdjustments.balancingCharge.balancingChargeAmount,
      privateUseAdjustment = Some(propertyRentalAdjustments.privateUseAdjustment),
      businessPremisesRenovationAllowanceBalancingCharges =
        propertyRentalAdjustments.renovationAllowanceBalancingCharge.renovationAllowanceBalancingChargeAmount
    )

    PropertyAnnualSubmission().copy(
      ukOtherProperty = Some(AnnualUkOtherProperty().copy(
        ukOtherPropertyAnnualAdjustments = Some(ukOtherAdjustments)
      ))
    )
  }

  def fromRaRAdjustments(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    raRAdjustments: RaRAdjustments
  ): PropertyAnnualSubmission = {
    val maybeNonResidentLandlord: Option[Boolean] = for {
      ukOtherProperty <- propertyAnnualSubmission.ukOtherProperty
      ukOtherPropertyAnnualAdjustments <- ukOtherProperty.ukOtherPropertyAnnualAdjustments
      nonResidentLandlord <- ukOtherPropertyAnnualAdjustments.nonResidentLandlord
    } yield nonResidentLandlord

    val ukOtherAdjustments: UkOtherAdjustments = UkOtherAdjustments().copy(
      balancingCharge = raRAdjustments.balancingCharge.flatMap(_.balancingChargeAmount),
      nonResidentLandlord = maybeNonResidentLandlord.orElse(Some(false))
    )

    PropertyAnnualSubmission().copy(
      ukOtherProperty = Some(AnnualUkOtherProperty().copy(
        ukOtherPropertyAnnualAdjustments = Some(ukOtherAdjustments),
        ukOtherPropertyAnnualAllowances = None
      ))
    )
  }

  def fromRaRAllowances(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    rentARoomAllowances: RentARoomAllowances
  ): PropertyAnnualSubmission = {
    val ukOtherAllowances = UkOtherAllowances().copy(
      zeroEmissionGoodsVehicleAllowance = rentARoomAllowances.zeroEmissionGoodsVehicleAllowance,
      zeroEmissionsCarAllowance = rentARoomAllowances.zeroEmissionCarAllowance,
      otherCapitalAllowance = rentARoomAllowances.capitalAllowancesForACar.flatMap(_.capitalAllowancesForACarAmount)
        .fold(rentARoomAllowances.otherCapitalAllowance)(Some(_)),
      costOfReplacingDomesticGoods = rentARoomAllowances.replacementOfDomesticGoodsAllowance
    )
    PropertyAnnualSubmission().copy(
      ukOtherProperty = Some(AnnualUkOtherProperty().copy(
        ukOtherPropertyAnnualAllowances = Some(ukOtherAllowances)
      ))
    )
  }

  def fromRentalAllowances(
    propertyAnnualSubmission: PropertyAnnualSubmission,
    rentalAllowances: RentalAllowances
  ): PropertyAnnualSubmission = {
    val ukOtherAllowances = UkOtherAllowances().copy(
      annualInvestmentAllowance = rentalAllowances.annualInvestmentAllowance,
      zeroEmissionsCarAllowance = rentalAllowances.zeroEmissionCarAllowance,
      zeroEmissionGoodsVehicleAllowance = rentalAllowances.zeroEmissionGoodsVehicleAllowance,
      otherCapitalAllowance = rentalAllowances.capitalAllowancesForACar.flatMap(_.capitalAllowancesForACarAmount)
        .orElse(rentalAllowances.otherCapitalAllowance),
      costOfReplacingDomesticGoods = rentalAllowances.replacementOfDomesticGoodsAllowance,
      businessPremisesRenovationAllowance = rentalAllowances.businessPremisesRenovationAllowance
    )
    PropertyAnnualSubmission().copy(
      ukOtherProperty = Some(AnnualUkOtherProperty().copy(
        ukOtherPropertyAnnualAllowances = Some(ukOtherAllowances)
      ))
    )
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
  ukOtherPropertyAnnualAdjustments: Option[UkOtherAdjustments] = None,
  ukOtherPropertyAnnualAllowances: Option[UkOtherAllowances] = None
)

object AnnualUkOtherProperty {
  implicit val format: OFormat[AnnualUkOtherProperty] = Json.format[AnnualUkOtherProperty]
}

case class UkOtherAdjustments(
  lossBroughtForward: Option[BigDecimal] = None,
  balancingCharge: Option[BigDecimal] = None,
  privateUseAdjustment: Option[BigDecimal] = None,
  businessPremisesRenovationAllowanceBalancingCharges: Option[BigDecimal] = None,
  nonResidentLandlord: Option[Boolean] = None,
  ukOtherRentARoom: Option[UkRentARoom] = None,  // API#1598 (Get) expects ukOtherRentARoom
  rentARoom: Option[UkRentARoom] = None,         // API#1805 (Get) expects rentARoom
  whenYouReportedTheLoss: Option[WhenYouReportedTheLoss] = None
)

object UkOtherAdjustments {
  implicit val format: OFormat[UkOtherAdjustments] = Json.format[UkOtherAdjustments]
}

case class UkOtherAllowances(
  annualInvestmentAllowance: Option[BigDecimal] = None,
  zeroEmissionGoodsVehicleAllowance: Option[BigDecimal] = None,
  businessPremisesRenovationAllowance: Option[BigDecimal] = None,
  otherCapitalAllowance: Option[BigDecimal] = None,
  costOfReplacingDomesticGoods: Option[BigDecimal] = None,
  structuredBuildingAllowance: Option[Seq[StructuredBuildingAllowance]] = None,
  enhancedStructuredBuildingAllowance: Option[Seq[Esba]] = None,
  zeroEmissionsCarAllowance: Option[BigDecimal] = None,
  propertyIncomeAllowance: Option[BigDecimal] = None
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
