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

import play.api.libs.json.{Json, OFormat}

import java.time.{LocalDate, LocalDateTime}

case class PropertyAnnualSubmission(submittedOn: Option[LocalDateTime],
                                    foreignFhlEea: Option[AnnualForeignFhlEea],
                                    foreignProperty: Option[Seq[AnnualForeignProperty]],
                                    ukFhlProperty: Option[AnnualUkFhlProperty],
                                    ukOtherProperty: Option[AnnualUkOtherProperty])

object PropertyAnnualSubmission {
  implicit val format: OFormat[PropertyAnnualSubmission] = Json.format[PropertyAnnualSubmission]

  def fromEsbas(esbas: List[Esba]): PropertyAnnualSubmission = { //Todo: Validations MUST BE added!!!
    PropertyAnnualSubmission(
      Some(LocalDateTime.now()),
      None,
      None,
      None,
      Some(AnnualUkOtherProperty(
        Some(UkOtherAdjustments(
          None, None, None, None, None, None
        )),
        Some(UkOtherAllowances(
          None, None, None, None, None, None, None, Some(esbas), None, None
        ))
      ))
    )
  }

  def fromSbas(sbas: List[StructuredBuildingAllowance]): PropertyAnnualSubmission = { //Todo: Validations MUST BE added!!!
    PropertyAnnualSubmission(
      Some(LocalDateTime.now()),
      None,
      None,
      None,
      Some(AnnualUkOtherProperty(
        Some(UkOtherAdjustments(
          None, None, None, None, None, None
        )),
        Some(UkOtherAllowances(
          None, None, None, None, None, None, Some(sbas), None, None, None
        ))
      ))
    )
  }
}

case class AnnualForeignFhlEea(adjustments: ForeignFhlAdjustments,
                               allowances: ForeignFhlAllowances)

object AnnualForeignFhlEea {
  implicit val format: OFormat[AnnualForeignFhlEea] = Json.format[AnnualForeignFhlEea]
}

case class ForeignFhlAdjustments(privateUseAdjustment: BigDecimal,
                                 balancingCharge: BigDecimal,
                                 periodOfGraceAdjustment: Boolean)

object ForeignFhlAdjustments {
  implicit val format: OFormat[ForeignFhlAdjustments] = Json.format[ForeignFhlAdjustments]
}

case class ForeignFhlAllowances(annualInvestmentAllowance: Option[BigDecimal],
                                otherCapitalAllowance: Option[BigDecimal],
                                electricChargePointAllowance: Option[BigDecimal],
                                zeroEmissionsCarAllowance: Option[BigDecimal],
                                propertyAllowance: Option[BigDecimal])

object ForeignFhlAllowances {
  implicit val format: OFormat[ForeignFhlAllowances] = Json.format[ForeignFhlAllowances]
}

case class AnnualForeignProperty(countryCode: String,
                                 adjustments: Option[ForeignPropertyAdjustments],
                                 allowances: Option[ForeignPropertyAllowances])

object AnnualForeignProperty {
  implicit val format: OFormat[AnnualForeignProperty] = Json.format[AnnualForeignProperty]
}

case class ForeignPropertyAdjustments(privateUseAdjustment: Option[BigDecimal],
                                      balancingCharge: Option[BigDecimal])

object ForeignPropertyAdjustments {
  implicit val format: OFormat[ForeignPropertyAdjustments] = Json.format[ForeignPropertyAdjustments]
}

case class ForeignPropertyAllowances(annualInvestmentAllowance: Option[BigDecimal],
                                     costOfReplacingDomesticItems: Option[BigDecimal],
                                     zeroEmissionsGoodsVehicleAllowance: Option[BigDecimal],
                                     otherCapitalAllowance: Option[BigDecimal],
                                     electricChargePointAllowance: Option[BigDecimal],
                                     structuredBuildingAllowance: Option[BigDecimal],
                                     zeroEmissionsCarAllowance: Option[BigDecimal],
                                     propertyAllowance: Option[BigDecimal])

object ForeignPropertyAllowances {
  implicit val format: OFormat[ForeignPropertyAllowances] = Json.format[ForeignPropertyAllowances]
}

case class AnnualUkFhlProperty(adjustments: UkFhlAdjustments,
                               allowances: UkFhlAllowances)

object AnnualUkFhlProperty {
  implicit val format: OFormat[AnnualUkFhlProperty] = Json.format[AnnualUkFhlProperty]
}

case class UkFhlAdjustments(lossBroughtForward: Option[BigDecimal],
                            privateUseAdjustment: Option[BigDecimal],
                            balancingCharge: Option[BigDecimal],
                            periodOfGraceAdjustment: Option[Boolean],
                            businessPremisesRenovationAllowanceBalancingCharges: Option[BigDecimal],
                            nonResidentLandlord: Boolean,
                            ukFhlRentARoom: Option[UkRentARoom])

object UkFhlAdjustments {
  implicit val format: OFormat[UkFhlAdjustments] = Json.format[UkFhlAdjustments]
}

case class UkRentARoom(jointlyLet: Boolean)

object UkRentARoom {
  implicit val format: OFormat[UkRentARoom] = Json.format[UkRentARoom]
}

case class UkFhlAllowances(annualInvestmentAllowance: Option[BigDecimal],
                           businessPremisesRenovationAllowance: Option[BigDecimal],
                           otherCapitalAllowance: Option[BigDecimal],
                           electricChargePointAllowance: Option[BigDecimal],
                           zeroEmissionsCarAllowance: Option[BigDecimal],
                           propertyIncomeAllowance: Option[BigDecimal])

object UkFhlAllowances {
  implicit val format: OFormat[UkFhlAllowances] = Json.format[UkFhlAllowances]
}

case class AnnualUkOtherProperty(ukOtherPropertyAnnualAdjustments: Option[UkOtherAdjustments],
                                 ukOtherPropertyAnnualAllowances: Option[UkOtherAllowances])

object AnnualUkOtherProperty {
  implicit val format: OFormat[AnnualUkOtherProperty] = Json.format[AnnualUkOtherProperty]
}

case class UkOtherAdjustments(lossBroughtForward: Option[BigDecimal],
                              balancingCharge: Option[BigDecimal],
                              privateUseAdjustment: Option[BigDecimal],
                              businessPremisesRenovationAllowanceBalancingCharges: Option[BigDecimal],
                              nonResidentLandlord: Option[Boolean],
                              ukOtherRentARoom: Option[UkRentARoom])

object UkOtherAdjustments {
  implicit val format: OFormat[UkOtherAdjustments] = Json.format[UkOtherAdjustments]
}

case class UkOtherAllowances(annualInvestmentAllowance: Option[BigDecimal],
                             zeroEmissionGoodsVehicleAllowance: Option[BigDecimal],
                             businessPremisesRenovationAllowance: Option[BigDecimal],
                             otherCapitalAllowance: Option[BigDecimal],
                             costOfReplacingDomesticGoods: Option[BigDecimal],
                             electricChargePointAllowance: Option[BigDecimal],
                             structuredBuildingAllowance: Option[Seq[StructuredBuildingAllowance]],
                             enhancedStructuredBuildingAllowance: Option[Seq[Esba]],
                             zeroEmissionsCarAllowance: Option[BigDecimal],
                             propertyIncomeAllowance: Option[BigDecimal])

object UkOtherAllowances {
  implicit val format: OFormat[UkOtherAllowances] = Json.format[UkOtherAllowances]
}

case class Esba(amount: BigDecimal,
                firstYear: Option[StructuredBuildingAllowanceDate],
                building: StructuredBuildingAllowanceBuilding)

object Esba {
  implicit val format: OFormat[Esba] = Json.format[Esba]
}

case class StructuredBuildingAllowance(amount: BigDecimal,
                                       firstYear: Option[StructuredBuildingAllowanceDate],
                                       building: StructuredBuildingAllowanceBuilding)

object StructuredBuildingAllowance {
  implicit val format: OFormat[StructuredBuildingAllowance] = Json.format[StructuredBuildingAllowance]
}

case class StructuredBuildingAllowanceDate(qualifyingDate: LocalDate,
                                           qualifyingAmountExpenditure: BigDecimal)

object StructuredBuildingAllowanceDate {
  implicit val format: OFormat[StructuredBuildingAllowanceDate] = Json.format[StructuredBuildingAllowanceDate]
}

case class StructuredBuildingAllowanceBuilding(name: Option[String],
                                               number: Option[String],
                                               postCode: String)

object StructuredBuildingAllowanceBuilding {
  implicit val format: OFormat[StructuredBuildingAllowanceBuilding] = Json.format[StructuredBuildingAllowanceBuilding]
}
