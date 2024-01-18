
package uk.gov.hmrc.incometaxproperty.models.responses

import play.api.libs.json.{Json, OFormat}

case class AnnualPeriodicAnnualSubmissionModel(foreignFhlEea: Option[ForeignFhlEea],
                                               foreignProperty: Option[ForeignProperty],
                                               ukOtherProperty: Option[UkOtherProperty])

object AnnualPeriodicAnnualSubmissionModel {
  implicit val format: OFormat[AnnualPeriodicAnnualSubmissionModel] = Json.format[AnnualPeriodicAnnualSubmissionModel]
}

case class Adjustments(privateUseAdjustment: Option[BigDecimal],
                       balancingCharge: Option[BigDecimal],
                       periodOfGraceAdjustment: Option[Boolean],
                       lossBroughtForward: Option[BigDecimal],
                       businessPremisesRenovationAllowanceBalancingCharges: Option[BigDecimal],
                       nonResidentLandlord: Boolean,
                       ukFhlRentARoom: Option[UkFhlRentARoom]
                      )

object Adjustments {
  implicit val format: OFormat[Adjustments] = Json.format[Adjustments]
}

case class UkFhlRentARoom(jointlyLet: Option[Boolean])

object UkFhlRentARoom {
  implicit val format: OFormat[UkFhlRentARoom] = Json.format[UkFhlRentARoom]
}

case class Allowances(annualInvestmentAllowance: Option[BigDecimal],
                      businessPremisesRenovationAllowance: Option[BigDecimal],
                      otherCapitalAllowance: Option[BigDecimal],
                      electricChargePointAllowance: Option[BigDecimal],
                      zeroEmissionGoodsVehicleAllowance: Option[BigDecimal]
                      )

object Allowances {
  implicit val format: OFormat[Allowances] = Json.format[Allowances]
}

case class ukOtherPropertyAnnualAllowances(annualInvestmentAllowance: Option[BigDecimal],
                                           zeroEmissionGoodsVehicleAllowance: Option[BigDecimal],
                                           businessPremisesRenovationAllowance: Option[BigDecimal],
                                           otherCapitalAllowance: Option[BigDecimal],
                                           costOfReplacingDomesticGoods: Option[BigDecimal],
                                           electricChargePointAllowance: Option[BigDecimal],
                                           structuredBuildingAllowance: Option[StructuredBuildingAllowance],
                                           enhancedStructuredBuildingAllowance: Option[StructuredBuildingAllowance],
                                           zeroEmissionsCarAllowance: Option[BigDecimal])

object ukOtherPropertyAnnualAllowances {
  implicit val format: OFormat[ukOtherPropertyAnnualAllowances] = Json.format[ukOtherPropertyAnnualAllowances]
}

case class StructuredBuildingAllowance(amount: Option[BigDecimal],
                                       firstYear: FirstYear,
                                       building: Building)

object StructuredBuildingAllowance {
  implicit val format: OFormat[StructuredBuildingAllowance] = Json.format[StructuredBuildingAllowance]
}

case class FirstYear(qualifyingDate: String, qualifyingAmountExpenditure: Boolean)

object FirstYear {
  implicit val format: OFormat[FirstYear] = Json.format[FirstYear]
}
case class Building(name: Option[String], number: Option[String], postCode: String)

object Building {
  implicit val format: OFormat[Building] = Json.format[Building]
}

case class ForeignFhlEea(adjustments: Option[Adjustments], allowances: Option[Allowances])

object ForeignFhlEea {
  implicit val format: OFormat[ForeignFhlEea] = Json.format[ForeignFhlEea]
}

case class UkOtherProperty(ukOtherPropertyAnnualAdjustments: Option[Adjustments], ukOtherPropertyAnnualAllowances: Option[ukOtherPropertyAnnualAllowances])

object UkOtherProperty {
  implicit val format: OFormat[UkOtherProperty] = Json.format[UkOtherProperty]
}
case class ForeignProperty(countryCode: String,
                           adjustments: Option[Adjustments],
                           allowances: Option[Allowances])

object ForeignProperty {
  implicit val format: OFormat[ForeignProperty] = Json.format[ForeignProperty]
}