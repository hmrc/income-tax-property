
package uk.gov.hmrc.incometaxproperty.models.responses

import play.api.libs.json.{Json, OFormat}

case class AnnualSubmissionModel(foreignFhlEea: Option[ForeignFhlEeaAnnual],
                                 foreignProperty: Option[ForeignPropertyAnnual],
                                 ukOtherProperty: Option[UkOtherPropertyAnnual])

object AnnualSubmissionModel {
  implicit val format: OFormat[AnnualSubmissionModel] = Json.format[AnnualSubmissionModel]
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


case class FirstYear(qualifyingDate: String, qualifyingAmountExpenditure: Boolean)

object FirstYear {
  implicit val format: OFormat[FirstYear] = Json.format[FirstYear]
}
case class Building(name: Option[String], number: Option[String], postCode: String)

object Building {
  implicit val format: OFormat[Building] = Json.format[Building]
}

 case class ForeignFhlEeaAnnual(adjustments: Option[Adjustments], allowances: Option[Allowances])

object ForeignFhlEeaAnnual {
  implicit val format: OFormat[ForeignFhlEeaAnnual] = Json.format[ForeignFhlEeaAnnual]
}

case class UkOtherPropertyAnnual(ukOtherPropertyAnnualAdjustments: Option[Adjustments],
                                 ukOtherPropertyAnnualAllowances: Option[ukOtherPropertyAnnualAllowances])

object UkOtherPropertyAnnual {
  implicit val format: OFormat[UkOtherPropertyAnnual] = Json.format[UkOtherPropertyAnnual]
}

case class ForeignPropertyAnnual(countryCode: String,
                                 adjustments: Option[Adjustments],
                                 allowances: Option[Allowances])

object ForeignPropertyAnnual {
  implicit val format: OFormat[ForeignPropertyAnnual] = Json.format[ForeignPropertyAnnual]
}