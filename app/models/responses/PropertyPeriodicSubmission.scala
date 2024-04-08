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

case class PropertyPeriodicSubmission(submittedOn: Option[LocalDateTime],
                                      fromDate: LocalDate,
                                      toDate: LocalDate,
                                      foreignFhlEea: Option[ForeignFhlEea],
                                      foreignProperty: Option[Seq[ForeignProperty]],
                                      ukFhlProperty: Option[UkFhlProperty],
                                      ukOtherProperty: Option[UkOtherProperty])

object PropertyPeriodicSubmission {
  implicit val format: OFormat[PropertyPeriodicSubmission] = Json.format[PropertyPeriodicSubmission]

  def fromUkOtherPropertyExpenses(ukOtherPropertyExpenses: UkOtherPropertyExpenses): PropertyPeriodicSubmission = {
    PropertyPeriodicSubmission(
      None,
      LocalDate.now(),
      LocalDate.now(),
      None,
      None,
      None,
      Some(
        UkOtherProperty(
          UkOtherPropertyIncome(None, None, None, None, None, None),
          ukOtherPropertyExpenses
        )
      )
    )
  }

  def fromUkOtherPropertyIncome(ukOtherPropertyIncome: UkOtherPropertyIncome): PropertyPeriodicSubmission = {
    PropertyPeriodicSubmission(
      None,
      LocalDate.now(),
      LocalDate.now(),
      None,
      None,
      None,
      Some(
        UkOtherProperty(
          ukOtherPropertyIncome,
          UkOtherPropertyExpenses(
            Some(0), //Todo: This needs to be fetched from request(to be updated with expenses), and needs to be updated when Expenses ticket is implemented!
            None,
            None,
            None,
            None,
            None,
            None
          )
        )
      )
    )
  }

}

case class ForeignFhlEea(income: ForeignFhlIncome,
                         expenses: ForeignFhlExpenses)

object ForeignFhlEea {
  implicit val format: OFormat[ForeignFhlEea] = Json.format[ForeignFhlEea]
}

case class ForeignFhlIncome(rentAmount: BigDecimal)

object ForeignFhlIncome {
  implicit val format: OFormat[ForeignFhlIncome] = Json.format[ForeignFhlIncome]
}

case class ForeignFhlExpenses(premisesRunningCosts: Option[BigDecimal],
                              repairsAndMaintenance: Option[BigDecimal],
                              financialCosts: Option[BigDecimal],
                              professionalFees: Option[BigDecimal],
                              costOfServices: Option[BigDecimal],
                              travelCosts: Option[BigDecimal],
                              other: Option[BigDecimal],
                              consolidatedExpense: Option[BigDecimal])

object ForeignFhlExpenses {
  implicit val format: OFormat[ForeignFhlExpenses] = Json.format[ForeignFhlExpenses]
}

case class ForeignProperty(countryCode: String,
                           income: Option[ForeignPropertyIncome],
                           expenses: Option[ForeignPropertyExpenses])

object ForeignProperty {
  implicit val format: OFormat[ForeignProperty] = Json.format[ForeignProperty]
}

case class ForeignPropertyIncome(rentIncome: Option[ForeignPropertyRentIncome],
                                 foreignTaxCreditRelief: Option[Boolean],
                                 premiumsOfLeaseGrant: Option[BigDecimal],
                                 otherPropertyIncome: Option[BigDecimal],
                                 foreignTaxPaidOrDeducted: Option[BigDecimal],
                                 specialWithholdingTaxOrUkTaxPaid: Option[BigDecimal])

object ForeignPropertyIncome {
  implicit val format: OFormat[ForeignPropertyIncome] = Json.format[ForeignPropertyIncome]
}

case class ForeignPropertyExpenses(premisesRunningCosts: Option[BigDecimal],
                                   repairsAndMaintenance: Option[BigDecimal],
                                   financialCosts: Option[BigDecimal],
                                   professionalFees: Option[BigDecimal],
                                   travelCosts: Option[BigDecimal],
                                   costOfServices: Option[BigDecimal],
                                   residentialFinancialCost: Option[BigDecimal],
                                   broughtFwdResidentialFinancialCost: Option[BigDecimal],
                                   other: Option[BigDecimal],
                                   consolidatedExpense: Option[BigDecimal])

object ForeignPropertyExpenses {
  implicit val format: OFormat[ForeignPropertyExpenses] = Json.format[ForeignPropertyExpenses]
}

case class ForeignPropertyRentIncome(rentAmount: BigDecimal)

object ForeignPropertyRentIncome {
  implicit val format: OFormat[ForeignPropertyRentIncome] = Json.format[ForeignPropertyRentIncome]
}

case class UkFhlProperty(income: UkFhlIncome,
                         expenses: UkPropertyExpenses)

object UkFhlProperty {
  implicit val format: OFormat[UkFhlProperty] = Json.format[UkFhlProperty]
}

case class UkFhlIncome(periodAmount: Option[BigDecimal],
                       taxDeducted: Option[BigDecimal],
                       ukFhlRentARoom: Option[RentARoomIncome])

object UkFhlIncome {
  implicit val format: OFormat[UkFhlIncome] = Json.format[UkFhlIncome]
}

case class RentARoomIncome(rentsReceived: BigDecimal)

object RentARoomIncome {
  implicit val format: OFormat[RentARoomIncome] = Json.format[RentARoomIncome]
}

case class UkPropertyExpenses(premisesRunningCosts: Option[BigDecimal],
                              repairsAndMaintenance: Option[BigDecimal],
                              financialCosts: Option[BigDecimal],
                              professionalFees: Option[BigDecimal],
                              travelCosts: Option[BigDecimal],
                              costOfServices: Option[BigDecimal],
                              other: Option[BigDecimal],
                              ukFhlRentARoom: Option[UkRentARoomExpense])

object UkPropertyExpenses {
  implicit val format: OFormat[UkPropertyExpenses] = Json.format[UkPropertyExpenses]
}

case class UkRentARoomExpense(amountClaimed: BigDecimal)

object UkRentARoomExpense {
  implicit val format: OFormat[UkRentARoomExpense] = Json.format[UkRentARoomExpense]
}

case class UkOtherProperty(income: UkOtherPropertyIncome,
                           expenses: UkOtherPropertyExpenses)

object UkOtherProperty {
  implicit val format: OFormat[UkOtherProperty] = Json.format[UkOtherProperty]
}

case class UkOtherPropertyIncome(premiumsOfLeaseGrant: Option[BigDecimal],
                                 reversePremiums: Option[BigDecimal],
                                 periodAmount: Option[BigDecimal],
                                 taxDeducted: Option[BigDecimal],
                                 otherIncome: Option[BigDecimal],
                                 ukOtherRentARoom: Option[RentARoomIncome])

object UkOtherPropertyIncome {
  implicit val format: OFormat[UkOtherPropertyIncome] = Json.format[UkOtherPropertyIncome]
}

case class UkOtherPropertyExpenses(RentsRatesAndInsurance: Option[BigDecimal],
                                   RepairsAndMaintenanceCosts: Option[BigDecimal],
                                   loanInterest: Option[BigDecimal],
                                   otherProfessionalFee: Option[BigDecimal],
                                   costsOfServicesProvide: Option[BigDecimal],
                                   propertyBusinessTravelCost: Option[BigDecimal],
                                   otherAllowablePropertyExpenses: Option[BigDecimal]
                                  )

object UkOtherPropertyExpenses {
  implicit val format: OFormat[UkOtherPropertyExpenses] = Json.format[UkOtherPropertyExpenses]
}
