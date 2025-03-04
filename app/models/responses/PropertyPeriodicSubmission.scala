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

case class PropertyPeriodicSubmission(
  submissionId: Option[PeriodicSubmissionId] = None,
  submittedOn: Option[LocalDateTime],
  fromDate: LocalDate,
  toDate: LocalDate,
  foreignProperty: Option[Seq[ForeignProperty]],
  ukOtherProperty: Option[UkOtherProperty]
)

object PropertyPeriodicSubmission {
  implicit val format: OFormat[PropertyPeriodicSubmission] = Json.format[PropertyPeriodicSubmission]
}

case class ForeignProperty(
  countryCode: String,
  income: Option[ForeignPropertyIncome],
  expenses: Option[ForeignPropertyExpenses]
)

object ForeignProperty {
  implicit val format: OFormat[ForeignProperty] = Json.format[ForeignProperty]
}

case class ForeignPropertyIncome(
  rentIncome: Option[ForeignPropertyRentIncome],
  foreignTaxCreditRelief: Option[Boolean],
  premiumsOfLeaseGrant: Option[BigDecimal],
  otherPropertyIncome: Option[BigDecimal],
  foreignTaxPaidOrDeducted: Option[BigDecimal],
  specialWithholdingTaxOrUkTaxPaid: Option[BigDecimal]
)

object ForeignPropertyIncome {
  implicit val format: OFormat[ForeignPropertyIncome] = Json.format[ForeignPropertyIncome]
}

case class ForeignPropertyExpenses(
  premisesRunningCosts: Option[BigDecimal],
  repairsAndMaintenance: Option[BigDecimal],
  financialCosts: Option[BigDecimal],
  professionalFees: Option[BigDecimal],
  travelCosts: Option[BigDecimal],
  costOfServices: Option[BigDecimal],
  residentialFinancialCost: Option[BigDecimal],
  broughtFwdResidentialFinancialCost: Option[BigDecimal],
  other: Option[BigDecimal],
  consolidatedExpense: Option[BigDecimal],      // API#1958 (Update) / API#1595 and API#1862 (Get) expects consolidatedExpense
  consolidatedExpenseAmount: Option[BigDecimal] // API#1861 (Create) expects consolidatedExpenseAmount
)

object ForeignPropertyExpenses {
  implicit val format: OFormat[ForeignPropertyExpenses] = Json.format[ForeignPropertyExpenses]
}

case class ForeignPropertyRentIncome(rentAmount: BigDecimal)

object ForeignPropertyRentIncome {
  implicit val format: OFormat[ForeignPropertyRentIncome] = Json.format[ForeignPropertyRentIncome]
}

case class RentARoomIncome(rentsReceived: BigDecimal)

case class UkOtherRoomRent(amountClaimed: BigDecimal)

object UkOtherRoomRent {
  implicit val format: OFormat[UkOtherRoomRent] = Json.format[UkOtherRoomRent]
}

object RentARoomIncome {
  implicit val format: OFormat[RentARoomIncome] = Json.format[RentARoomIncome]
}

case class UkRentARoomExpense(amountClaimed: BigDecimal)

object UkRentARoomExpense {
  implicit val format: OFormat[UkRentARoomExpense] = Json.format[UkRentARoomExpense]
}

case class UkOtherProperty(income: Option[UkOtherPropertyIncome], expenses: Option[UkOtherPropertyExpenses])

object UkOtherProperty {
  implicit val format: OFormat[UkOtherProperty] = Json.format[UkOtherProperty]

  def convertToNoneIfAllFieldsNone(ukOtherProperty: UkOtherProperty): Option[UkOtherProperty] =
    ukOtherProperty match {
      case UkOtherProperty(None, None) => None
      case UkOtherProperty(
            Some(UkOtherPropertyIncome(None, None, None, None, None, None)),
            Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None, None))
          ) =>
        None
      case UkOtherProperty(Some(UkOtherPropertyIncome(None, None, None, None, None, None)), expensesMaybe) =>
        Some(UkOtherProperty(None, expensesMaybe))
      case UkOtherProperty(
            incomeMaybe,
            Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None, None))
          ) =>
        Some(UkOtherProperty(incomeMaybe, None))

      case uopi => Some(uopi)
    }

}

case class UkOtherPropertyIncome(
  premiumsOfLeaseGrant: Option[BigDecimal],
  reversePremiums: Option[BigDecimal],
  periodAmount: Option[BigDecimal],
  taxDeducted: Option[BigDecimal],
  otherIncome: Option[BigDecimal],
  ukOtherRentARoom: Option[RentARoomIncome]
)

object UkOtherPropertyIncome {
  implicit val format: OFormat[UkOtherPropertyIncome] = Json.format[UkOtherPropertyIncome]
}

case class UkOtherPropertyExpenses(
  premisesRunningCosts: Option[BigDecimal] = None,
  repairsAndMaintenance: Option[BigDecimal] = None,
  financialCosts: Option[BigDecimal] = None,
  professionalFees: Option[BigDecimal] = None,
  travelCosts: Option[BigDecimal] = None,
  costOfServices: Option[BigDecimal] = None,
  other: Option[BigDecimal] = None,
  residentialFinancialCost: Option[BigDecimal] = None,
  residentialFinancialCostsCarriedForward: Option[BigDecimal] = None,
  ukOtherRentARoom: Option[UkRentARoomExpense] = None,
  consolidatedExpenses: Option[BigDecimal] = None,     // API#1861 (Create) expects consolidatedExpenses
  consolidatedExpense: Option[BigDecimal] = None,      // API#1958 (Update) / API#1595 and API#1862 (Get) expects consolidatedExpense
)

object UkOtherPropertyExpenses {
  implicit val format: OFormat[UkOtherPropertyExpenses] = Json.format[UkOtherPropertyExpenses]
}
