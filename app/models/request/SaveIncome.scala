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

package models.request

import models.responses.UkOtherPropertyIncome
import play.api.libs.json.{Format, Json, OFormat}

final case class SaveIncome(
  ukOtherPropertyIncome: UkOtherPropertyIncome,
  incomeToSave: Income
)

object SaveIncome {
  implicit val format: OFormat[SaveIncome] = Json.format[SaveIncome]
}

final case class PropertyRentalsIncome(
  isNonUKLandlord: Boolean,
  incomeFromPropertyRentals: BigDecimal,
  otherIncomeFromProperty: BigDecimal,
  deductingTax: Option[DeductingTax],
  calculatedFigureYourself: Option[CalculatedFigureYourself],
  yearLeaseAmount: Option[BigDecimal],
  receivedGrantLeaseAmount: Option[BigDecimal],
  premiumsGrantLease: Option[PremiumsGrantLease],
  reversePremiumsReceived: Option[ReversePremiumsReceived]
)

case object PropertyRentalsIncome {

  implicit val formats: Format[PropertyRentalsIncome] = Json.format[PropertyRentalsIncome]
}

final case class PropertyRentalsExpense(
  consolidatedExpenses: Option[ConsolidatedExpenses],
  rentsRatesAndInsurance: Option[BigDecimal],
  repairsAndMaintenanceCosts: Option[BigDecimal],
  loanInterestOrOtherFinancialCost: Option[BigDecimal],
  otherProfessionalFees: Option[BigDecimal],
  costsOfServicesProvided: Option[BigDecimal],
  propertyBusinessTravelCosts: Option[BigDecimal],
  otherAllowablePropertyExpenses: Option[BigDecimal]
)

object PropertyRentalsExpense {
  implicit val format = Json.format[PropertyRentalsExpense]
}
