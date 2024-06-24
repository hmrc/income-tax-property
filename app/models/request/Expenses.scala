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

import play.api.libs.json.{Json, OFormat}

final case class ConsolidatedExpenses(
  consolidatedExpensesYesOrNo: Boolean,
  consolidatedExpensesAmount: Option[BigDecimal]
)

object ConsolidatedExpenses {
  implicit val format: OFormat[ConsolidatedExpenses] = Json.format
}
//Rent A Room
//===========
//Rents, rates and insurance	£22	Change
//Property repairs and maintenance	£22	Change
//Legal, management or other professional fees	£22	Change
//Costs of services provided, including wages	£22	Change
//Residential property finance costs	£22	Change
//Unused residential property finance costs brought forward	£22	Change
//Other allowable property expenses	£22	Change

//Property Rentals
//Consolidated expenses	No	Change
//Running costs of your property	£33	Change
//Property repairs and maintenance	£2	Change
//Loan interest or other financial costs	£2	Change
//Legal, management or other professional fees	£2	Change
//Costs of services provided, including wages	£2	Change
//Property business travel costs	£2	Change
//Other allowable property expenses

//Property Rentals
//Consolidated expenses	No	Change
//Running costs of your property	£33	Change
//Property repairs and maintenance	£2	Change
//Loan interest or other financial costs	£2	Change
//Legal, management or other professional fees	£2	Change
//Costs of services provided, including wages	£2	Change
//Property business travel costs	£2	Change
//Other allowable property expenses
final case class Expenses(
  consolidatedExpenses: Option[ConsolidatedExpenses],
  rentsRatesAndInsurance: Option[BigDecimal],
  repairsAndMaintenanceCosts: Option[BigDecimal],
  loanInterest: Option[BigDecimal],
  otherProfessionalFee: Option[BigDecimal],
  costsOfServicesProvided: Option[BigDecimal],
  propertyBusinessTravelCost: Option[BigDecimal],
  otherAllowablePropertyExpenses: Option[BigDecimal]
)

object Expenses {
  implicit val format: OFormat[Expenses] = Json.format[Expenses]
}
