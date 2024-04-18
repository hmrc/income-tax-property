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

import models.request.Expenses
import play.api.libs.json.{Json, OFormat}

import java.time.{LocalDate, LocalDateTime}

//
final case class PropertyPeriodicSubmissionRequest(submittedOn: Option[LocalDateTime],
                                             foreignFhlEea: Option[ForeignFhlEea],
                                             foreignProperty: Option[Seq[ForeignProperty]],
                                             ukFhlProperty: Option[UkFhlProperty],
                                             ukOtherProperty: Option[UkOtherProperty])

object PropertyPeriodicSubmissionRequest {
  implicit val format: OFormat[PropertyPeriodicSubmissionRequest] = Json.format[PropertyPeriodicSubmissionRequest]

  def fromExpenses(expenses: Expenses): PropertyPeriodicSubmissionRequest = {
    PropertyPeriodicSubmissionRequest(
      None,
      None,
      None,
      None,
      Some(
        UkOtherProperty(
          UkOtherPropertyIncome(Some(0), None, None, None, None, None),
          UkOtherPropertyExpenses(
            premisesRunningCosts = expenses.rentsRatesAndInsurance,
            repairsAndMaintenance = expenses.repairsAndMaintenanceCosts,
            financialCosts = expenses.loanInterest,
            professionalFees = expenses.otherProfessionalFee,
            costOfServices = expenses.costsOfServicesProvided,
            travelCosts = expenses.propertyBusinessTravelCost,
            other = expenses.otherAllowablePropertyExpenses,
            residentialFinancialCostsCarriedForward = None,
            ukOtherRentARoom = None,
            consolidatedExpense = None,
            residentialFinancialCost = None
          )
        )
      )
    )
  }

  def fromUkOtherPropertyIncome(ukOtherPropertyIncome: UkOtherPropertyIncome): PropertyPeriodicSubmissionRequest = {
    PropertyPeriodicSubmissionRequest(
      None,
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