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

package models.request

import cats.implicits.catsSyntaxEitherId
import models.errors.ServiceError
import models.responses._
import monocle.Optional
import play.api.libs.json.{Json, OFormat}

final case class PropertyPeriodicSubmissionRequest(
  foreignFhlEea: Option[ForeignFhlEea],
  foreignProperty: Option[Seq[ForeignProperty]],
  ukFhlProperty: Option[UkFhlProperty],
  ukOtherProperty: Option[UkOtherProperty]
)

object PropertyPeriodicSubmissionRequest {
  implicit val format: OFormat[PropertyPeriodicSubmissionRequest] = Json.format[PropertyPeriodicSubmissionRequest]

  private def createNewUkPropertyExpenses(
           periodicSubmission: Option[PropertyPeriodicSubmission],
           expenses: Expenses,
           consolidatedExpenses: Option[ConsolidatedExpenses]): UkOtherPropertyExpenses = {

    val newExpense = UkOtherPropertyExpenses(
      premisesRunningCosts = expenses.rentsRatesAndInsurance,
      repairsAndMaintenance = expenses.repairsAndMaintenanceCosts,
      financialCosts = expenses.loanInterestOrOtherFinancialCost,
      professionalFees = expenses.otherProfessionalFees,
      costOfServices = expenses.costsOfServicesProvided,
      travelCosts = expenses.propertyBusinessTravelCosts,
      other = expenses.otherAllowablePropertyExpenses,
      residentialFinancialCostsCarriedForward = periodicSubmission.flatMap(
        _.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCostsCarriedForward))
      ),
      ukOtherRentARoom = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.ukOtherRentARoom))),
      consolidatedExpense =
        periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.consolidatedExpense))),
      residentialFinancialCost =
        periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCost)))
    )

    consolidatedExpenses.fold{
      newExpense
    }{
      ce =>
        if(ce.consolidatedExpensesYesOrNo) {
          UkOtherPropertyExpenses(
           None, None, None, None, None, None, None, None, None, None, consolidatedExpense = ce.consolidatedExpensesAmount)
        } else {
          newExpense
        }
    }
  }

  def fromExpenses(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    expenses: Expenses
  ): Either[ServiceError, PropertyPeriodicSubmissionRequest] = {
    val (periodicSubmission, ukOtherPropertyIncome)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyIncome]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, _, _, Some(UkOtherProperty(Some(income), _)))) =>
          (Some(pps), Some(income))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }

      PropertyPeriodicSubmissionRequest(
        periodicSubmission.flatMap(_.foreignFhlEea),
        periodicSubmission.flatMap(_.foreignProperty),
        periodicSubmission.flatMap(_.ukFhlProperty),
        Some(
          UkOtherProperty(
            ukOtherPropertyIncome,
            Some(
             createNewUkPropertyExpenses(periodicSubmission, expenses, expenses.consolidatedExpenses)
            )
          )
        )
      ).asRight[ServiceError]
  }
  def fromUkOtherPropertyIncome(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    saveIncome: SaveIncome
  ): Either[ServiceError, PropertyPeriodicSubmissionRequest] = {

    val (periodicSubmission, ukOtherPropertyExpenses)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyExpenses]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, _, _, Some(UkOtherProperty(_, Some(expenses))))) =>
          (Some(pps), Some(expenses))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }

    val ukOtherPropertyIncome = UkOtherPropertyIncome(
      saveIncome.ukOtherPropertyIncome.premiumsOfLeaseGrant,
      saveIncome.ukOtherPropertyIncome.reversePremiums,
      saveIncome.ukOtherPropertyIncome.periodAmount,
      saveIncome.ukOtherPropertyIncome.taxDeducted,
      saveIncome.ukOtherPropertyIncome.otherIncome,
      saveIncome.ukOtherPropertyIncome.ukOtherRentARoom
    )

    val requestWithEmptyRentalsIncome = PropertyPeriodicSubmissionRequest(
      periodicSubmission.flatMap(_.foreignFhlEea),
      periodicSubmission.flatMap(_.foreignProperty),
      periodicSubmission.flatMap(_.ukFhlProperty),
      Some(
        UkOtherProperty(
          None,
          ukOtherPropertyExpenses // Todo: Change here, move consolidated to separate part!
        )
      )
    )

    val result: PropertyPeriodicSubmissionRequest =
      updateUkPropertyRentalsIncome(ukOtherPropertyIncome, requestWithEmptyRentalsIncome)
    result.asRight[ServiceError]
  }

  private def updateUkPropertyRentalsIncome(
    ukOtherPropertyIncome: UkOtherPropertyIncome,
    request: PropertyPeriodicSubmissionRequest
  ): PropertyPeriodicSubmissionRequest = {
    val ukOtherPropertyLens: Optional[PropertyPeriodicSubmissionRequest, UkOtherProperty] =
      Optional[PropertyPeriodicSubmissionRequest, UkOtherProperty] { ppsr =>
        ppsr match {
          case PropertyPeriodicSubmissionRequest(_, _, _, None) => Some(UkOtherProperty(None, None))
          case PropertyPeriodicSubmissionRequest(_, _, _, uopi) => uopi
        }
      } { ukop => ppsr =>
        ppsr.copy(ukOtherProperty = Some(ukop))
      }

    val ukOtherPropertyIncomeLens: Optional[UkOtherProperty, UkOtherPropertyIncome] =
      Optional[UkOtherProperty, UkOtherPropertyIncome] { ukop =>
        ukop match {
          case UkOtherProperty(None, _)  => Some(UkOtherPropertyIncome(None, None, None, None, None, None))
          case UkOtherProperty(ukopi, _) => ukopi
        }
      } { ukopi => ukop =>
        ukop.copy(income = Some(ukopi))
      }

    val focusFromRequestOnToIncomeukOtherPropertyLens = ukOtherPropertyLens.andThen(ukOtherPropertyIncomeLens)
    val result = focusFromRequestOnToIncomeukOtherPropertyLens.replace(ukOtherPropertyIncome)(request)
    result
  }
}
