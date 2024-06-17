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
import models.responses
import models.responses._
import monocle.Optional
import monocle.macros.GenLens
import play.api.libs.json.{Json, OFormat}

import java.time.LocalDate

final case class CreatePropertyPeriodicSubmissionRequest(
  fromDate: LocalDate,
  toDate: LocalDate,
  foreignFhlEea: Option[ForeignFhlEea],
  foreignProperty: Option[Seq[ForeignProperty]],
  ukFhlProperty: Option[UkFhlProperty],
  ukOtherProperty: Option[UkOtherProperty]
)

object CreatePropertyPeriodicSubmissionRequest {
  implicit val format: OFormat[CreatePropertyPeriodicSubmissionRequest] =
    Json.format[CreatePropertyPeriodicSubmissionRequest]

  def fromUkRaRAbout(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    ukRaRAbout: RaRAbout
  ): Either[ServiceError, CreatePropertyPeriodicSubmissionRequest] = {
    val ukOtherPropertyIncomeMaybe: Option[responses.UkOtherPropertyIncome] =
      periodicSubmissionMaybe.flatMap(_.ukOtherProperty.flatMap(_.income))

    val ukOtherPropertyExpensesMaybe: Option[responses.UkOtherPropertyExpenses] =
      periodicSubmissionMaybe.flatMap(_.ukOtherProperty.flatMap(_.expenses))

    val ukOtherPropertyIncome: UkOtherPropertyIncome = ukOtherPropertyIncomeMaybe
      .fold(
        UkOtherPropertyIncome(None, None, None, None, None, Some(RentARoomIncome(ukRaRAbout.totalIncomeAmount)))
      )(
        _.copy(
          ukOtherRentARoom = Some(RentARoomIncome(ukRaRAbout.totalIncomeAmount))
        )
      )

    val ukOtherPropertyExpenses: UkOtherPropertyExpenses = ukOtherPropertyExpensesMaybe
      .fold(
        UkOtherPropertyExpenses(
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          ukRaRAbout.claimExpensesOrRRR.rentARoomAmount.map(UkRentARoomExpense(_)),
          None
        )
      )(_.copy(ukOtherRentARoom = ukRaRAbout.claimExpensesOrRRR.rentARoomAmount.map(UkRentARoomExpense(_))))

    val requestWithEmptyOtherPropertyIncomeAndExpenses = CreatePropertyPeriodicSubmissionRequest(
      periodicSubmissionMaybe.map(_.fromDate).getOrElse(LocalDate.now()), // Todo:
      periodicSubmissionMaybe.map(_.toDate).getOrElse(LocalDate.now()), // Todo:
      periodicSubmissionMaybe.flatMap(_.foreignFhlEea),
      periodicSubmissionMaybe.flatMap(_.foreignProperty),
      periodicSubmissionMaybe.flatMap(_.ukFhlProperty),
      Some(
        UkOtherProperty(
          None,
          None // Todo: Change here, move consolidated to separate part!
        )
      )
    )

    val resultWithIncome: CreatePropertyPeriodicSubmissionRequest =
      updateUkOtherPropertiesIncome(ukOtherPropertyIncome, requestWithEmptyOtherPropertyIncomeAndExpenses)

    val result = updateUkOtherPropertiesExpenses(ukOtherPropertyExpenses, resultWithIncome)

    result.asRight[ServiceError]
  }

  def fromExpenses(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    expenses: Expenses
  ): Either[ServiceError, CreatePropertyPeriodicSubmissionRequest] = {
    val (periodicSubmission, ukOtherPropertyIncome)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyIncome]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, _, _, Some(UkOtherProperty(Some(income), _)))) =>
          (Some(pps), Some(income))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }
    CreatePropertyPeriodicSubmissionRequest(
      periodicSubmissionMaybe.map(_.fromDate).getOrElse(LocalDate.now()), // Todo:
      periodicSubmissionMaybe.map(_.toDate).getOrElse(LocalDate.now()), // Todo:
      periodicSubmission.flatMap(_.foreignFhlEea),
      periodicSubmission.flatMap(_.foreignProperty),
      periodicSubmission.flatMap(_.ukFhlProperty),
      Some(
        UkOtherProperty(
          ukOtherPropertyIncome,
          Some(
            UkOtherPropertyExpenses(
              premisesRunningCosts = expenses.rentsRatesAndInsurance,
              repairsAndMaintenance = expenses.repairsAndMaintenanceCosts,
              financialCosts = expenses.loanInterest,
              professionalFees = expenses.otherProfessionalFee,
              costOfServices = expenses.costsOfServicesProvided,
              travelCosts = expenses.propertyBusinessTravelCost,
              other = expenses.otherAllowablePropertyExpenses,
              residentialFinancialCostsCarriedForward = periodicSubmission.flatMap(
                _.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCostsCarriedForward))
              ),
              ukOtherRentARoom =
                periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.ukOtherRentARoom))),
              consolidatedExpense =
                periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.consolidatedExpense))),
              residentialFinancialCost =
                periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCost)))
            )
          )
        )
      )
    ).asRight[ServiceError]

  }

  def fromUkOtherPropertyIncome(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    saveIncome: SaveIncome
  ): Either[ServiceError, CreatePropertyPeriodicSubmissionRequest] = {

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
      periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.income.flatMap(_.ukOtherRentARoom)))
    )

    val requestWithEmptyRentalsIncome = CreatePropertyPeriodicSubmissionRequest(
      periodicSubmissionMaybe.map(_.fromDate).getOrElse(LocalDate.now()), // Todo:
      periodicSubmissionMaybe.map(_.toDate).getOrElse(LocalDate.now()), // Todo:
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

    val result: CreatePropertyPeriodicSubmissionRequest =
      updateUkOtherPropertiesIncome(ukOtherPropertyIncome, requestWithEmptyRentalsIncome)
    result.asRight[ServiceError]
  }

  private def updateUkOtherPropertiesIncome(
    ukOtherPropertyIncome: UkOtherPropertyIncome,
    request: CreatePropertyPeriodicSubmissionRequest
  ): CreatePropertyPeriodicSubmissionRequest = {
    val ukOtherPropertyLens: Optional[CreatePropertyPeriodicSubmissionRequest, UkOtherProperty] =
      Optional[CreatePropertyPeriodicSubmissionRequest, UkOtherProperty] { ppsr =>
        ppsr match {
          case CreatePropertyPeriodicSubmissionRequest(_, _, _, _, _, None) => Some(UkOtherProperty(None, None))
          case CreatePropertyPeriodicSubmissionRequest(_, _, _, _, _, uopi) => uopi
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

  private def updateUkOtherPropertiesExpenses(
    ukOtherPropertyExpenses: UkOtherPropertyExpenses,
    request: CreatePropertyPeriodicSubmissionRequest
  ): CreatePropertyPeriodicSubmissionRequest = {
    val ukOtherPropertyLens: Optional[CreatePropertyPeriodicSubmissionRequest, UkOtherProperty] =
      Optional[CreatePropertyPeriodicSubmissionRequest, UkOtherProperty] { ppsr =>
        ppsr match {
          case CreatePropertyPeriodicSubmissionRequest(_, _, _, _, _, None) => Some(UkOtherProperty(None, None))
          case CreatePropertyPeriodicSubmissionRequest(_, _, _, _, _, uopi) => uopi
        }
      } { ukop => ppsr =>
        ppsr.copy(ukOtherProperty = Some(ukop))
      }

    val ukOtherPropertyExpensesLens: Optional[UkOtherProperty, UkOtherPropertyExpenses] =
      Optional[UkOtherProperty, UkOtherPropertyExpenses] { ukop =>
        ukop match {
          case UkOtherProperty(_, None) =>
            Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
          case UkOtherProperty(_, ukope) => ukope
        }
      } { ukope => ukop =>
        ukop.copy(expenses = Some(ukope))
      }

    val focusFromRequestOnToExpensesukOtherPropertyLens = ukOtherPropertyLens.andThen(ukOtherPropertyExpensesLens)
    val result = focusFromRequestOnToExpensesukOtherPropertyLens.replace(ukOtherPropertyExpenses)(request)
    result
  }
}
