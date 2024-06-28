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
import models.errors.{InternalError, ServiceError}
import models.responses
import models.responses._
import monocle.Optional
import play.api.libs.json.{Json, OFormat}

final case class UpdatePropertyPeriodicSubmissionRequest(
  foreignFhlEea: Option[ForeignFhlEea],
  foreignProperty: Option[Seq[ForeignProperty]],
  ukFhlProperty: Option[UkFhlProperty],
  ukOtherProperty: Option[UkOtherProperty]
)

object UpdatePropertyPeriodicSubmissionRequest {

//propertyIncomeAllowance: BigDecimal, // ToDo: Where is this used?
//  renovationAllowanceBalancingCharge: RenovationAllowanceBalancingCharge, //
//  residentialFinanceCost: BigDecimal,
//  unusedResidentialFinanceCost: BigDecimal
  implicit val format: OFormat[UpdatePropertyPeriodicSubmissionRequest] =
    Json.format[UpdatePropertyPeriodicSubmissionRequest]

  private def fromPropertyPeriodicSubmission(
    maybePropertyPeriodicSubmission: Option[PropertyPeriodicSubmission]
  ): UpdatePropertyPeriodicSubmissionRequest = maybePropertyPeriodicSubmission match {
    case Some(propertyPeriodicSubmission) =>
      UpdatePropertyPeriodicSubmissionRequest(
        propertyPeriodicSubmission.foreignFhlEea,
        propertyPeriodicSubmission.foreignProperty,
        propertyPeriodicSubmission.ukFhlProperty,
        propertyPeriodicSubmission.ukOtherProperty
      )
    case None =>
      UpdatePropertyPeriodicSubmissionRequest(
        None,
        None,
        None,
        None
      )
  }

  def fromPropertyRentalAdjustments(
    maybePeriodicSubmission: Option[PropertyPeriodicSubmission],
    propertyRentalAdjustments: PropertyRentalAdjustments
  ): UpdatePropertyPeriodicSubmissionRequest = {

    val propertyPeriodicSubmission = fromPropertyPeriodicSubmission(maybePeriodicSubmission)
    val expensesMaybe: Option[UkOtherPropertyExpenses] = propertyPeriodicSubmission.ukOtherProperty.flatMap(_.expenses)
    val ukOtherExpenses = expensesMaybe.fold(
      UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None)
    )(expenses => expenses)

    val expenses = ukOtherExpenses.copy(
      residentialFinancialCost = Some(propertyRentalAdjustments.residentialFinanceCost),
      residentialFinancialCostsCarriedForward = Some(propertyRentalAdjustments.unusedResidentialFinanceCost)
    )

    updateUkOtherPropertiesExpenses(expenses, propertyPeriodicSubmission)
  }

  def fromEntity[T](
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    entity: T
  ): Either[ServiceError, UpdatePropertyPeriodicSubmissionRequest] =
    entity match {
      case e @ RaRAbout(_, _, _)                                => fromUkRaRAbout(periodicSubmissionMaybe, e)
      case e @ Expenses(_, _, _, _, _, _, _, _)                 => fromExpenses(periodicSubmissionMaybe, e)
      case e @ RentARoomExpenses(_, _, _, _, _, _)              => fromRaRExpenses(periodicSubmissionMaybe, e)
      case e @ PropertyRentalsIncome(_, _, _, _, _, _, _, _, _) => fromPropertyRentalsIncome(periodicSubmissionMaybe, e)
      case e @ PropertyRentalAdjustments(_, _, _, _, _, _) =>
        fromPropertyRentalAdjustments(periodicSubmissionMaybe, e).asRight[ServiceError]
      case _ =>
        InternalError("No relevant entity found to convert from (to UpdatePropertyPeriodicSubmissionRequest)")
          .asLeft[UpdatePropertyPeriodicSubmissionRequest]
    }

  def fromUkRaRAbout(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    ukRaRAbout: RaRAbout
  ): Either[ServiceError, UpdatePropertyPeriodicSubmissionRequest] = {
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

    val requestWithEmptyOtherPropertyIncomeAndExpenses = UpdatePropertyPeriodicSubmissionRequest(
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

    val resultWithIncome: UpdatePropertyPeriodicSubmissionRequest =
      updateUkOtherPropertiesIncome(ukOtherPropertyIncome, requestWithEmptyOtherPropertyIncomeAndExpenses)

    val result = updateUkOtherPropertiesExpenses(ukOtherPropertyExpenses, resultWithIncome)

    result.asRight[ServiceError]
  }

  def fromExpenses(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    expenses: Expenses
  ): Either[ServiceError, UpdatePropertyPeriodicSubmissionRequest] = {
    val (periodicSubmission, ukOtherPropertyIncome)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyIncome]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, _, _, Some(UkOtherProperty(Some(income), _)))) =>
          (Some(pps), Some(income))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }
    UpdatePropertyPeriodicSubmissionRequest(
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

  def fromRaRExpenses(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    raRExpenses: RentARoomExpenses
  ): Either[ServiceError, UpdatePropertyPeriodicSubmissionRequest] = {
    val (periodicSubmission, ukOtherPropertyIncome)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyIncome]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, _, _, Some(UkOtherProperty(Some(income), _)))) =>
          (Some(pps), Some(income))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }
    UpdatePropertyPeriodicSubmissionRequest(
      periodicSubmission.flatMap(_.foreignFhlEea),
      periodicSubmission.flatMap(_.foreignProperty),
      periodicSubmission.flatMap(_.ukFhlProperty),
      Some(
        UkOtherProperty(
          ukOtherPropertyIncome,
          Some(
            UkOtherPropertyExpenses(
              premisesRunningCosts = raRExpenses.rentsRatesAndInsurance, // Recheck?
              repairsAndMaintenance = raRExpenses.repairsAndMaintenanceCosts,
              professionalFees = raRExpenses.legalManagementOtherFee,
              costOfServices = raRExpenses.costOfServicesProvided,
              residentialFinancialCost =
                periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCost))),
              residentialFinancialCostsCarriedForward = periodicSubmission.flatMap(
                _.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCostsCarriedForward))
              ),
              other = raRExpenses.otherPropertyExpenses,
              consolidatedExpense = raRExpenses.consolidatedExpenses.flatMap(_.consolidatedExpensesAmount),
              financialCosts =
                periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.financialCosts))),
              travelCosts = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.travelCosts))),
              ukOtherRentARoom =
                periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.ukOtherRentARoom)))
            )
          )
        )
      )
    ).asRight[ServiceError]

  }

  def fromPropertyRentalsIncome(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    propertyRentalsIncome: PropertyRentalsIncome
  ): Either[ServiceError, UpdatePropertyPeriodicSubmissionRequest] = {

    val (periodicSubmission, ukOtherPropertyExpenses)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyExpenses]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, _, _, Some(UkOtherProperty(_, Some(expenses))))) =>
          (Some(pps), Some(expenses))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }

    val ukOtherPropertyIncome = UkOtherPropertyIncome(
      premiumsOfLeaseGrant = propertyRentalsIncome.premiumsGrantLease.flatMap(_.premiumsGrantLease),
      reversePremiums = propertyRentalsIncome.reversePremiumsReceived.flatMap(_.amount),
      periodAmount = Some(propertyRentalsIncome.incomeFromPropertyRentals),
      taxDeducted = propertyRentalsIncome.deductingTax.flatMap(_.taxDeductedAmount),
      otherIncome = Some(propertyRentalsIncome.otherIncomeFromProperty),
      ukOtherRentARoom = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.income.flatMap(_.ukOtherRentARoom)))
    )

    val requestWithEmptyRentalsIncome = UpdatePropertyPeriodicSubmissionRequest(
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

    val result: UpdatePropertyPeriodicSubmissionRequest =
      updateUkOtherPropertiesIncome(ukOtherPropertyIncome, requestWithEmptyRentalsIncome)
    result.asRight[ServiceError]
  }

  private def updateUkOtherPropertiesIncome(
    ukOtherPropertyIncome: UkOtherPropertyIncome,
    request: UpdatePropertyPeriodicSubmissionRequest
  ): UpdatePropertyPeriodicSubmissionRequest = {
    val ukOtherPropertyLens: Optional[UpdatePropertyPeriodicSubmissionRequest, UkOtherProperty] =
      Optional[UpdatePropertyPeriodicSubmissionRequest, UkOtherProperty] {
        case UpdatePropertyPeriodicSubmissionRequest(_, _, _, None) => Some(UkOtherProperty(None, None))
        case UpdatePropertyPeriodicSubmissionRequest(_, _, _, uopi) => uopi
      } { ukop => ppsr =>
        ppsr.copy(ukOtherProperty = Some(ukop))
      }

    val ukOtherPropertyIncomeLens: Optional[UkOtherProperty, UkOtherPropertyIncome] =
      Optional[UkOtherProperty, UkOtherPropertyIncome] {
        case UkOtherProperty(None, _)  => Some(UkOtherPropertyIncome(None, None, None, None, None, None))
        case UkOtherProperty(ukopi, _) => ukopi
      } { ukopi => ukop =>
        ukop.copy(income = Some(ukopi))
      }

    val focusFromRequestOnToIncomeukOtherPropertyLens = ukOtherPropertyLens.andThen(ukOtherPropertyIncomeLens)
    val result = focusFromRequestOnToIncomeukOtherPropertyLens.replace(ukOtherPropertyIncome)(request)
    result
  }

  private def updateUkOtherPropertiesExpenses(
    ukOtherPropertyExpenses: UkOtherPropertyExpenses,
    request: UpdatePropertyPeriodicSubmissionRequest
  ): UpdatePropertyPeriodicSubmissionRequest = {
    val ukOtherPropertyLens: Optional[UpdatePropertyPeriodicSubmissionRequest, UkOtherProperty] =
      Optional[UpdatePropertyPeriodicSubmissionRequest, UkOtherProperty] {
        case UpdatePropertyPeriodicSubmissionRequest(_, _, _, None) => Some(UkOtherProperty(None, None))
        case UpdatePropertyPeriodicSubmissionRequest(_, _, _, uopi) => uopi
      } { ukop => ppsr =>
        ppsr.copy(ukOtherProperty = Some(ukop))
      }

    val ukOtherPropertyExpensesLens: Optional[UkOtherProperty, UkOtherPropertyExpenses] =
      Optional[UkOtherProperty, UkOtherPropertyExpenses] {
        case UkOtherProperty(_, None) =>
          Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
        case UkOtherProperty(_, ukope) => ukope
      } { ukope => ukop =>
        ukop.copy(expenses = Some(ukope))
      }

    val focusFromRequestOnToExpensesukOtherPropertyLens = ukOtherPropertyLens.andThen(ukOtherPropertyExpensesLens)
    val result = focusFromRequestOnToExpensesukOtherPropertyLens.replace(ukOtherPropertyExpenses)(request)
    result
  }
}
