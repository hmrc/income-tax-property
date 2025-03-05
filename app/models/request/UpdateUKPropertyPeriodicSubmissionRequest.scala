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
import models.responses._
import models.{RentalsAndRaRAbout, responses}
import monocle.Optional
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.libs.ws.BodyWritable

final case class UpdateUKPropertyPeriodicSubmissionRequest(
  ukOtherProperty: Option[UkOtherProperty]
)

object UpdateUKPropertyPeriodicSubmissionRequest {

//propertyIncomeAllowance: BigDecimal, // ToDo: Where is this used?
//  renovationAllowanceBalancingCharge: RenovationAllowanceBalancingCharge, //
//  residentialFinanceCost: BigDecimal,
//  unusedResidentialFinanceCost: BigDecimal
  implicit val writes: Writes[UpdateUKPropertyPeriodicSubmissionRequest] =
    Json.writes[UpdateUKPropertyPeriodicSubmissionRequest]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  private def fromPropertyPeriodicSubmission(
    maybePropertyPeriodicSubmission: Option[PropertyPeriodicSubmission]
  ): UpdateUKPropertyPeriodicSubmissionRequest = maybePropertyPeriodicSubmission match {
    case Some(propertyPeriodicSubmission) =>
      UpdateUKPropertyPeriodicSubmissionRequest(
        propertyPeriodicSubmission.ukOtherProperty
      )
    case None =>
      UpdateUKPropertyPeriodicSubmissionRequest(None)
  }

  def fromPropertyRentalAdjustments(
    maybePeriodicSubmission: Option[PropertyPeriodicSubmission],
    propertyRentalAdjustments: PropertyRentalAdjustments
  ): UpdateUKPropertyPeriodicSubmissionRequest = {

    val expenses = UkOtherPropertyExpenses().copy(
      residentialFinancialCost = Some(propertyRentalAdjustments.residentialFinanceCost),
      residentialFinancialCostsCarriedForward = propertyRentalAdjustments.unusedResidentialFinanceCost
    )

    UpdateUKPropertyPeriodicSubmissionRequest(
      ukOtherProperty = Some(UkOtherProperty(
        income = None,
        expenses = Some(expenses)
      ))
    )
  }

  def fromEntity[T](
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    entity: T
  ): Either[ServiceError, UpdateUKPropertyPeriodicSubmissionRequest] = {
    val result = entity match {
      case e @ RaRAbout(_, _, _)                                => fromUkRaRAbout(periodicSubmissionMaybe, e)
      case e @ Expenses(_, _, _, _, _, _, _, _)                 => fromExpenses(periodicSubmissionMaybe, e)
      case e @ RentARoomExpenses(_, _, _, _, _, _)              => fromRaRExpenses(periodicSubmissionMaybe, e)
      case e @ PropertyRentalsIncome(_, _, _, _, _, _, _, _, _) => fromPropertyRentalsIncome(periodicSubmissionMaybe, e)
      case e @ RentalsAndRaRIncome(_, _, _, _, _, _, _, _)      => fromRentalsAndRaRIncome(periodicSubmissionMaybe, e)
      case e @ PropertyRentalAdjustments(_, _, _, _, _, _) =>
        fromPropertyRentalAdjustments(periodicSubmissionMaybe, e).asRight[ServiceError]
      case e @ RentalsAndRaRAbout(_, _, _, _, _) => fromRentalsAndRaRAbout(periodicSubmissionMaybe, e)
      case _ =>
        InternalError("No relevant entity found to convert from (to UpdateUKPropertyPeriodicSubmissionRequest)")
          .asLeft[UpdateUKPropertyPeriodicSubmissionRequest]
    }
    result.map(r =>
      r.copy(ukOtherProperty = r.ukOtherProperty.flatMap(UkOtherProperty.convertToNoneIfAllFieldsNone))
    )
  }

  def fromUkRaRAbout(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    ukRaRAbout: RaRAbout
  ): Either[ServiceError, UpdateUKPropertyPeriodicSubmissionRequest] = {

    val ukOtherPropertyIncome: UkOtherPropertyIncome = UkOtherPropertyIncome().copy(
      ukOtherRentARoom = Some(RentARoomIncome(ukRaRAbout.totalIncomeAmount))
    )

    UpdateUKPropertyPeriodicSubmissionRequest(
      ukOtherProperty = Some(UkOtherProperty(
        income = Some(ukOtherPropertyIncome),
        expenses = ukRaRAbout.claimExpensesOrRelief.rentARoomAmount.map { rentARoomAmount =>
          UkOtherPropertyExpenses().copy(
            ukOtherRentARoom = Some(UkRentARoomExpense(rentARoomAmount))
          )
        }
      ))
    ).asRight[ServiceError]
  }

  def fromRentalsAndRaRAbout(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    rentalsAndRaRAbout: RentalsAndRaRAbout
  ): Either[ServiceError, UpdateUKPropertyPeriodicSubmissionRequest] = {

    val ukOtherPropertyIncome: UkOtherPropertyIncome = UkOtherPropertyIncome().copy(
      periodAmount = Some(rentalsAndRaRAbout.propertyRentalIncome),
      ukOtherRentARoom = Some(RentARoomIncome(rentalsAndRaRAbout.totalIncomeAmount))
    )

    UpdateUKPropertyPeriodicSubmissionRequest(
      ukOtherProperty = Some(UkOtherProperty(
          income = Some(ukOtherPropertyIncome),
        expenses = rentalsAndRaRAbout.claimExpensesOrRelief.rentARoomAmount.map { rentARoomAmount =>
          UkOtherPropertyExpenses().copy(ukOtherRentARoom = Some(UkRentARoomExpense(rentARoomAmount)))
        }
      ))
    ).asRight[ServiceError]
  }

  def fromExpenses(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    expenses: Expenses
  ): Either[ServiceError, UpdateUKPropertyPeriodicSubmissionRequest] = {
    UpdateUKPropertyPeriodicSubmissionRequest(
      Some(
        UkOtherProperty(
          income = None,
          Some(
            expenses.consolidatedExpenses match {
              case Some(ConsolidatedExpenses(true, Some(consolidatedExpenseAmount))) =>
                UkOtherPropertyExpenses().copy(consolidatedExpense = Some(consolidatedExpenseAmount))
              case _ => UkOtherPropertyExpenses().copy(
                premisesRunningCosts = expenses.rentsRatesAndInsurance,
                repairsAndMaintenance = expenses.repairsAndMaintenanceCosts,
                financialCosts = expenses.loanInterest,
                professionalFees = expenses.otherProfessionalFee,
                costOfServices = expenses.costsOfServicesProvided,
                travelCosts = expenses.propertyBusinessTravelCost,
                other = expenses.otherAllowablePropertyExpenses,
                consolidatedExpense = None,
                consolidatedExpenses = None
              )
            }
          )
        )
      )
    ).asRight[ServiceError]

  }

  def fromRaRExpenses(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    raRExpenses: RentARoomExpenses
  ): Either[ServiceError, UpdateUKPropertyPeriodicSubmissionRequest] = {
    UpdateUKPropertyPeriodicSubmissionRequest(
      Some(
        UkOtherProperty(
          income = None,
          Some(
            raRExpenses.consolidatedExpenses match {
              case Some(ConsolidatedExpenses(true, Some(consolidatedExpenseAmount))) =>
                UkOtherPropertyExpenses().copy(consolidatedExpense = Some(consolidatedExpenseAmount))
              case _ => UkOtherPropertyExpenses().copy(
                premisesRunningCosts = raRExpenses.rentsRatesAndInsurance, // Recheck?
                repairsAndMaintenance = raRExpenses.repairsAndMaintenanceCosts,
                professionalFees = raRExpenses.legalManagementOtherFee,
                costOfServices = raRExpenses.costOfServicesProvided,
                other = raRExpenses.otherPropertyExpenses,
                consolidatedExpense = None,
                consolidatedExpenses = None
              )
            }
          )
        )
      )
    ).asRight[ServiceError]
  }

  def fromPropertyRentalsIncome(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    propertyRentalsIncome: PropertyRentalsIncome
  ): Either[ServiceError, UpdateUKPropertyPeriodicSubmissionRequest] = {

    val ukOtherPropertyIncome = UkOtherPropertyIncome().copy(
      premiumsOfLeaseGrant = propertyRentalsIncome.premiumsGrantLease.flatMap(_.premiumsGrantLease),
      reversePremiums = propertyRentalsIncome.reversePremiumsReceived.flatMap(_.reversePremiums),
      periodAmount = Some(propertyRentalsIncome.propertyRentalIncome),
      taxDeducted = propertyRentalsIncome.deductingTax.flatMap(_.taxDeductedAmount),
      otherIncome = Some(propertyRentalsIncome.otherIncomeFromProperty)
    )

    UpdateUKPropertyPeriodicSubmissionRequest(
      ukOtherProperty = Some(UkOtherProperty(
        income = Some(ukOtherPropertyIncome),
        expenses = None
      ))
    ).asRight[ServiceError]
  }

  def fromRentalsAndRaRIncome(
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    rentalsAndRaRIncome: RentalsAndRaRIncome
  ): Either[ServiceError, UpdateUKPropertyPeriodicSubmissionRequest] = {

    val ukOtherPropertyIncome = UkOtherPropertyIncome().copy(
      premiumsOfLeaseGrant = rentalsAndRaRIncome.calculatedFigureYourself
        .flatMap(_.amount)
        .orElse(rentalsAndRaRIncome.premiumsGrantLease.flatMap(_.premiumsGrantLease)),
      reversePremiums = rentalsAndRaRIncome.reversePremiumsReceived.flatMap(_.reversePremiums),
      taxDeducted = rentalsAndRaRIncome.deductingTax.flatMap(_.taxDeductedAmount),
      otherIncome = Some(rentalsAndRaRIncome.otherIncomeFromProperty)
    )

    UpdateUKPropertyPeriodicSubmissionRequest(
      ukOtherProperty = Some(
        UkOtherProperty(
          income = Some(ukOtherPropertyIncome),
          expenses = None
        )
      )
    ).asRight[ServiceError]
  }
}
