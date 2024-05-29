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

import cats.implicits.catsSyntaxEitherId
import models.errors.{InternalError, ServiceError}
import models.request.{Expenses, SaveIncome}
import play.api.libs.json.{Json, OFormat}

import java.time.LocalDateTime

//
final case class PropertyPeriodicSubmissionRequest(submittedOn: Option[LocalDateTime],
                                                   foreignFhlEea: Option[ForeignFhlEea],
                                                   foreignProperty: Option[Seq[ForeignProperty]],
                                                   ukFhlProperty: Option[UkFhlProperty],
                                                   ukOtherProperty: Option[UkOtherProperty])

object PropertyPeriodicSubmissionRequest {
  implicit val format: OFormat[PropertyPeriodicSubmissionRequest] = Json.format[PropertyPeriodicSubmissionRequest]

  def fromExpenses(periodicSubmissionMaybe: Option[PropertyPeriodicSubmission], expenses: Expenses): Either[ServiceError, PropertyPeriodicSubmissionRequest] = {

    val ukOtherPropertyIncomeWithPeriodicSubmission = for {
      periodicSubmission <- periodicSubmissionMaybe
      ukOtherPropertyIncome <- periodicSubmission.ukOtherProperty.map(_.income)
    } yield (periodicSubmission, ukOtherPropertyIncome)

    ukOtherPropertyIncomeWithPeriodicSubmission.fold[Either[ServiceError, PropertyPeriodicSubmissionRequest]](
      //Todo: Maybe elaborate each
      InternalError("No periodicSubmission / uk other property income is present, should be together with expenses").asLeft[PropertyPeriodicSubmissionRequest]
    )(result => {

      val (periodicSubmission, ukOtherPropertyIncome) = result
      PropertyPeriodicSubmissionRequest(
        periodicSubmission.submittedOn, //Todo: Change to now?
        periodicSubmission.foreignFhlEea,
        periodicSubmission.foreignProperty,
        periodicSubmission.ukFhlProperty,
        Some(
          UkOtherProperty(
            ukOtherPropertyIncome,
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
      ).asRight[ServiceError]
    }
    )
  }

  def fromUkOtherPropertyIncome(
                                 periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
                                 saveIncome: SaveIncome
                               ): Either[ServiceError, PropertyPeriodicSubmissionRequest] = {

    val ukOtherPropertyExpensesWithPeriodicSubmission: Option[(Option[PropertyPeriodicSubmission], UkOtherPropertyExpenses)] = (
      periodicSubmissionMaybe, saveIncome.ukOtherPropertyExpenses
    ) match {
      case (Some(pps@PropertyPeriodicSubmission(_, _, _, _, _, _, _, Some(UkOtherProperty(_, expenses)))), None) =>
        Some((Some(pps), expenses))
      case (None, Some(expenses)) => Some((None, expenses))
      case (Some(periodicSubmission), Some(expenses)) => Some((Some(periodicSubmission), expenses))
      case _ => None
    }

    ukOtherPropertyExpensesWithPeriodicSubmission.fold[Either[ServiceError, PropertyPeriodicSubmissionRequest]](
      //Todo: Maybe elaborate each
      InternalError("No periodicSubmission / uk other property expenses is present, should be together with expenses").asLeft[PropertyPeriodicSubmissionRequest]
    )(result => {

      val (periodicSubmission, ukOtherPropertyExpenses) = result
      PropertyPeriodicSubmissionRequest(
        periodicSubmission.flatMap(_.submittedOn), //Todo: Change to now?
        periodicSubmission.flatMap(_.foreignFhlEea),
        periodicSubmission.flatMap(_.foreignProperty),
        periodicSubmission.flatMap(_.ukFhlProperty),
        Some(
          UkOtherProperty(
            UkOtherPropertyIncome(
              saveIncome.ukOtherPropertyIncome.premiumsOfLeaseGrant, saveIncome.ukOtherPropertyIncome.reversePremiums,
              saveIncome.ukOtherPropertyIncome.periodAmount, saveIncome.ukOtherPropertyIncome.taxDeducted,
              saveIncome.ukOtherPropertyIncome.otherIncome, saveIncome.ukOtherPropertyIncome.ukOtherRentARoom
            ),
            ukOtherPropertyExpenses
          )
        )
      ).asRight[ServiceError]
    })
  }
}