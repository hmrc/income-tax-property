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
import models.errors.ServiceError
import models.request.{Expenses, SaveIncome}
import play.api.libs.json.{Json, OFormat}

//
final case class PropertyPeriodicSubmissionRequest( //submittedOn: Option[LocalDateTime],
                                                    foreignFhlEea: Option[ForeignFhlEea],
                                                    foreignProperty: Option[Seq[ForeignProperty]],
                                                    ukFhlProperty: Option[UkFhlProperty],
                                                    ukOtherProperty: Option[UkOtherProperty])

object PropertyPeriodicSubmissionRequest {
  implicit val format: OFormat[PropertyPeriodicSubmissionRequest] = Json.format[PropertyPeriodicSubmissionRequest]

  def fromExpenses(
                    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
                    expenses: Expenses
                  ): Either[ServiceError, PropertyPeriodicSubmissionRequest] = {

    //    val ukOtherPropertyIncomeWithPeriodicSubmission = for {
    //      periodicSubmission <- periodicSubmissionMaybe
    //      ukOtherPropertyIncome <- periodicSubmission.ukOtherProperty.map(_.income)
    //    } yield (periodicSubmission, ukOtherPropertyIncome)
    //
    //
    //    ukOtherPropertyIncomeWithPeriodicSubmission.fold[Either[ServiceError, PropertyPeriodicSubmissionRequest]](
    //      //Todo: Maybe elaborate each
    //      InternalError("No periodicSubmission / uk other property income is present, should be together with expenses").asLeft[PropertyPeriodicSubmissionRequest]
    //    )(result => {
    //
    //      val (periodicSubmission, ukOtherPropertyIncome) = result
    //      val ppsrLens = GenLens[PropertyPeriodicSubmissionRequest](_.ukOtherProperty)
    val (periodicSubmission, ukOtherPropertyIncome): (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyIncome]) =
    periodicSubmissionMaybe match {
      case Some(pps@PropertyPeriodicSubmission(_, _, _, _, _, _, _, Some(UkOtherProperty(Some(income), _)))) =>
        (Some(pps), Some(income))
      case Some(pps) => (Some(pps), None)
      case _ => (None, None)
    }
    PropertyPeriodicSubmissionRequest(
      periodicSubmission.flatMap(_.foreignFhlEea),
      periodicSubmission.flatMap(_.foreignProperty),
      periodicSubmission.flatMap(_.ukFhlProperty),
      Some(
        UkOtherProperty(
          ukOtherPropertyIncome,
          Some(UkOtherPropertyExpenses(
            premisesRunningCosts = expenses.rentsRatesAndInsurance,
            repairsAndMaintenance = expenses.repairsAndMaintenanceCosts,
            financialCosts = expenses.loanInterest,
            professionalFees = expenses.otherProfessionalFee,
            costOfServices = expenses.costsOfServicesProvided,
            travelCosts = expenses.propertyBusinessTravelCost,
            other = expenses.otherAllowablePropertyExpenses,
            residentialFinancialCostsCarriedForward = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCostsCarriedForward))),
            ukOtherRentARoom = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.ukOtherRentARoom))),
            consolidatedExpense = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.consolidatedExpense))),
            residentialFinancialCost = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCost)))
          ))
        )
      )
    ).asRight[ServiceError]

  }

  def fromUkOtherPropertyIncome(
                                 periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
                                 saveIncome: SaveIncome
                               ): Either[ServiceError, PropertyPeriodicSubmissionRequest] = {

    val (periodicSubmission, ukOtherPropertyExpenses): (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyExpenses]) =
      periodicSubmissionMaybe match {
        case Some(pps@PropertyPeriodicSubmission(_, _, _, _, _, _, _, Some(UkOtherProperty(_, Some(expenses))))) =>
          (Some(pps), Some(expenses))
        case Some(pps) => ((Some(pps), None))
        case _ => (None, None)
      }

    PropertyPeriodicSubmissionRequest(
      periodicSubmission.flatMap(_.foreignFhlEea),
      periodicSubmission.flatMap(_.foreignProperty),
      periodicSubmission.flatMap(_.ukFhlProperty),
      Some(
        UkOtherProperty(
          Some(UkOtherPropertyIncome(
            saveIncome.ukOtherPropertyIncome.premiumsOfLeaseGrant, saveIncome.ukOtherPropertyIncome.reversePremiums,
            saveIncome.ukOtherPropertyIncome.periodAmount, saveIncome.ukOtherPropertyIncome.taxDeducted,
            saveIncome.ukOtherPropertyIncome.otherIncome, saveIncome.ukOtherPropertyIncome.ukOtherRentARoom
          )),
          ukOtherPropertyExpenses.map(_.copy(consolidatedExpense = None)) //Todo: Change here, move consolidated to separate part!
        )
      )
    ).asRight[ServiceError]
  }
}