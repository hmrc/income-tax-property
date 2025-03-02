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
import models.common.TaxYear
import models.errors.{InternalError, ServiceError}
import models.responses._
import models.{RentalsAndRaRAbout, responses}
import monocle.Optional
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.libs.ws.BodyWritable

import java.time.LocalDate

final case class CreateUKPropertyPeriodicSubmissionRequest(
  fromDate: LocalDate,
  toDate: LocalDate,
  ukOtherProperty: Option[UkOtherProperty]
)

object CreateUKPropertyPeriodicSubmissionRequest {

  implicit val writes: Writes[CreateUKPropertyPeriodicSubmissionRequest] =
    Json.writes[CreateUKPropertyPeriodicSubmissionRequest]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  def fromEntity[T](
    taxYear: TaxYear,
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    entity: T
  ): Either[ServiceError, CreateUKPropertyPeriodicSubmissionRequest] = {
    val result = entity match {
      case e @ RaRAbout(_, _, _)                   => fromUkRaRAbout(taxYear, periodicSubmissionMaybe, e)
      case e @ Expenses(_, _, _, _, _, _, _, _)    => fromExpenses(taxYear, periodicSubmissionMaybe, e)
      case e @ RentARoomExpenses(_, _, _, _, _, _) => fromRaRExpenses(taxYear, periodicSubmissionMaybe, e)
      case e @ PropertyRentalsIncome(_, _, _, _, _, _, _, _, _) =>
        fromPropertyRentalsIncome(taxYear, periodicSubmissionMaybe, e)
      case e @ RentalsAndRaRIncome(_, _, _, _, _, _, _, _) =>
        fromRentalsAndRaRIncome(taxYear, periodicSubmissionMaybe, e)
      case e @ PropertyRentalAdjustments(_, _, _, _, _, _) =>
        fromPropertyRentalAdjustments(taxYear, periodicSubmissionMaybe, e).asRight[ServiceError]
      case e @ RentalsAndRaRAbout(_, _, _, _, _) => fromRentalsAndRaRAbout(taxYear, periodicSubmissionMaybe, e)

      case _ =>
        InternalError("No relevant entity found to convert from (to UpdatePropertyPeriodicSubmissionRequest)")
          .asLeft[CreateUKPropertyPeriodicSubmissionRequest]
    }

    result.map(r => r.copy(ukOtherProperty = r.ukOtherProperty.flatMap(UkOtherProperty.convertToNoneIfAllFieldsNone)))

  }

  private def fromPropertyPeriodicSubmission(
    taxYear: TaxYear,
    maybePropertyPeriodicSubmission: Option[PropertyPeriodicSubmission]
  ): CreateUKPropertyPeriodicSubmissionRequest = maybePropertyPeriodicSubmission match {
    case Some(propertyPeriodicSubmission) =>
      CreateUKPropertyPeriodicSubmissionRequest(
        LocalDate.parse(TaxYear.startDate(taxYear)),
        LocalDate.parse(TaxYear.endDate(taxYear)),
        propertyPeriodicSubmission.ukOtherProperty
      )
    case None =>
      CreateUKPropertyPeriodicSubmissionRequest(
        LocalDate.parse(TaxYear.startDate(taxYear)),
        LocalDate.parse(TaxYear.endDate(taxYear)),
        None
      )
  }

  def fromPropertyRentalAdjustments(
    taxYear: TaxYear,
    maybePeriodicSubmission: Option[PropertyPeriodicSubmission],
    propertyRentalAdjustments: PropertyRentalAdjustments
  ): CreateUKPropertyPeriodicSubmissionRequest = {

    val propertyPeriodicSubmission = fromPropertyPeriodicSubmission(taxYear, maybePeriodicSubmission)
    val expensesMaybe: Option[UkOtherPropertyExpenses] = propertyPeriodicSubmission.ukOtherProperty.flatMap(_.expenses)
    val ukOtherExpenses = expensesMaybe.fold(
      UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None, None)
    )(expenses => expenses)

    val expenses = ukOtherExpenses.copy(
      residentialFinancialCost = Some(propertyRentalAdjustments.residentialFinanceCost),
      residentialFinancialCostsCarriedForward = propertyRentalAdjustments.unusedResidentialFinanceCost
    )

    updateUkOtherPropertiesExpenses(expenses, propertyPeriodicSubmission)
  }

  def fromUkRaRAbout(
    taxYear: TaxYear,
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    ukRaRAbout: RaRAbout
  ): Either[ServiceError, CreateUKPropertyPeriodicSubmissionRequest] = {
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
          ukRaRAbout.claimExpensesOrRelief.rentARoomAmount.map(UkRentARoomExpense(_)),
          None,
          None
        )
      )(_.copy(ukOtherRentARoom = ukRaRAbout.claimExpensesOrRelief.rentARoomAmount.map(UkRentARoomExpense(_))))

    val requestWithEmptyOtherPropertyIncomeAndExpenses = CreateUKPropertyPeriodicSubmissionRequest(
      periodicSubmissionMaybe.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmissionMaybe.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        UkOtherProperty(
          None,
          None // Todo: Change here, move consolidated to separate part!
        )
      )
    )

    val resultWithIncome: CreateUKPropertyPeriodicSubmissionRequest =
      updateUkOtherPropertiesIncome(ukOtherPropertyIncome, requestWithEmptyOtherPropertyIncomeAndExpenses)

    val result = updateUkOtherPropertiesExpenses(ukOtherPropertyExpenses, resultWithIncome)

    result.asRight[ServiceError]
  }

  def fromRentalsAndRaRAbout(
    taxYear: TaxYear,
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    rentalsAndRaRAbout: RentalsAndRaRAbout
  ): Either[ServiceError, CreateUKPropertyPeriodicSubmissionRequest] = {
    val ukOtherPropertyIncomeMaybe: Option[responses.UkOtherPropertyIncome] =
      periodicSubmissionMaybe.flatMap(_.ukOtherProperty.flatMap(_.income))

    val ukOtherPropertyExpensesMaybe: Option[responses.UkOtherPropertyExpenses] =
      periodicSubmissionMaybe.flatMap(_.ukOtherProperty.flatMap(_.expenses))

    val ukOtherPropertyIncome: UkOtherPropertyIncome = ukOtherPropertyIncomeMaybe
      .fold(
        UkOtherPropertyIncome(
          None,
          None,
          Some(rentalsAndRaRAbout.propertyRentalIncome),
          None,
          None,
          Some(RentARoomIncome(rentalsAndRaRAbout.totalIncomeAmount))
        )
      )(
        _.copy(
          ukOtherRentARoom = Some(RentARoomIncome(rentalsAndRaRAbout.totalIncomeAmount))
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
          rentalsAndRaRAbout.claimExpensesOrRelief.rentARoomAmount.map(UkRentARoomExpense(_)),
          None,
          None
        )
      )(_.copy(ukOtherRentARoom = rentalsAndRaRAbout.claimExpensesOrRelief.rentARoomAmount.map(UkRentARoomExpense(_))))

    val requestWithEmptyOtherPropertyIncomeAndExpenses = CreateUKPropertyPeriodicSubmissionRequest(
      periodicSubmissionMaybe.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmissionMaybe.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        UkOtherProperty(
          None,
          None // Todo: Change here, move consolidated to separate part!
        )
      )
    )

    val resultWithIncome: CreateUKPropertyPeriodicSubmissionRequest =
      updateUkOtherPropertiesIncome(ukOtherPropertyIncome, requestWithEmptyOtherPropertyIncomeAndExpenses)

    val result = updateUkOtherPropertiesExpenses(ukOtherPropertyExpenses, resultWithIncome)

    result.asRight[ServiceError]
  }

  def fromExpenses(
    taxYear: TaxYear,
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    expenses: Expenses
  ): Either[ServiceError, CreateUKPropertyPeriodicSubmissionRequest] = {
    val (periodicSubmission, ukOtherPropertyIncome)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyIncome]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, Some(UkOtherProperty(Some(income), _)))) =>
          (Some(pps), Some(income))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }
    CreateUKPropertyPeriodicSubmissionRequest(
      periodicSubmissionMaybe.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmissionMaybe.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        UkOtherProperty(
          ukOtherPropertyIncome,
          Some(
            expenses.consolidatedExpenses match {
              case Some(ConsolidatedExpenses(true, Some(consolidatedExpenseAmount))) =>
                UkOtherPropertyExpenses().copy(consolidatedExpenses = Some(consolidatedExpenseAmount))
              case _ => UkOtherPropertyExpenses(
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
                residentialFinancialCost =
                  periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCost))),
                consolidatedExpenses = None,
                consolidatedExpense = None
              )
            }
          )
        )
      )
    ).asRight[ServiceError]

  }

  def fromRaRExpenses(
    taxYear: TaxYear,
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    raRExpenses: RentARoomExpenses
  ): Either[ServiceError, CreateUKPropertyPeriodicSubmissionRequest] = {
    val (periodicSubmission, ukOtherPropertyIncome)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyIncome]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, Some(UkOtherProperty(Some(income), _)))) =>
          (Some(pps), Some(income))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }

    CreateUKPropertyPeriodicSubmissionRequest(
      periodicSubmissionMaybe.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmissionMaybe.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        UkOtherProperty(
          ukOtherPropertyIncome,
          Some(
            raRExpenses.consolidatedExpenses match {
              case Some(ConsolidatedExpenses(true, Some(consolidatedExpenseAmount))) =>
                UkOtherPropertyExpenses().copy(consolidatedExpenses = Some(consolidatedExpenseAmount))
              case _ => UkOtherPropertyExpenses(
                premisesRunningCosts = raRExpenses.rentsRatesAndInsurance,
                repairsAndMaintenance = raRExpenses.repairsAndMaintenanceCosts,
                professionalFees = raRExpenses.legalManagementOtherFee,
                costOfServices = raRExpenses.costOfServicesProvided,
                residentialFinancialCost =
                  periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCost))),
                residentialFinancialCostsCarriedForward = periodicSubmission.flatMap(
                  _.ukOtherProperty.flatMap(_.expenses.flatMap(_.residentialFinancialCostsCarriedForward))
                ),
                other = raRExpenses.otherPropertyExpenses,
                financialCosts =
                  periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.financialCosts))),
                travelCosts = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.travelCosts))),
                ukOtherRentARoom =
                  periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.expenses.flatMap(_.ukOtherRentARoom))),
                consolidatedExpenses = None,
                consolidatedExpense = None
              )
            }
          )
        )
      )
    ).asRight[ServiceError]

  }

  def fromPropertyRentalsIncome(
    taxYear: TaxYear,
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    propertyRentalsIncome: PropertyRentalsIncome
  ): Either[ServiceError, CreateUKPropertyPeriodicSubmissionRequest] = {

    val (periodicSubmission, ukOtherPropertyExpenses)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyExpenses]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, Some(UkOtherProperty(_, Some(expenses))))) =>
          (Some(pps), Some(expenses))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }

    val ukOtherPropertyIncome = UkOtherPropertyIncome(
      premiumsOfLeaseGrant = propertyRentalsIncome.premiumsGrantLease.flatMap(_.premiumsGrantLease),
      reversePremiums = propertyRentalsIncome.reversePremiumsReceived.flatMap(_.reversePremiums),
      periodAmount = Some(propertyRentalsIncome.propertyRentalIncome),
      taxDeducted = propertyRentalsIncome.deductingTax.flatMap(_.taxDeductedAmount),
      otherIncome = Some(propertyRentalsIncome.otherIncomeFromProperty),
      ukOtherRentARoom = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.income.flatMap(_.ukOtherRentARoom)))
    )

    val requestWithEmptyRentalsIncome = CreateUKPropertyPeriodicSubmissionRequest(
      periodicSubmissionMaybe.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmissionMaybe.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        UkOtherProperty(
          None,
          ukOtherPropertyExpenses // Todo: Change here, move consolidated to separate part!
        )
      )
    )

    val result: CreateUKPropertyPeriodicSubmissionRequest =
      updateUkOtherPropertiesIncome(ukOtherPropertyIncome, requestWithEmptyRentalsIncome)
    result.asRight[ServiceError]
  }

  def fromRentalsAndRaRIncome(
    taxYear: TaxYear,
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    rentalsAndRaRIncome: RentalsAndRaRIncome
  ): Either[ServiceError, CreateUKPropertyPeriodicSubmissionRequest] = {

    val (periodicSubmission, ukOtherPropertyExpenses)
      : (Option[PropertyPeriodicSubmission], Option[UkOtherPropertyExpenses]) =
      periodicSubmissionMaybe match {
        case Some(pps @ PropertyPeriodicSubmission(_, _, _, _, _, Some(UkOtherProperty(_, Some(expenses))))) =>
          (Some(pps), Some(expenses))
        case Some(pps) => (Some(pps), None)
        case _         => (None, None)
      }

    val ukOtherPropertyIncome = UkOtherPropertyIncome(
      premiumsOfLeaseGrant = rentalsAndRaRIncome.calculatedFigureYourself
        .flatMap(_.amount)
        .orElse(rentalsAndRaRIncome.premiumsGrantLease.flatMap(_.premiumsGrantLease)),
      reversePremiums = rentalsAndRaRIncome.reversePremiumsReceived.flatMap(_.reversePremiums),
      periodAmount = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.income.flatMap(_.periodAmount))),
      taxDeducted = rentalsAndRaRIncome.deductingTax.flatMap(_.taxDeductedAmount),
      otherIncome = Some(rentalsAndRaRIncome.otherIncomeFromProperty),
      ukOtherRentARoom = periodicSubmission.flatMap(_.ukOtherProperty.flatMap(_.income.flatMap(_.ukOtherRentARoom)))
    )

    val requestWithEmptyRentalsIncome = CreateUKPropertyPeriodicSubmissionRequest(
      periodicSubmissionMaybe.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmissionMaybe.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        UkOtherProperty(
          None,
          ukOtherPropertyExpenses // Todo: Change here, move consolidated to separate part!
        )
      )
    )

    val result: CreateUKPropertyPeriodicSubmissionRequest =
      updateUkOtherPropertiesIncome(ukOtherPropertyIncome, requestWithEmptyRentalsIncome)
    result.asRight[ServiceError]
  }

  private def updateUkOtherPropertiesIncome(
    ukOtherPropertyIncome: UkOtherPropertyIncome,
    request: CreateUKPropertyPeriodicSubmissionRequest
  ): CreateUKPropertyPeriodicSubmissionRequest = {
    val ukOtherPropertyLens: Optional[CreateUKPropertyPeriodicSubmissionRequest, UkOtherProperty] =
      Optional[CreateUKPropertyPeriodicSubmissionRequest, UkOtherProperty] {
        case CreateUKPropertyPeriodicSubmissionRequest(_, _, None) => Some(UkOtherProperty(None, None))
        case CreateUKPropertyPeriodicSubmissionRequest(_, _, uopi) => uopi
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
    request: CreateUKPropertyPeriodicSubmissionRequest
  ): CreateUKPropertyPeriodicSubmissionRequest = {
    val ukOtherPropertyLens: Optional[CreateUKPropertyPeriodicSubmissionRequest, UkOtherProperty] =
      Optional[CreateUKPropertyPeriodicSubmissionRequest, UkOtherProperty] {
        case CreateUKPropertyPeriodicSubmissionRequest(_, _, None) => Some(UkOtherProperty(None, None))
        case CreateUKPropertyPeriodicSubmissionRequest(_, _, uopi) => uopi
      } { ukop => ppsr =>
        ppsr.copy(ukOtherProperty = Some(ukop))
      }

    val ukOtherPropertyExpensesLens: Optional[UkOtherProperty, UkOtherPropertyExpenses] =
      Optional[UkOtherProperty, UkOtherPropertyExpenses] {
        case UkOtherProperty(_, None) =>
          Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None, None))
        case UkOtherProperty(_, ukope) => ukope
      } { ukope => ukop =>
        ukop.copy(expenses = Some(ukope))
      }

    val focusFromRequestOnToExpensesukOtherPropertyLens = ukOtherPropertyLens.andThen(ukOtherPropertyExpensesLens)
    val result = focusFromRequestOnToExpensesukOtherPropertyLens.replace(ukOtherPropertyExpenses)(request)
    result
  }
}
