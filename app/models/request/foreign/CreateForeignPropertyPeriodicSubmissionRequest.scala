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

package models.request.foreign

import cats.implicits.catsSyntaxEitherId
import models.common.TaxYear
import models.errors.{InternalError, ServiceError}
import models.request.foreign.adjustments.ForeignPropertyAdjustmentsWithCountryCode
import models.request.foreign.expenses.{ConsolidatedExpenses, ForeignPropertyExpensesWithCountryCode}
import models.responses._
import monocle.Optional
import monocle.macros.GenLens
import play.api.libs.json.{JsValue, Json, OWrites, Writes}
import play.api.libs.ws.BodyWritable

import java.time.LocalDate

case class CreateForeignPropertyPeriodicSubmissionRequest(
  fromDate: LocalDate,
  toDate: LocalDate,
  foreignProperty: Option[Seq[ForeignProperty]]
)

object CreateForeignPropertyPeriodicSubmissionRequest {
  implicit val writes: OWrites[CreateForeignPropertyPeriodicSubmissionRequest] =
    Json.writes[CreateForeignPropertyPeriodicSubmissionRequest]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  def fromEntity[T](
    taxYear: TaxYear,
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    entity: T
  ): Either[ServiceError, CreateForeignPropertyPeriodicSubmissionRequest] = {
    val result = entity match {
      case e: ForeignPropertyTaxWithCountryCode         => fromForeignPropertyTax(taxYear, periodicSubmissionMaybe, e)
      case e: ForeignPropertyExpensesWithCountryCode    => fromForeignPropertyExpenses(taxYear, periodicSubmissionMaybe, e)
      case e: ForeignIncomeWithCountryCode              => fromForeignIncome(taxYear, periodicSubmissionMaybe, e)
      case e: ForeignPropertyAdjustmentsWithCountryCode => fromForeignAdjustments(taxYear, periodicSubmissionMaybe, e)
      case _ =>
        InternalError("No relevant entity found to convert from (to CreateForeignPropertyPeriodicSubmissionRequest)")
          .asLeft[CreateForeignPropertyPeriodicSubmissionRequest]
    }

    result

  }

  def fromForeignPropertyTax(
    taxYear: TaxYear,
    maybeSubmission: Option[PropertyPeriodicSubmission],
    foreignPropertyTaxWithCountryCode: ForeignPropertyTaxWithCountryCode
  ): Either[ServiceError, CreateForeignPropertyPeriodicSubmissionRequest] = {

    val foreignPropertyLens = GenLens[CreateForeignPropertyPeriodicSubmissionRequest](_.foreignProperty)
    val foreignPropertyIncomeLens = GenLens[ForeignProperty](_.income)
    val firstForeignPropertyIncomeLens
      : Optional[CreateForeignPropertyPeriodicSubmissionRequest, ForeignPropertyIncome] =
      foreignPropertyLens.some.index(0).andThen(foreignPropertyIncomeLens.some)

    val (periodicSubmission, maybeForeignPropertyExpenses, maybeForeignPropertyIncome)
      : (Option[PropertyPeriodicSubmission], Option[ForeignPropertyExpenses], Option[ForeignPropertyIncome]) =
      maybeSubmission match {
        case Some(
              pps @ PropertyPeriodicSubmission(
                _,
                _,
                _,
                _,
                Some(Seq(ForeignProperty(_, Some(income), Some(expenses)))),
                _
              )
            ) =>
          (Some(pps), Some(expenses), Some(income))
        case Some(pps) => (Some(pps), None, None)
        case _         => (None, None, None)
      }

    val foreignPropertyIncome = ForeignPropertyIncome(
      rentIncome = maybeForeignPropertyIncome.flatMap(_.rentIncome),
      isForeignTaxCreditRelief = foreignPropertyTaxWithCountryCode.isForeignTaxCreditRelief,
      premiumsOfLeaseGrant = maybeForeignPropertyIncome.flatMap(_.premiumsOfLeaseGrant),
      otherPropertyIncome = maybeForeignPropertyIncome.flatMap(_.otherPropertyIncome),
      foreignTaxPaidOrDeducted =
        Some(foreignPropertyTaxWithCountryCode.foreignIncomeTax.flatMap(_.foreignTaxPaidOrDeducted).getOrElse(0)),
      specialWithholdingTaxOrUkTaxPaid = maybeForeignPropertyIncome.flatMap(_.specialWithholdingTaxOrUkTaxPaid)
    )

    val requestWithEmptyForeignPropertyIncome = CreateForeignPropertyPeriodicSubmissionRequest(
      periodicSubmission.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmission.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        Seq(
          ForeignProperty(
            foreignPropertyTaxWithCountryCode.countryCode,
            Some(ForeignPropertyIncome(None, None, None, None, None, None)),
            maybeForeignPropertyExpenses
          )
        )
      )
    )

    val updatedRequest: CreateForeignPropertyPeriodicSubmissionRequest =
      firstForeignPropertyIncomeLens.replace(foreignPropertyIncome)(requestWithEmptyForeignPropertyIncome)
    Right(updatedRequest)
  }

  def fromForeignPropertyExpenses(
    taxYear: TaxYear,
    maybeSubmission: Option[PropertyPeriodicSubmission],
    foreignPropertyExpenses: ForeignPropertyExpensesWithCountryCode
  ): Either[ServiceError, CreateForeignPropertyPeriodicSubmissionRequest] = {
    val foreignPropertyLens = GenLens[CreateForeignPropertyPeriodicSubmissionRequest](_.foreignProperty)
    val foreignPropertyExpenseLens = GenLens[ForeignProperty](_.expenses)
    val firstForeignPropertyExpenseLens
      : Optional[CreateForeignPropertyPeriodicSubmissionRequest, ForeignPropertyExpenses] =
      foreignPropertyLens.some.index(0).andThen(foreignPropertyExpenseLens.some)

    val (periodicSubmission, maybeForeignPropertyExpenses, maybeForeignPropertyIncome)
      : (Option[PropertyPeriodicSubmission], Option[ForeignPropertyExpenses], Option[ForeignPropertyIncome]) =
      maybeSubmission match {
        case Some(
              pps @ PropertyPeriodicSubmission(
                _,
                _,
                _,
                _,
                Some(Seq(ForeignProperty(_, Some(income), Some(expenses)))),
                _
              )
            ) =>
          (Some(pps), Some(expenses), Some(income))
        case Some(pps) => (Some(pps), None, None)
        case _         => (None, None, None)
      }

    val newForeignPropertyExpenses = foreignPropertyExpenses.consolidatedExpenses match {
      case Some(ConsolidatedExpenses(_, Some(consolidatedExpense))) =>
        ForeignPropertyExpenses(
          premisesRunningCosts = None,
          repairsAndMaintenance = None,
          financialCosts = None,
          professionalFees = None,
          travelCosts = None,
          costOfServices = None,
          residentialFinancialCost = maybeForeignPropertyExpenses.flatMap(_.residentialFinancialCost),
          broughtFwdResidentialFinancialCost =
            maybeForeignPropertyExpenses.flatMap(_.broughtFwdResidentialFinancialCost),
          other = None,
          consolidatedExpense = None,
          consolidatedExpenseAmount = Some(consolidatedExpense)
        )
      case _ =>
        ForeignPropertyExpenses(
          premisesRunningCosts = foreignPropertyExpenses.premisesRunningCosts,
          repairsAndMaintenance = foreignPropertyExpenses.repairsAndMaintenance,
          financialCosts = foreignPropertyExpenses.financialCosts,
          professionalFees = foreignPropertyExpenses.professionalFees,
          travelCosts = maybeForeignPropertyExpenses.flatMap(_.travelCosts),
          costOfServices = foreignPropertyExpenses.costOfServices,
          residentialFinancialCost = maybeForeignPropertyExpenses.flatMap(_.residentialFinancialCost),
          broughtFwdResidentialFinancialCost =
            maybeForeignPropertyExpenses.flatMap(_.broughtFwdResidentialFinancialCost),
          other = foreignPropertyExpenses.other,
          consolidatedExpense = None,
          consolidatedExpenseAmount = None
        )
    }

    val periodicSubmissionRequestRetainingIncome = CreateForeignPropertyPeriodicSubmissionRequest(
      periodicSubmission.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmission.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        Seq(
          ForeignProperty(
            foreignPropertyExpenses.countryCode,
            maybeForeignPropertyIncome,
            Some(ForeignPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
          )
        )
      )
    )

    val periodicSubmissionRequestWithNewForeignPropertyExpenses: CreateForeignPropertyPeriodicSubmissionRequest =
      firstForeignPropertyExpenseLens.replace(newForeignPropertyExpenses)(periodicSubmissionRequestRetainingIncome)

    Right(periodicSubmissionRequestWithNewForeignPropertyExpenses)
  }

  def fromForeignIncome(
    taxYear: TaxYear,
    maybeSubmission: Option[PropertyPeriodicSubmission],
    foreignIncome: ForeignIncomeWithCountryCode
  ): Either[ServiceError, CreateForeignPropertyPeriodicSubmissionRequest] = {

    val foreignPropertyLens = GenLens[CreateForeignPropertyPeriodicSubmissionRequest](_.foreignProperty)
    val foreignPropertyIncomeLens = GenLens[ForeignProperty](_.income)
    val firstForeignPropertyIncomeLens
      : Optional[CreateForeignPropertyPeriodicSubmissionRequest, ForeignPropertyIncome] =
      foreignPropertyLens.some.index(0).andThen(foreignPropertyIncomeLens.some)

    val (periodicSubmission, maybeForeignPropertyExpenses, maybeForeignPropertyIncome)
      : (Option[PropertyPeriodicSubmission], Option[ForeignPropertyExpenses], Option[ForeignPropertyIncome]) =
      maybeSubmission match {
        case Some(
              pps @ PropertyPeriodicSubmission(
                _,
                _,
                _,
                _,
                Some(Seq(ForeignProperty(_, Some(income), Some(expenses)))),
                _
              )
            ) =>
          (Some(pps), Some(expenses), Some(income))
        case Some(pps) => (Some(pps), None, None)
        case _         => (None, None, None)
      }

    val newForeignPropertyIncome = ForeignPropertyIncome(
      rentIncome = Some(ForeignPropertyRentIncome(foreignIncome.rentIncome)),
      otherPropertyIncome = Some(foreignIncome.otherPropertyIncome),
      premiumsOfLeaseGrant = foreignIncome.premiumsOfLeaseGrantAgreed.fold(
        maybeForeignPropertyIncome.flatMap(_.premiumsOfLeaseGrant)
      )(_.premiumsOfLeaseGrant),
      isForeignTaxCreditRelief = maybeForeignPropertyIncome.flatMap(_.isForeignTaxCreditRelief),
      foreignTaxPaidOrDeducted = maybeForeignPropertyIncome.flatMap(_.foreignTaxPaidOrDeducted),
      specialWithholdingTaxOrUkTaxPaid = maybeForeignPropertyIncome.flatMap(_.specialWithholdingTaxOrUkTaxPaid)
    )

    val periodicSubmissionRequestRetainingExpenses = CreateForeignPropertyPeriodicSubmissionRequest(
      periodicSubmission.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmission.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        Seq(
          ForeignProperty(
            foreignIncome.countryCode,
            Some(ForeignPropertyIncome(None, None, None, None, None, None)), // This is required for Lens to work
            maybeForeignPropertyExpenses
          )
        )
      )
    )

    val periodicSubmissionRequestWithNewForeignIncome: CreateForeignPropertyPeriodicSubmissionRequest =
      firstForeignPropertyIncomeLens.replace(newForeignPropertyIncome)(periodicSubmissionRequestRetainingExpenses)

    Right(periodicSubmissionRequestWithNewForeignIncome)
  }

  def fromForeignAdjustments(
    taxYear: TaxYear,
    maybeSubmission: Option[PropertyPeriodicSubmission],
    foreignAdjustmentsWithCountryCode: ForeignPropertyAdjustmentsWithCountryCode
  ): Either[ServiceError, CreateForeignPropertyPeriodicSubmissionRequest] = {

    val foreignPropertyLens = GenLens[CreateForeignPropertyPeriodicSubmissionRequest](_.foreignProperty)
    val foreignPropertyExpenseLens = GenLens[ForeignProperty](_.expenses)
    val firstForeignPropertyExpenseLens
    : Optional[CreateForeignPropertyPeriodicSubmissionRequest, ForeignPropertyExpenses] =
      foreignPropertyLens.some.index(0).andThen(foreignPropertyExpenseLens.some)

    val (periodicSubmission, maybeForeignPropertyExpenses, maybeForeignPropertyIncome)
    : (Option[PropertyPeriodicSubmission], Option[ForeignPropertyExpenses], Option[ForeignPropertyIncome]) =
      maybeSubmission match {
        case Some(
        pps @ PropertyPeriodicSubmission(
        _,
        _,
        _,
        _,
        Some(Seq(ForeignProperty(_, Some(income), Some(expenses)))),
        _
        )
        ) =>
          (Some(pps), Some(expenses), Some(income))
        case Some(pps) => (Some(pps), None, None)
        case _         => (None, None, None)
      }

    val newForeignPropertyExpenses = ForeignPropertyExpenses(
      residentialFinancialCost = foreignAdjustmentsWithCountryCode.residentialFinanceCost,
      broughtFwdResidentialFinancialCost =
        foreignAdjustmentsWithCountryCode.unusedResidentialFinanceCost.flatMap(_.foreignUnusedResidentialFinanceCostAmount),
      premisesRunningCosts = maybeForeignPropertyExpenses.flatMap(_.premisesRunningCosts),
      repairsAndMaintenance = maybeForeignPropertyExpenses.flatMap(_.repairsAndMaintenance),
      financialCosts = maybeForeignPropertyExpenses.flatMap(_.financialCosts),
      professionalFees = maybeForeignPropertyExpenses.flatMap(_.professionalFees),
      travelCosts = maybeForeignPropertyExpenses.flatMap(_.travelCosts),
      costOfServices = maybeForeignPropertyExpenses.flatMap(_.costOfServices),
      other = maybeForeignPropertyExpenses.flatMap(_.other),
      consolidatedExpense = None,
      consolidatedExpenseAmount = None
    )
    val periodicSubmissionRequestRetainingIncome = CreateForeignPropertyPeriodicSubmissionRequest(
      periodicSubmission.map(_.fromDate).getOrElse(LocalDate.parse(TaxYear.startDate(taxYear))),
      periodicSubmission.map(_.toDate).getOrElse(LocalDate.parse(TaxYear.endDate(taxYear))),
      Some(
        Seq(
          ForeignProperty(
            foreignAdjustmentsWithCountryCode.countryCode,
            maybeForeignPropertyIncome,
            Some(ForeignPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
          )
        )
      )
    )
    val periodicSubmissionRequestWithNewForeignExpenses =
      firstForeignPropertyExpenseLens.replace(newForeignPropertyExpenses)(periodicSubmissionRequestRetainingIncome)
    Right(periodicSubmissionRequestWithNewForeignExpenses)
  }

}
