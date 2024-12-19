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
import models.errors.{InternalError, ServiceError}
import models.request.foreign.expenses.{ConsolidatedExpenses, ForeignPropertyExpensesWithCountryCode}
import models.responses._
import monocle.macros.GenLens
import monocle.{Lens, Optional}
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.libs.ws.BodyWritable

case class UpdateForeignPropertyPeriodicSubmissionRequest(foreignProperty: Option[Seq[ForeignProperty]])

object UpdateForeignPropertyPeriodicSubmissionRequest {
  implicit val writes: Writes[UpdateForeignPropertyPeriodicSubmissionRequest] =
    Json.writes[UpdateForeignPropertyPeriodicSubmissionRequest]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  def fromEntity[T](
    periodicSubmissionMaybe: Option[PropertyPeriodicSubmission],
    entity: T
  ): Either[ServiceError, UpdateForeignPropertyPeriodicSubmissionRequest] = {
    val result = entity match {

      case e @ ForeignPropertyTaxWithCountryCode(_, _, _) => fromForeignPropertyTax(periodicSubmissionMaybe, e)
      case e @ ForeignPropertyExpensesWithCountryCode(_, _, _, _, _, _, _, _) =>
        fromForeignPropertyExpenses(periodicSubmissionMaybe, e)

      case _ =>
        InternalError("No relevant entity found to convert from (to UpdateForeignPropertyPeriodicSubmissionRequest)")
          .asLeft[UpdateForeignPropertyPeriodicSubmissionRequest]
    }

    result
  }

  def fromForeignPropertyTax(
    maybeSubmission: Option[PropertyPeriodicSubmission],
    foreignPropertyTaxWithCountryCode: ForeignPropertyTaxWithCountryCode
  ): Either[ServiceError, UpdateForeignPropertyPeriodicSubmissionRequest] = {

    val targetCountryCode = foreignPropertyTaxWithCountryCode.countryCode

    val foreignPropertyLens: Lens[UpdateForeignPropertyPeriodicSubmissionRequest, Option[Seq[ForeignProperty]]] =
      GenLens[UpdateForeignPropertyPeriodicSubmissionRequest](_.foreignProperty)

    val foreignPropertyIncomeLens: Lens[ForeignProperty, Option[ForeignPropertyIncome]] =
      GenLens[ForeignProperty](_.income)

    val filteredForeignPropertyLens: Optional[UpdateForeignPropertyPeriodicSubmissionRequest, ForeignProperty] =
      foreignPropertyLens.some.andThen(
        Optional[Seq[ForeignProperty], ForeignProperty](_.find(_.countryCode == targetCountryCode)) { fp => seq =>
          seq.map(existing => if (existing.countryCode == targetCountryCode) fp else existing)
        }
      )

    val filteredForeignPropertyIncomeLens
      : Optional[UpdateForeignPropertyPeriodicSubmissionRequest, ForeignPropertyIncome] =
      filteredForeignPropertyLens.andThen(foreignPropertyIncomeLens.some)

    val (maybeForeignPropertyExpenses, maybeForeignPropertyIncome)
      : (Option[ForeignPropertyExpenses], Option[ForeignPropertyIncome]) =
      maybeSubmission match {
        case Some(
        PropertyPeriodicSubmission(
                _,
                _,
                _,
                _,
                Some(Seq(ForeignProperty(_, Some(income), Some(expenses)))),
                _
              )
            ) =>
          (Some(expenses), Some(income))
        case _ => (None, None)
      }

    val foreignPropertyIncome = ForeignPropertyIncome(
      rentIncome = maybeForeignPropertyIncome.flatMap(_.rentIncome),
      foreignTaxCreditRelief = foreignPropertyTaxWithCountryCode.foreignTaxCreditRelief,
      premiumsOfLeaseGrant = maybeForeignPropertyIncome.flatMap(_.premiumsOfLeaseGrant),
      otherPropertyIncome = maybeForeignPropertyIncome.flatMap(_.otherPropertyIncome),
      foreignTaxPaidOrDeducted =
        Some(foreignPropertyTaxWithCountryCode.foreignIncomeTax.flatMap(_.foreignTaxPaidOrDeducted).getOrElse(0)),
      specialWithholdingTaxOrUkTaxPaid = maybeForeignPropertyIncome.flatMap(_.specialWithholdingTaxOrUkTaxPaid)
    )

    val requestWithEmptyForeignPropertyIncome = UpdateForeignPropertyPeriodicSubmissionRequest(
      Some(
        Seq(
          ForeignProperty(
            targetCountryCode,
            Some(ForeignPropertyIncome(None, None, None, None, None, None)),
            maybeForeignPropertyExpenses
          )
        )
      )
    )

    val updatedRequest =
      filteredForeignPropertyIncomeLens.replace(foreignPropertyIncome)(requestWithEmptyForeignPropertyIncome)
    Right(updatedRequest)
  }

  def fromForeignPropertyExpenses(
                                   maybeSubmission: Option[PropertyPeriodicSubmission],
                                   foreignPropertyExpenses: ForeignPropertyExpensesWithCountryCode
                                 ): Either[ServiceError, UpdateForeignPropertyPeriodicSubmissionRequest] = {

    val targetCountryCode = foreignPropertyExpenses.countryCode

    val foreignPropertyLens = GenLens[UpdateForeignPropertyPeriodicSubmissionRequest](_.foreignProperty)
    val foreignPropertyExpenseLens = GenLens[ForeignProperty](_.expenses)
    val filteredForeignPropertyLens: Optional[UpdateForeignPropertyPeriodicSubmissionRequest, ForeignProperty] =
      foreignPropertyLens.some.andThen(
        Optional[Seq[ForeignProperty], ForeignProperty](_.find(_.countryCode == targetCountryCode)) {
          fp => seq =>
            seq.map(existing => if (existing.countryCode == targetCountryCode) fp else existing)
        }
      )
    val filteredForeignPropertyExpensesLens: Optional[UpdateForeignPropertyPeriodicSubmissionRequest, ForeignPropertyExpenses] =
      filteredForeignPropertyLens.andThen(foreignPropertyExpenseLens.some)
    val (maybeForeignPropertyExpenses, maybeForeignPropertyIncome)
    : (Option[ForeignPropertyExpenses], Option[ForeignPropertyIncome]) =
      maybeSubmission match {
        case Some(periodicSubmission) =>
          periodicSubmission.foreignProperty.flatMap(_.find(_.countryCode == targetCountryCode)) match {
            case Some(ForeignProperty(_, Some(income), Some(expenses))) =>
              (Some(expenses), Some(income))
            case _ => (None, None)
          }
        case _         => (None, None)
      }

    val newForeignPropertyExpenses = foreignPropertyExpenses.consolidatedExpenses match {
      case Some(ConsolidatedExpenses(_, Some(consolidatedExpense))) => ForeignPropertyExpenses(
        premisesRunningCosts = None,
        repairsAndMaintenance = None,
        financialCosts = None,
        professionalFees = None,
        travelCosts = None,
        costOfServices = None,
        residentialFinancialCost = maybeForeignPropertyExpenses.flatMap(_.residentialFinancialCost),
        broughtFwdResidentialFinancialCost = maybeForeignPropertyExpenses.flatMap(_.broughtFwdResidentialFinancialCost),
        other = None,
        consolidatedExpense = Some(consolidatedExpense),
        consolidatedExpenseAmount = None
      )
      case _ => ForeignPropertyExpenses(
        premisesRunningCosts = foreignPropertyExpenses.premisesRunningCosts,
        repairsAndMaintenance = foreignPropertyExpenses.repairsAndMaintenance,
        financialCosts = foreignPropertyExpenses.financialCosts,
        professionalFees = foreignPropertyExpenses.professionalFees,
        travelCosts = maybeForeignPropertyExpenses.flatMap(_.travelCosts),
        costOfServices = foreignPropertyExpenses.costOfServices,
        residentialFinancialCost = maybeForeignPropertyExpenses.flatMap(_.residentialFinancialCost),
        broughtFwdResidentialFinancialCost = maybeForeignPropertyExpenses.flatMap(_.broughtFwdResidentialFinancialCost),
        other = foreignPropertyExpenses.other,
        consolidatedExpense = None,
        consolidatedExpenseAmount = None
      )
    }

    val periodicSubmissionRequestRetainingIncome = UpdateForeignPropertyPeriodicSubmissionRequest(
      Some(
        Seq(
          ForeignProperty(
            targetCountryCode,
            maybeForeignPropertyIncome,
            Some(ForeignPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
          )
        )
      )
    )
    val periodicSubmissionRequestWithNewForeignExpenses =
      filteredForeignPropertyExpensesLens.replace(newForeignPropertyExpenses)(periodicSubmissionRequestRetainingIncome)
    Right(periodicSubmissionRequestWithNewForeignExpenses)
  }

}
