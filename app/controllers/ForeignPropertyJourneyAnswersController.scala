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

package controllers

import actions.AuthorisedAction
import errorhandling.ErrorHandler
import models.common._
import models.request.foreign._
import models.request.foreign.adjustments.ForeignPropertyAdjustmentsWithCountryCode
import models.request.foreign.sba.ForeignPropertySbaWithCountryCode
import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.request.foreign.expenses.ForeignPropertyExpensesWithCountryCode
import play.api.Logging
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import services.ForeignPropertyService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class ForeignPropertyJourneyAnswersController @Inject() (
  foreignPropertyService: ForeignPropertyService,
  auth: AuthorisedAction,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BackendController(cc) with ErrorHandler with Logging with RequestHandler {

  def saveSelectCountrySection(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignPropertySelectCountry](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignPropertySelectCountry,
        request
      ) { (ctx, foreignPropertySelectCountry: ForeignPropertySelectCountry) =>
        handleResponse(NO_CONTENT) {
          foreignPropertyService.saveForeignPropertySelectCountry(ctx, foreignPropertySelectCountry)
        }
      }
    }

  def saveForeignPropertyTax(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino
  ): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignPropertyTaxWithCountryCode](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignPropertyTax,
        request
      ) { (ctx, foreignPropertyTaxWithCountryCode: ForeignPropertyTaxWithCountryCode) =>
        handleResponse(NO_CONTENT) {
          foreignPropertyService.saveForeignPropertyTax(ctx, nino, foreignPropertyTaxWithCountryCode)
        }
      }
    }

  def saveForeignPropertyExpenses(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignPropertyExpensesWithCountryCode](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignPropertyExpenses,
        request
      ) { (ctx, foreignPropertyExpensesWithCountryCode: ForeignPropertyExpensesWithCountryCode) =>
        handleResponse(NO_CONTENT) {
          foreignPropertyService.saveForeignPropertyExpenses(ctx, nino, foreignPropertyExpensesWithCountryCode)
        }
      }
    }

  def saveForeignIncome(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignIncomeWithCountryCode](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignPropertyIncome,
        request
      ) { (ctx, foreignIncome: ForeignIncomeWithCountryCode) =>
        handleResponse(NO_CONTENT) {
          foreignPropertyService.saveForeignIncome(ctx, nino, foreignIncome)
        }
      }
    }

  def saveForeignPropertyAllowances(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignPropertyAllowancesWithCountryCode](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignPropertyAllowances,
        request
      ) { (ctx, foreignPropertyAllowancesWithCountryCode: ForeignPropertyAllowancesWithCountryCode) =>
        handleResponse(NO_CONTENT) {
          foreignPropertyService.saveForeignPropertyAllowances(ctx, nino, foreignPropertyAllowancesWithCountryCode)
        }
      }
    }

  def saveForeignPropertyAdjustments(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignPropertyAdjustmentsWithCountryCode](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignPropertyAdjustments,
        request
      ) { (ctx, foreignPropertyAdjustmentsWithCountryCode: ForeignPropertyAdjustmentsWithCountryCode) =>
        handleResponse(NO_CONTENT) {
          foreignPropertyService.saveForeignPropertyAdjustments(ctx, nino, foreignPropertyAdjustmentsWithCountryCode)
        }
      }
    }

  def saveForeignPropertySba(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignPropertySbaWithCountryCode](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignPropertySba,
        request
      ) { (ctx, foreignPropertySbaWithCountryCode: ForeignPropertySbaWithCountryCode) =>
        handleResponse(NO_CONTENT) {
          foreignPropertyService.saveForeignPropertySba(ctx, nino, foreignPropertySbaWithCountryCode)
        }
      }
    }
}
