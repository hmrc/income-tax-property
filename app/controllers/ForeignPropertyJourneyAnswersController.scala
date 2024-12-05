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
import models.request.foreign.expenses.ForeignPropertyExpenses
import models.request.foreign.{ForeignPropertySelectCountry, ForeignPropertyTaxWithCountryCode}
import play.api.Logging
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import services.ForeignPropertyService
import services.journeyAnswers.JourneyStatusService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class ForeignPropertyJourneyAnswersController @Inject() (
  propertyService: ForeignPropertyService,
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
          propertyService.saveForeignPropertySelectCountry(ctx,foreignPropertySelectCountry)
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
          propertyService.saveForeignPropertyTax(ctx, nino, foreignPropertyTaxWithCountryCode)
        }
      }
    }

  def saveForeignPropertyExpenses(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignPropertyExpenses](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignPropertyExpenses,
        request
      ) { (ctx, foreignPropertyExpenses: ForeignPropertyExpenses) =>
        handleResponse(NO_CONTENT) {
          propertyService.saveForeignPropertyExpenses(ctx, foreignPropertyExpenses)
        }
      }
    }
}
