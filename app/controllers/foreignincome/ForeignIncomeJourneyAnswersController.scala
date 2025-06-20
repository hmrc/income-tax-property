/*
 * Copyright 2025 HM Revenue & Customs
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

package controllers.foreignincome

import actions.AuthorisedAction
import controllers.RequestHandler
import errorhandling.ErrorHandler
import models.common.{IncomeSourceId, JourneyName, Nino, TaxYear}
import models.request.foreignincome.ForeignIncomeDividendsWithCountryCode
import play.api.Logging
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import services.ForeignIncomeService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class ForeignIncomeJourneyAnswersController @Inject() (
                                                        foreignIncomeService: ForeignIncomeService,
                                                        auth: AuthorisedAction,
                                                        cc: ControllerComponents
                                                      )(implicit ec: ExecutionContext)
  extends BackendController(cc) with ErrorHandler with Logging with RequestHandler {

  def saveForeignIncomeDividends(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignIncomeDividendsWithCountryCode](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignIncomeDividends,
        request
      ) { (ctx, foreignIncomeDividendsWithCountryCode: ForeignIncomeDividendsWithCountryCode) =>
        handleResponse(NO_CONTENT) {
          foreignIncomeService.saveForeignIncomeDividends(ctx, nino, foreignIncomeDividendsWithCountryCode)
        }
      }
    }

}
