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

package controllers

import actions.AuthorisedAction
import errorhandling.ErrorHandler
import models.common.{IncomeSourceId, JourneyName, Nino, TaxYear}
import models.request.ukAndForeign.UkAndForeignAbout
import play.api.Logging
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import services.UkAndForeignPropertyService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class UkAndForeignJourneyAnswersController @Inject() (
                                                       ukAndForeignPropertyService: UkAndForeignPropertyService,
                                                       auth: AuthorisedAction,
                                                       cc: ControllerComponents
                                                     )(implicit ec: ExecutionContext)
  extends BackendController(cc) with ErrorHandler with Logging with RequestHandler {

  def saveUkAndForeignPropertyAbout(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[UkAndForeignAbout](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.UkAndForeignPropertyAbout,
        request
      ) { (ctx, ukAndForeignAbout: UkAndForeignAbout) =>
        handleResponse(NO_CONTENT) {
          ukAndForeignPropertyService.saveUkAndForeignPropertyAbout(ctx, ukAndForeignAbout)
        }
      }
    }

}