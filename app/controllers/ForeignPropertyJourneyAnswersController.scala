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
import models.RentalsAndRaRAbout
import models.common._
import models.errors.{CannotParseJsonError, CannotReadJsonError}
import models.request._
import models.request.esba.EsbaInfo
import models.request.esba.EsbaInfo._
import models.request.foreign.ForeignPropertiesSelectCountry
import models.request.sba.SbaInfo
import models.request.ukrentaroom.RaRAdjustments
import play.api.Logging
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, ControllerComponents, Result}
import services.{ForeignPropertyService, PropertyService}
import services.journeyAnswers.JourneyStatusService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.JsonSupport._

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class ForeignPropertyJourneyAnswersController @Inject() (
  propertyService: ForeignPropertyService,
  journeyStatusService: JourneyStatusService,
  auth: AuthorisedAction,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BackendController(cc) with ErrorHandler with Logging with RequestHandler {

  def saveSelectCountrySection(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[ForeignPropertiesSelectCountry](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.ForeignPropertySelectCountry,
        request
      ) { (ctx, foreignPropertiesSelectCountry: ForeignPropertiesSelectCountry) =>
        handleResponse(NO_CONTENT) {
          propertyService.saveForeignPropertiesSelectCountry(ctx, nino, foreignPropertiesSelectCountry)
        }
      }
    }
}
