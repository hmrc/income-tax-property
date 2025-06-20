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
import models.errors.{CannotParseJsonError, CannotReadJsonError}
import play.api.Logging
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, ControllerComponents, Result}
import services.{JourneyStatusService, PropertyService}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class PropertyController @Inject()(
  propertyService: PropertyService,
  journeyStatusService: JourneyStatusService,
  auth: AuthorisedAction,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
  extends BackendController(cc) with ErrorHandler with Logging with RequestHandler {


  def setStatus(taxYear: TaxYear, incomeSourceId: IncomeSourceId, journeyName: String): Action[AnyContent] =
    auth.async { implicit request =>
      val ctx =
        JourneyContext(taxYear, incomeSourceId, request.user.getMtditid, JourneyName.withNameInsensitive(journeyName))
      val requestBody = parseBody[JourneyStatusData](request)

      requestBody match {
        case Success(validatedRes) =>
          validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
            case JsSuccess(value, _) =>
              journeyStatusService.setStatus(ctx, value).value.map(_ => NoContent)
            case JsError(err) =>
              Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
          }
        case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
      }
    }

  def setForeignStatus(taxYear: TaxYear, incomeSourceId: IncomeSourceId, journeyName: String, countryCode: String): Action[AnyContent] =
    auth.async { implicit request =>
      val ctx =
        JourneyContext(taxYear, incomeSourceId, request.user.getMtditid, JourneyName.withNameInsensitive(journeyName))
      val requestBody = parseBody[JourneyStatusData](request)

      requestBody match {
        case Success(validatedRes) =>
          validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
            case JsSuccess(value, _) =>
              journeyStatusService.setForeignStatus(ctx, value, countryCode).value.map(_ => NoContent)
            case JsError(err) =>
              Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
          }
        case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
      }
    }

  def fetchPropertyData(taxYear: TaxYear, nino: Nino, incomeSourceId: IncomeSourceId): Action[AnyContent] = auth.async {
    implicit request =>
      withJourneyContext(taxYear, incomeSourceId, nino, JourneyName.AllJourneys, request) { ctx =>
        handleResponse(OK) {
          propertyService.getFetchedPropertyDataMerged(ctx, nino, incomeSourceId)
        }
      }
  }

  }
