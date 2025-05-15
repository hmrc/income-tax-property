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

package controllers.ukproperty

import actions.AuthorisedAction
import controllers.RequestHandler
import errorhandling.ErrorHandler
import models.RentalsAndRaRAbout
import models.common._
import models.errors.{CannotParseJsonError, CannotReadJsonError}
import models.request._
import models.request.esba.EsbaInfo
import models.request.esba.EsbaInfo._
import models.request.sba.SbaInfo
import models.request.ukrentaroom.RaRAdjustments
import play.api.Logging
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, ControllerComponents, Result}
import services.PropertyService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.JsonSupport._

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class JourneyAnswersController @Inject() (
  propertyService: PropertyService,
  auth: AuthorisedAction,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BackendController(cc) with ErrorHandler with Logging with RequestHandler {

  def savePropertyAbout(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] = auth.async {
    implicit request =>
      withJourneyContextAndEntity[PropertyAbout](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.About,
        request
      ) { (ctx, about) =>
        handleResponse(NO_CONTENT) {
          propertyService.savePropertyAbout(ctx, about)
        }
      }
  }

  // Rentals
  def savePropertyRentalAbout(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[PropertyRentalsAbout](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.RentalAbout,
        request
      ) { (ctx, about) =>
        handleResponse(NO_CONTENT) {
          propertyService.savePropertyRentalAbout(ctx, about)
        }
      }
    }

  def savePropertyRentalAdjustments(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[PropertyRentalAdjustments](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.RentalAdjustments,
        request
      ) { (ctx, adjustments) =>
        handleResponse(NO_CONTENT) {
          propertyService.savePropertyRentalAdjustments(ctx, nino, adjustments)
        }
      }
    }

  def saveExpenses(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino,
    journeyName: String
  ): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[Expenses](taxYear, incomeSourceId, nino, JourneyName.withName(journeyName), request) {
        (ctx, expenses) =>
          handleResponse(CREATED) {
            propertyService.saveExpenses(ctx, nino, expenses)
          }
      }
    }

  def savePropertyRentalAllowances(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      val ctx = JourneyContext(taxYear, incomeSourceId, request.user.getMtditid, JourneyName.RentalAllowances)
      val requestBody = parseBody[RentalAllowances](request)

      requestBody match {
        case Success(validatedRes) =>
          validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
            case JsSuccess(value, _) =>
              propertyService.savePropertyRentalAllowances(ctx, nino, value).value.map(_ => NoContent)
            case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
          }
        case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
      }
    }

  def savePropertyRentalSBA(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[SbaInfo](taxYear, incomeSourceId, nino, JourneyName.RentalSBA, request) {
        (ctx, sbaInfo) =>
          handleResponse(NO_CONTENT) {
            propertyService.saveSBA(ctx, nino, sbaInfo)
          }
      }
    }

  def saveEsba(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContext(taxYear, incomeSourceId, nino, JourneyName.RentalESBA, request) { ctx =>
        withEntity[EsbaInfo](request) { esbaInfo =>
          handleResponse(NO_CONTENT) {
            propertyService.saveEsbas(ctx, nino, esbaInfo)
          }
        }
      }
    }

  def saveIncome(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[PropertyRentalsIncome](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.RentalIncome,
        request
      ) { (ctx, propertyRentalsIncome) =>
        handleResponse(CREATED) {
          propertyService.saveIncome(
            ctx,
            nino,
            propertyRentalsIncome
          )
        }
      }
    }

  // RAR
  def saveRentARoomAbout(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[RaRAbout](taxYear, incomeSourceId, nino, JourneyName.RentARoomAbout, request) {
        (ctx, rarAbout) =>
          handleResponse(CREATED) {
            propertyService.saveRaRAbout(ctx, nino, rarAbout)
          }
      }
    }

  def saveRentARoomAllowances(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      val ctx = JourneyContextWithNino(taxYear, incomeSourceId, request.user.getMtditid, nino)
      val requestBody = parseBody[RentARoomAllowances](request)

      requestBody match {
        case Success(validatedRes) =>
          validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
            case JsSuccess(value, _) =>
              propertyService.saveRentARoomAllowances(ctx, value).value.map(_ => NoContent)
            case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
          }
        case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
      }
    }

  def saveRaRExpenses(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino,
    journeyName: String
  ): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[RentARoomExpenses](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.withName(journeyName),
        request
      ) { (ctx, raRExpenses) =>
        handleResponse(CREATED) {
          propertyService.saveRaRExpenses(ctx, nino, raRExpenses)
        }
      }
    }

  def savePropertyRaRAdjustments(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[RaRAdjustments](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.RentARoomAdjustments,
        request
      ) { (ctx, annualRaRAdjustmentsBody) =>
        handleResponse(CREATED) {
          propertyService.saveRaRAdjustments(ctx, nino, annualRaRAdjustmentsBody)
        }
      }
    }

  // Rentals and RAR
  def saveRentalsAndRaRAbout(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino
  ): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[RentalsAndRaRAbout](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.RentalsAndRaRAbout,
        request
      ) { (ctx, rentalsAndRaRAbout) =>
        handleResponse(CREATED) {
          propertyService.saveRentalsAndRaRAbout(ctx, nino, rentalsAndRaRAbout)
        }
      }
    }

  def saveRentalsAndRaRIncome(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[RentalsAndRaRIncome](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.RentalsAndRaRIncome,
        request
      ) { (ctx, propertyRentalsIncome) =>
        handleResponse(CREATED) {
          propertyService.saveRentalsAndRaRIncome(
            ctx,
            nino,
            propertyRentalsIncome
          )
        }
      }
    }

  def saveRentalsAndRaRExpenses(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino
  ): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[Expenses](taxYear, incomeSourceId, nino, JourneyName.RentalsAndRaRExpenses, request) {
        (ctx, expenses) =>
          handleResponse(CREATED) {
            propertyService.saveExpenses(ctx, nino, expenses)
          }
      }
    }

  def saveRentalsAndRaRAllowances(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[RentalAllowances](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.RentalsAndRaRAllowances,
        request
      ) { (ctx, allowances) =>
        handleResponse(NO_CONTENT) {
          propertyService.savePropertyRentalAllowances(ctx, nino, allowances)
        }
      }
    }


  def savRentalsAndRaRSBA(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[SbaInfo](taxYear, incomeSourceId, nino, JourneyName.RentalsAndRaRSBA, request) {
        (ctx, sbaInfo) =>
          handleResponse(NO_CONTENT) {
            propertyService.saveSBA(ctx, nino, sbaInfo)
          }
      }
    }

  def saveRentalsAndRaRAdjustments(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[PropertyRentalAdjustments](
        taxYear,
        incomeSourceId,
        nino,
        JourneyName.RentalsAndRaRAdjustments,
        request
      ) { (ctx, adjustments) =>
        handleResponse(NO_CONTENT) {
          propertyService.savePropertyRentalAdjustments(ctx, nino, adjustments)
        }
      }
    }

  def saveRentalsAndRaREsba(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[EsbaInfo](taxYear, incomeSourceId, nino, JourneyName.RentalsAndRaRESBA, request) {
        (ctx, esbaInfo) =>
          handleResponse(NO_CONTENT) {
            propertyService.saveEsbas(ctx, nino, esbaInfo)
          }
      }
    }

}
