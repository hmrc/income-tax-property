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
import models.repository.Extractor._
import models.request._
import models.request.esba.EsbaInfo
import models.request.esba.EsbaInfo._
import models.request.esba.EsbaInfoExtensions.EsbaExtensions
import models.request.sba.SbaInfo
import models.request.sba.SbaInfoExtensions.SbaExtensions
import models.responses._
import play.api.Logging
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, ControllerComponents, Result}
import services.PropertyService
import services.journeyAnswers.JourneyStatusService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.JsonSupport._

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class JourneyAnswersController @Inject() (
  propertyService: PropertyService,
  journeyStatusService: JourneyStatusService,
  auth: AuthorisedAction,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BackendController(cc) with ErrorHandler with Logging with RequestHandler {

  def fetchPropertyData(taxYear: TaxYear, nino: Nino, incomeSourceId: IncomeSourceId): Action[AnyContent] = auth.async {
    implicit request =>
      // Todo: Which taxyear, let's say for 23-24, do we have to send 2024 here?
      // Todo: incomeSourceId vs mtdid
      withJourneyContext(taxYear, incomeSourceId, nino, JourneyName.AllJourneys, request) { ctx =>
        handleResponse(OK) {
          propertyService.getFetchedPropertyDataMerged(ctx, nino, incomeSourceId.value)
        }
      }
  }

  def savePropertyAbout(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] = auth.async {
    implicit request =>
      val ctx = JourneyContextWithNino(taxYear, incomeSourceId, request.user.getMtditid, nino)
        .toJourneyContext(JourneyName.About)
      val requestBody = parseBody[PropertyAbout](request)

      requestBody match {
        case Success(validatedRes) =>
          validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
            case JsSuccess(value, _) =>
              propertyService.persistAnswers(ctx, value).value.map(_ => NoContent)
            case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
          }
        case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
      }
  }

  def savePropertyRentalAbout(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      val ctx = JourneyContextWithNino(taxYear, incomeSourceId, request.user.getMtditid, nino)
        .toJourneyContext(JourneyName.RentalAbout)
      val requestBody = parseBody[PropertyRentalsAbout](request)

      requestBody match {
        case Success(validatedRes) =>
          validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
            case JsSuccess(value, _) =>
              propertyService.persistAnswers(ctx, value).value.map(_ => NoContent)
            case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
          }
        case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
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

  def savePropertyRentalAdjustments(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      val journeyContextWithNino = JourneyContextWithNino(taxYear, incomeSourceId, request.user.getMtditid, nino)
      val annualPropertyRentalAdjustmentsBody = parseBody[PropertyRentalAdjustments](request)

      annualPropertyRentalAdjustmentsBody match {
        case Success(validatedRes) =>
          validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
            case JsSuccess(value, _) =>
              propertyService.savePropertyRentalAdjustments(journeyContextWithNino, value).value.map(_ => NoContent)
            case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
          }
        case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
      }
    }

  def saveIncome(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[SaveIncome](taxYear, incomeSourceId, nino, JourneyName.RentalIncome, request) {
        (ctx, incomeToSaveWithUkOtherPropertyIncome) =>
          handleResponse(CREATED) {
            propertyService.saveIncome(
              ctx,
              nino,
              incomeToSaveWithUkOtherPropertyIncome.incomeToSave,
              incomeToSaveWithUkOtherPropertyIncome
            )
          }
      }
    }

  def saveEsba(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContext(taxYear, incomeSourceId, nino, JourneyName.RentalESBA, request) { ctx =>
        withEntity[EsbaInfo](request) { esbaInfo =>
          handleResponse(NO_CONTENT) {
            for {
              r <- propertyService.createOrUpdateAnnualSubmission(
                     taxYear,
                     incomeSourceId,
                     nino,
                     PropertyAnnualSubmission.fromEsbas(
                       esbaInfo.toEsba
                     )
                   )
              _ <- propertyService
                     .persistAnswers(ctx, esbaInfo.extractToSavePart())
                     .map(isPersistSuccess =>
                       if (!isPersistSuccess) {
                         logger.error("Could not persist")
                       } else {
                         logger.info("Persist successful")
                       }
                     )
            } yield r
          }
        }
      }
    }

  def savePropertyRentalSBA(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[SbaInfo](taxYear, incomeSourceId, nino, JourneyName.RentalSBA, request) {
        (ctx, sbaInfo) =>
          handleResponse(NO_CONTENT) {
            for {
              r <- propertyService.createOrUpdateAnnualSubmission(
                     taxYear,
                     incomeSourceId,
                     nino,
                     PropertyAnnualSubmission.fromSbas(
                       sbaInfo.toSba
                     )
                   )
              _ <- propertyService
                     .persistAnswers(ctx, sbaInfo.toSbaToSave)
                     .map(isPersistSuccess =>
                       if (!isPersistSuccess) {
                         logger.error("SBA Persist failed")
                       } else {
                         logger.info("SBA Persist successful")
                       }
                     )
            } yield r
          }
      }
    }

  def savePropertyRentalAllowances(taxYear: TaxYear, incomeSourceId: IncomeSourceId, nino: Nino): Action[AnyContent] =
    auth.async { implicit request =>
      val ctx = JourneyContextWithNino(taxYear, incomeSourceId, request.user.getMtditid, nino)
      val requestBody = parseBody[RentalAllowances](request)

      requestBody match {
        case Success(validatedRes) =>
          validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
            case JsSuccess(value, _) =>
              propertyService.savePropertyRentalAllowances(ctx, value).value.map(_ => NoContent)
            case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
          }
        case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
      }
    }

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
}
