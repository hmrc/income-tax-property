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

import actions._
import errorhandling._
import models.common._
import models.errors._
import models.request.Income._
import models.request.esba.EsbaInfo
import models.request.esba.EsbaInfo._
import models.request.esba.EsbaInfoExtensions.EsbaExtensions
import models.request._
import models.request.sba.SbaInfo
import models.request.sba.SbaInfoExtensions.SbaExtensions
import models.responses._
import play.api.Logging
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, ControllerComponents, Result}
import services.PropertyService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.JsonSupport._

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util._

class JourneyAnswersController @Inject()(propertyService: PropertyService,
                                         auth: AuthorisedAction,
                                         cc: ControllerComponents)(implicit ec: ExecutionContext)
  extends BackendController(cc) with ErrorHandler with Logging with RequestHandler {


  def savePropertyAbout(taxYear: TaxYear, businessId: BusinessId, nino: Nino): Action[AnyContent] = auth.async { implicit request =>

    val ctx = JourneyContextWithNino(taxYear, businessId, request.user.getMtditid, nino).toJourneyContext(JourneyName.About)
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

  def saveExpenses(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[Expenses](taxYear, businessId, nino,JourneyName.RentalExpenses, request) { (_, expenses) =>
        handleResponse(CREATED) {
          for {
            r <- propertyService.createPeriodicSubmission(
              nino.value,
              incomeSourceId.value,
              taxYear.endYear,
              PropertyPeriodicSubmissionRequest.fromExpenses(expenses)
            )
          } yield r
        }
      }
    }

  def updateExpenses(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId, submissionId: SubmissionId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[Expenses](taxYear, businessId, nino, JourneyName.RentalExpenses, request) { (_, expenses) =>
        handleResponse(NO_CONTENT) {
          for {
            r <- propertyService.updatePeriodicSubmission(
              nino.value,
              incomeSourceId.value,
              taxYear.endYear,
              submissionId.value,
              PropertyPeriodicSubmissionRequest.fromExpenses(expenses)
            )
          } yield r
        }
      }
    }

  def savePropertyRentalAdjustments(taxYear: TaxYear, businessId: BusinessId, nino: Nino): Action[AnyContent] = auth.async { implicit request =>

    val journeyContextWithNino = JourneyContextWithNino(taxYear, businessId, request.user.getMtditid, nino)
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

  def saveIncome(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[SaveIncome](taxYear, businessId, nino, JourneyName.RentalIncome, request) { (ctx, incomeToSaveWithUkOtherPropertyIncome) =>
        handleResponse(CREATED) {
          propertyService.saveIncome(
            taxYear, businessId, nino, incomeSourceId, ctx, incomeToSaveWithUkOtherPropertyIncome.incomeToSave, incomeToSaveWithUkOtherPropertyIncome.ukOtherPropertyIncome
          )
        }
      }
    }

  def saveEsba(taxYear: TaxYear, businessId: BusinessId, nino: Nino): Action[AnyContent] = {
    auth.async { implicit request =>
      withJourneyContextAndEntity[EsbaInfo](taxYear, businessId, nino, JourneyName.RentalESBA, request) { (ctx, esbaInfo) =>
        handleResponse(NO_CONTENT) {
          for {
            r <- propertyService.createOrUpdateAnnualSubmission(taxYear,
              businessId,
              nino,
              PropertyAnnualSubmission.fromEsbas(
                esbaInfo.toEsba
              )
            )
            _ <- propertyService.persistAnswers(ctx, esbaInfo.toEsbaToSave).map(isPersistSuccess =>
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

  def saveSba(taxYear: TaxYear, businessId: BusinessId, nino: Nino): Action[AnyContent] = {
    auth.async { implicit request =>
      withJourneyContextAndEntity[SbaInfo](taxYear, businessId, nino, JourneyName.RentalSBA, request) { (ctx, sbaInfo) =>
        handleResponse(NO_CONTENT) {
          for {
            r <- propertyService.createOrUpdateAnnualSubmission(taxYear,
              businessId,
              nino,
              PropertyAnnualSubmission.fromSbas(
                sbaInfo.toSba
              )
            )
            _ <- propertyService.persistAnswers(ctx, sbaInfo.toSbaToSave).map(isPersistSuccess =>
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



  def updateIncome(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId, submissionId: SubmissionId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[SaveIncome](taxYear, businessId, nino, JourneyName.RentalIncome, request) { (ctx, incomeToSaveWithUkOtherPropertyIncome) =>

        handleResponse(NO_CONTENT) {
          (for {
            r <- propertyService.updatePeriodicSubmission(
              nino.value,
              incomeSourceId.value,
              taxYear.endYear,
              submissionId.value,
              PropertyPeriodicSubmissionRequest.fromUkOtherPropertyIncome(incomeToSaveWithUkOtherPropertyIncome.ukOtherPropertyIncome)
            )
            _ <- propertyService.persistAnswers(ctx, incomeToSaveWithUkOtherPropertyIncome.incomeToSave).map(isPersistSuccess =>
              if (!isPersistSuccess) {
                logger.error("Could not persist")
              } else {
                logger.info("Persist successful")
              }
            )
          } yield r)
        }
      }
    }

  def savePropertyRentalAllowances(taxYear: TaxYear, businessId: BusinessId, nino: Nino): Action[AnyContent] = auth.async { implicit request =>

    val ctx = JourneyContextWithNino(taxYear, businessId, request.user.getMtditid, nino)
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
}