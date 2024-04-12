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

import actions.{AuthorisationRequest, AuthorisedAction}
import models.common._
import models.errors.{ApiServiceError, CannotParseJsonError, CannotReadJsonError, ServiceError}
import models.request.Income._
import models.request.{PropertyAbout, Expenses, SaveIncome}
import models.responses.PropertyPeriodicSubmission
import models.request.RentalAllowances
import models.request.esba.EsbaInfo
import models.request.esba.EsbaInfo._
import models.request.esba.EsbaInfoExtensions.EsbaExtensions
import models.request.{PropertyAbout, SaveIncome}
import models.responses._
import models.request._
import models.responses.{PropertyPeriodicSubmission, UkOtherPropertyIncome}
import play.api.Logging
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, ControllerComponents, Result}
import services.PropertyService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class JourneyAnswersController @Inject()(propertyService: PropertyService,
                                         auth: AuthorisedAction,
                                         cc: ControllerComponents)(implicit ec: ExecutionContext) extends BackendController(cc) with Logging {


  def savePropertyAbout(taxYear: TaxYear, businessId: BusinessId, nino: Nino): Action[AnyContent] = auth.async { implicit request =>

    val ctx = JourneyContextWithNino(taxYear, businessId, request.user.getMtditid, nino).toJourneyContext(JourneyName.About)
    val requestBody = parseBody[PropertyAbout](request)

    requestBody match {
      case Success(validatedRes) =>
        validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
          case JsSuccess(value, _) =>
            propertyService.persistAnswers(ctx, value).map(_ => NoContent)
          case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
        }
      case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
    }
  }

  def saveExpenses(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[Expenses](taxYear, businessId, nino, request) { (_, expenses) =>
        for {
          r <- propertyService.createPeriodicSubmission(
            nino.value,
            incomeSourceId.value,
            taxYear.endYear,
            Some(
              Json.toJson(
                PropertyPeriodicSubmission.fromUkOtherPropertyExpenses(expenses)
              )
            )
          )
        } yield r match {
          case Right(periodicSubmissionData) => Created(Json.toJson(periodicSubmissionData))
          case Left(ApiServiceError(BAD_REQUEST)) => BadRequest
          case Left(ApiServiceError(CONFLICT)) => Conflict
          case Left(_) => InternalServerError
        }
      }
    }

  def updateExpenses(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId, submissionId: SubmissionId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[Expenses](taxYear, businessId, nino, request) { (_, expenses) =>
        for {
          r <- propertyService.updatePeriodicSubmission(
            nino.value,
            incomeSourceId.value,
            taxYear.endYear,
            submissionId.value,
            Some(
              Json.toJson(
                PropertyPeriodicSubmission.fromUkOtherPropertyExpenses(expenses)
              ).as[JsObject] - "fromDate" - "toDate"
            )
          )
        } yield r match {
          case Right(_) => NoContent
          case Left(ApiServiceError(BAD_REQUEST)) => BadRequest
          case Left(ApiServiceError(CONFLICT)) => Conflict
          case Left(_) => InternalServerError
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
            propertyService.savePropertyRentalAdjustments(journeyContextWithNino, value).map(_ => NoContent)
          case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
        }
      case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
    }
  }

  def saveIncome(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[SaveIncome](taxYear, businessId, nino, request) { (ctx, incomeToSaveWithUkOtherPropertyIncome) =>
        for {
          r <- propertyService.createPeriodicSubmission(
            nino.value,
            incomeSourceId.value,
            taxYear.endYear,
            Some(
              Json.toJson(
                PropertyPeriodicSubmission.fromUkOtherPropertyIncome(incomeToSaveWithUkOtherPropertyIncome.ukOtherPropertyIncome)
              )
            )
          )
          _ <- propertyService.persistAnswers(ctx, incomeToSaveWithUkOtherPropertyIncome.incomeToSave).map(isPersistSuccess =>
            if (!isPersistSuccess) {
              logger.error("Could not persist")
            } else {
              logger.info("Persist successful")
            }
          )
        } yield r match {
          case Right(periodicSubmissionData) => Created(Json.toJson(periodicSubmissionData))
          case Left(ApiServiceError(BAD_REQUEST)) => BadRequest
          case Left(ApiServiceError(CONFLICT)) => Conflict
          case Left(_) => InternalServerError
        }
      }
    }

  def updateEsba(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId): Action[AnyContent] = {
    auth.async { implicit request =>
      withJourneyContextAndEntity[EsbaInfo](taxYear, businessId, nino, request) { (ctx, esbaInfo) =>
        for {
          r <- propertyService.createOrUpdateAnnualSubmission(nino.value,
            incomeSourceId.value,
            taxYear.endYear,
            Some(
              Json.toJson(
                PropertyAnnualSubmission.fromEsbas(
                  esbaInfo.toEsba
                )
              )
            )
          )
          _ <- propertyService.persistAnswers(ctx, esbaInfo.toEsbaToSave).map(isPersistSuccess =>
            if (!isPersistSuccess) {
              logger.error("Could not persist")
            } else {
              logger.info("Persist successful")
            }
          )
        } yield r match {
          case Right(_) => NoContent
          case Left(ApiServiceError(BAD_REQUEST)) => BadRequest
          case Left(ApiServiceError(CONFLICT)) => Conflict
          case Left(_) => InternalServerError
        }
      }
    }
  }

  def updateIncome(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId, submissionId: SubmissionId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContextAndEntity[SaveIncome](taxYear, businessId, nino, request) { (ctx, incomeToSaveWithUkOtherPropertyIncome) =>
        for {
          r <- propertyService.updatePeriodicSubmission(
            nino.value,
            incomeSourceId.value,
            taxYear.endYear,
            submissionId.value,
            Some(
              Json.toJson(
                PropertyPeriodicSubmission.fromUkOtherPropertyIncome(incomeToSaveWithUkOtherPropertyIncome.ukOtherPropertyIncome)
              ).as[JsObject] - "fromDate" - "toDate"
            )
          )
          _ <- propertyService.persistAnswers(ctx, incomeToSaveWithUkOtherPropertyIncome.incomeToSave).map(isPersistSuccess =>
            if (!isPersistSuccess) {
              logger.error("Could not persist")
            } else {
              logger.info("Persist successful")
            }
          )
        } yield r match {
          case Right(_) => NoContent
          case Left(ApiServiceError(BAD_REQUEST)) => BadRequest
          case Left(ApiServiceError(CONFLICT)) => Conflict
          case Left(_) => InternalServerError
        }
      }
    }

  private def withJourneyContextAndEntity[T](
                                      taxYear: TaxYear,
                                      businessId: BusinessId,
                                      nino: Nino,
                                      authorisationRequest: AuthorisationRequest[AnyContent]
                                    )(block: (JourneyContext, T) => Future[Result])(implicit reads: Reads[T]): Future[Result] = {
    val ctx = JourneyContextWithNino(taxYear, businessId, Mtditid(authorisationRequest.user.mtditid), nino).toJourneyContext(JourneyName.About)
    val requestBody = parseBody[T](authorisationRequest)
    requestBody match {
      case Success(validatedRes) =>
        validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
          case JsSuccess(value, _) =>
            block(ctx, value)
          case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
        }
      case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
    }
  }

  private def parseBody[A: Reads](request: AuthorisationRequest[AnyContent]): Try[Option[JsResult[A]]] = {
    Try(request.body.asJson.map(_.validate[A]))
  }

  private def toBadRequest(error: ServiceError): Result = {
    logger.error(s"Bad Request: ${error.message}")
    BadRequest(Json.obj("code" -> BAD_REQUEST, "reason" -> error.message))
  }

  def savePropertyRentalAllowances(taxYear: TaxYear, businessId: BusinessId, nino: Nino): Action[AnyContent] = auth.async { implicit request =>

    val ctx = JourneyContextWithNino(taxYear, businessId, request.user.getMtditid, nino)
    val requestBody = parseBody[RentalAllowances](request)

    requestBody match {
      case Success(validatedRes) =>
        validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
          case JsSuccess(value, _) =>
            propertyService.savePropertyRentalAllowances(ctx, value).map(_ => NoContent)
          case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
        }
      case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
    }

  }
}