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
import models.request.{PropertyRentalAdjustment, Income, PropertyAbout, SaveIncome}
import models.responses.{PropertyPeriodicSubmission, UkOtherPropertyIncome}
import models.errors.{CannotParseJsonError, CannotReadJsonError, ServiceError}
import models.request.{PropertyAbout, RentalAllowances}
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

  def savePropertyRentalsAdjustment(taxYear: TaxYear, businessId: BusinessId, nino: Nino): Action[AnyContent] = auth.async { implicit request =>

    val journeyContextWithNino = JourneyContextWithNino(taxYear, businessId, request.user.getMtditid, nino)
    val annualPropertySubmissionBody = parseBody[PropertyRentalAdjustment](request)

    annualPropertySubmissionBody match {
      case Success(validatedRes) =>
        validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
          case JsSuccess(value, _) =>
            propertyService.savePropertyRentalsAdjustment(journeyContextWithNino, value).map(_ => NoContent)
          case JsError(err) => Future.successful(toBadRequest(CannotReadJsonError(err.toList)))
        }
      case Failure(err) => Future.successful(toBadRequest(CannotParseJsonError(err)))
    }
  }

  def saveIncome(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContext(taxYear, businessId, nino, request) { (ctx, incomeToSave, ukOtherPropertyIncome) =>
        for {
          r <- propertyService.createPeriodicSubmission(
            nino.value,
            incomeSourceId.value,
            taxYear.endYear,
            Some(
              Json.toJson(
                PropertyPeriodicSubmission.fromUkOtherPropertyIncome(ukOtherPropertyIncome)
              )
            )
          )
          _ <- propertyService.persistAnswers(ctx, incomeToSave).map(isPersistSuccess =>
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

  def updateIncome(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId, submissionId: SubmissionId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContext(taxYear, businessId, nino, request) { (ctx, incomeToSave, ukOtherPropertyIncome) =>
        for {
          r <- propertyService.updatePeriodicSubmission(
            nino.value,
            incomeSourceId.value,
            taxYear.endYear,
            submissionId.value,
            Some(
              Json.toJson(
                PropertyPeriodicSubmission.fromUkOtherPropertyIncome(ukOtherPropertyIncome)
              ).as[JsObject] - "fromDate" - "toDate"
            )
          )
          _ <- propertyService.persistAnswers(ctx, incomeToSave).map(isPersistSuccess =>
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

  def withJourneyContext(
                          taxYear: TaxYear,
                          businessId: BusinessId,
                          nino: Nino,
                          authorisationRequest: AuthorisationRequest[AnyContent]
                        )(block: (JourneyContext, Income, UkOtherPropertyIncome) => Future[Result]): Future[Result] = {
    val ctx = JourneyContextWithNino(taxYear, businessId, Mtditid(authorisationRequest.user.mtditid), nino).toJourneyContext(JourneyName.About)
    val requestBody = parseBody[SaveIncome](authorisationRequest)
    requestBody match {
      case Success(validatedRes) =>
        validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
          case JsSuccess(value, _) =>
            block(ctx, value.incomeToSave, value.ukOtherPropertyIncome)
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
