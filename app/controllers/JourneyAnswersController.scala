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
import models.request.{Income, PropertyAbout}
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

  def saveIncome(taxYear: TaxYear, businessId: BusinessId, nino: Nino, incomeSourceId: IncomeSourceId): Action[AnyContent] =
    auth.async { implicit request =>
      withJourneyContext(taxYear, businessId, nino, request) { (ctx, income) =>
        for {
          r <- propertyService.createPeriodicSubmission(
            nino.value,
            incomeSourceId.value,
            taxYear.endYear,
            request.body.asJson.map(_.as[JsObject] - "ukProperty" - "totalIncome")
          )
          _ <- propertyService.persistAnswers(ctx, income).map(isPersistSuccess =>
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
      withJourneyContext(taxYear, businessId, nino, request) { (ctx, income) =>
        for {
          r <- propertyService.updatePeriodicSubmission(
            nino.value,
            incomeSourceId.value,
            taxYear.endYear,
            submissionId.value,
            request.body.asJson.map(_.as[JsObject] - "ukProperty" - "totalIncome")
          )
          _ <- propertyService.persistAnswers(ctx, income).map(isPersistSuccess =>
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
                        )(block: (JourneyContext, Income) => Future[Result]): Future[Result] = {
    val ctx = JourneyContextWithNino(taxYear, businessId, Mtditid(authorisationRequest.headers.get("mtditid").get), nino).toJourneyContext(JourneyName.About)
    val requestBody = parseBody[Income](authorisationRequest)
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

  private def parseBody[A: Reads](request: AuthorisationRequest[AnyContent]) = {
    Try(request.body.asJson.map(_.validate[A]))
  }

  private def toBadRequest(error: ServiceError): Result = {
    logger.error(s"Bad Request: ${error.message}")
    BadRequest(Json.obj("code" -> BAD_REQUEST, "reason" -> error.message))
  }

}
