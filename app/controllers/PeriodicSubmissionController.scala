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
import models.common.{BusinessId, JourneyName, Nino, TaxYear}
import models.errors.{ApiServiceError, DataNotFoundError}
import models.responses.PropertyPeriodicSubmissionRequest
import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import services.PropertyService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton()
class PeriodicSubmissionController @Inject()(propertyServices: PropertyService,
                                             authorisedAction: AuthorisedAction,
                                             cc: ControllerComponents)(implicit ec: ExecutionContext)
  extends BackendController(cc) with RequestHandler with Logging {

  def getAllPeriodicSubmissions(taxYear: Int, nino: String, incomeSourceId: String): Action[AnyContent] =
    authorisedAction.async { implicit request =>
      propertyServices.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value.map {
        case Right(periodicSubmissionData) => Ok(Json.toJson(periodicSubmissionData))
        case Left(DataNotFoundError) => NotFound
        case Left(_) => InternalServerError
      }
    }

  def createPeriodicSubmission(nino: String, incomeSourceId: String, taxYear: Int): Action[AnyContent] =
    authorisedAction.async { implicit request =>
      withJourneyContextAndEntity[PropertyPeriodicSubmissionRequest](TaxYear(taxYear), BusinessId(""), Nino(nino), JourneyName.About, request) { (_, propertyPeriodicSubmissionRequest) =>
        propertyServices.createPeriodicSubmission(nino, incomeSourceId, taxYear, propertyPeriodicSubmissionRequest).value.map {
          case Right(periodicSubmissionData) => Created(Json.toJson(periodicSubmissionData))
          case Left(ApiServiceError(BAD_REQUEST)) => BadRequest
          case Left(ApiServiceError(CONFLICT)) => Conflict
          case Left(_) => InternalServerError
        }
      }
    }

  def updatePeriodicSubmission(nino: String, incomeSourceId: String, taxYear: Int, submissionId: String): Action[AnyContent] =
    authorisedAction.async { implicit request =>
      withJourneyContextAndEntity[PropertyPeriodicSubmissionRequest](TaxYear(taxYear), BusinessId(""), Nino(nino), JourneyName.About, request) { (_, propertyPeriodicSubmissionRequest) =>
        propertyServices.updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, propertyPeriodicSubmissionRequest).value.map {
          case Right(_) => NoContent
          case Left(ApiServiceError(BAD_REQUEST)) => BadRequest
          case Left(ApiServiceError(UNPROCESSABLE_ENTITY)) => UnprocessableEntity
          case Left(_) => InternalServerError
        }
      }

    }
}
