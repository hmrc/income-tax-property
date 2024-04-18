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
import models.common.{BusinessId, Nino, TaxYear}
import models.errors.{ApiServiceError, DataNotFoundError}
import models.responses.PropertyAnnualSubmission
import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import services.PropertyService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton()
class AnnualSubmissionController @Inject()(propertyService: PropertyService,
                                           authorisedAction: AuthorisedAction,
                                           cc: ControllerComponents)(implicit ec: ExecutionContext)
  extends BackendController(cc) with RequestHandler with Logging {

  def getAnnualSubmission(taxYear: Int, nino: String, incomeSourceId: String): Action[AnyContent] =
    authorisedAction.async { implicit request =>
      propertyService.getPropertyAnnualSubmission(taxYear, nino, incomeSourceId).value.map {
        case Right(annualSubmissionData) => Ok(Json.toJson(annualSubmissionData))
        case Left(DataNotFoundError) => NotFound
        case Left(_) => InternalServerError
      }
    }

  def deleteAnnualSubmission(incomeSourceId: String, taxableEntityId: String, taxYear: Int): Action[AnyContent] =
    authorisedAction.async { implicit request =>
      propertyService.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear).value.map {
        case Right(_) => NoContent
        case Left(ApiServiceError(BAD_REQUEST)) => BadRequest
        case Left(ApiServiceError(UNPROCESSABLE_ENTITY)) => UnprocessableEntity
        case Left(_) => InternalServerError
      }
    }

  def createOrUpdateAnnualSubmission(nino: String, incomeSourceId: String, taxYear: Int): Action[AnyContent] =
    authorisedAction.async { implicit request =>
      withJourneyContextAndEntity[PropertyAnnualSubmission](TaxYear(taxYear), BusinessId(incomeSourceId), Nino(nino), request) { (_, propertyAnnualSubmission) =>
        propertyService.createOrUpdateAnnualSubmission(TaxYear(taxYear), BusinessId(incomeSourceId), Nino(nino), propertyAnnualSubmission).value.map {
          case Right(_) => NoContent
          case Left(ApiServiceError(BAD_REQUEST)) => BadRequest
          case Left(ApiServiceError(UNPROCESSABLE_ENTITY)) => UnprocessableEntity
          case Left(_) => InternalServerError
        }
      }
    }
}
