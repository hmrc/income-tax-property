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

package uk.gov.hmrc.incometaxproperty.controllers

import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.incometaxproperty.actions.AuthorisedAction
import uk.gov.hmrc.incometaxproperty.models.errors.DataNotFoundError
import uk.gov.hmrc.incometaxproperty.services.IntegrationFrameworkService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class BusinessDetailsController @Inject()(integrationFrameworkService: IntegrationFrameworkService,
                                          authorisedAction: AuthorisedAction,
                                          cc: ControllerComponents)
                                         (implicit ec: ExecutionContext) extends BackendController(cc) with Logging {

  def getBusinessDetails(nino: String): Action[AnyContent] = authorisedAction.async { implicit request =>
    integrationFrameworkService.getBusinessDetails(nino).map {
      case Right(businessDetails) => Ok(Json.toJson(businessDetails))
      case Left(DataNotFoundError) => NotFound
      case Left(_) => InternalServerError
    }
  }
}