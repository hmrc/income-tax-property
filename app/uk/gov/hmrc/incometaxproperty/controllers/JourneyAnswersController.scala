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

import play.api.libs.json.{JsError, JsSuccess}
import play.api.mvc.{Action, AnyContent, ControllerComponents, Result}
import uk.gov.hmrc.incometaxproperty.actions.AuthorisedAction
import uk.gov.hmrc.incometaxproperty.models.common._
import uk.gov.hmrc.incometaxproperty.models.request.PropertyAbout
import uk.gov.hmrc.incometaxproperty.services.PropertyService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class JourneyAnswersController @Inject()(propertyService: PropertyService,
                                         auth: AuthorisedAction,
                                         cc: ControllerComponents)(implicit ec: ExecutionContext) extends BackendController(cc) {


  def savePropertyAbout(taxYear: TaxYear, businessId: BusinessId, nino: Nino): Action[AnyContent] = auth.async { implicit request =>

    val mtditid = request.headers.get("mtditid").getOrElse("")
    val ctx = JourneyContextWithNino(taxYear, businessId, Mtditid(mtditid), nino).toJourneyContext(JourneyName.PropertyAbout)

    val triedOption = Try(request.body.asJson.map(_.validate[PropertyAbout]))

    triedOption match {
      case Success(validatedRes) =>
        validatedRes.fold[Future[Result]](Future.successful(BadRequest)) {
          case JsSuccess(value, _) =>
            propertyService.persistAnswers(ctx, value).map(_ => NoContent)
          case JsError(_) => Future.successful(BadRequest)
        }
      case Failure(err) => Future.successful(BadRequest)
    }

  }

}
