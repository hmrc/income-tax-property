/*
 * Copyright 2024 HM Revenue & Customs
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

import actions.AuthorisationRequest
import models.common._
import models.errors.{CannotParseJsonError, CannotReadJsonError, ServiceError}
import play.api.Logging
import play.api.http.Status.BAD_REQUEST
import play.api.libs.json._
import play.api.mvc.Results.BadRequest
import play.api.mvc.{AnyContent, Result}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait RequestHandler {
  self: Logging =>
  def withJourneyContextAndEntity[T](
                                      taxYear: TaxYear,
                                      incomeSourceId: IncomeSourceId,
                                      nino: Nino,
                                      journeyName: JourneyName,
                                      authorisationRequest: AuthorisationRequest[AnyContent]
                                    )(block: (JourneyContext, T) => Future[Result])(implicit reads: Reads[T]): Future[Result] = {
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(authorisationRequest.user.mtditid), nino).toJourneyContext(journeyName)
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

  protected def toBadRequest(error: ServiceError): Result = {
    logger.error(s"Bad Request: ${error.message}")
    BadRequest(Json.obj("code" -> BAD_REQUEST, "reason" -> error.message))
  }

  protected def parseBody[A: Reads](request: AuthorisationRequest[AnyContent]): Try[Option[JsResult[A]]] = {
    Try(request.body.asJson.map(_.validate[A]))
  }

}
