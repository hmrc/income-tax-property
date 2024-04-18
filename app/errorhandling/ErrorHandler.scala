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

package errorhandling

import cats.data.EitherT
import models.errors.{ApiServiceError, ServiceError}
import play.api.http.Status.{BAD_REQUEST, CONFLICT, INTERNAL_SERVER_ERROR}
import play.api.libs.json.{Json, Writes}
import play.api.mvc.{Result, Results}

import scala.concurrent.{ExecutionContext, Future}

trait ErrorHandler extends Results {

  def handleResponse[T](
                         successStatusCode: Int
                       )
                       (
                         response: EitherT[Future, ServiceError, T]
                       )(
                         implicit ec: ExecutionContext,
                         writes: Writes[T]
                       ): Future[Result] = {
    response.fold({
      case e@ApiServiceError(BAD_REQUEST) => BadRequest(Json.toJson(ErrorResponse(BAD_REQUEST, e.message)))
      case e@ApiServiceError(CONFLICT) => Conflict(Json.toJson(ErrorResponse(CONFLICT, e.message)))
      case e => InternalServerError(Json.toJson(ErrorResponse(INTERNAL_SERVER_ERROR, e.message)))
    }, r =>
      Status(successStatusCode)(Json.toJson(r))
    )
  }
}
