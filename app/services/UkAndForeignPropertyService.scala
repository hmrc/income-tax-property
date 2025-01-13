/*
 * Copyright 2025 HM Revenue & Customs
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

package services

import cats.data.EitherT
import cats.implicits.catsSyntaxEitherId
import models.common.JourneyContext
import models.errors.{RepositoryError, ServiceError}
import models.request.ukAndForeign.UkAndForeignAbout
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import repositories.MongoJourneyAnswersRepository

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class UkAndForeignPropertyService @Inject() (
    repository: MongoJourneyAnswersRepository
  )(implicit ec: ExecutionContext)
  extends Logging {

  private def ukAndForeignPersistAnswers[A](ctx: JourneyContext, answers: A)(implicit
                                                                             writes: Writes[A]
  ): EitherT[Future, ServiceError, Boolean] =
    EitherT(
      repository.upsertAnswers(ctx, Json.toJson(answers)).map {
        case false => RepositoryError.asLeft[Boolean]
        case true  => true.asRight[ServiceError]
      }
    )

  def saveUkAndForeignPropertyAbout(
                                        ctx: JourneyContext,
                                        ukAndForeignAbout: UkAndForeignAbout
                                      ): EitherT[Future, ServiceError, Boolean] =
    ukAndForeignPersistAnswers(
      ctx,
      ukAndForeignAbout
    )
}