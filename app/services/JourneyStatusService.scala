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

package services.journeyAnswers

import cats.implicits._
import models.ITPEnvelope
import models.ITPEnvelope.ITPEnvelope
import models.common._
import models.errors.ServiceError
import repositories.MongoJourneyAnswersRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class JourneyStatusService @Inject()(repository: MongoJourneyAnswersRepository)(implicit ec: ExecutionContext) {

  def setStatus(ctx: JourneyContext, status: JourneyStatusData): ITPEnvelope[Unit] = {
    ITPEnvelope.liftFuture(repository.setStatus(ctx, status.status).map(_ => ().asRight[ServiceError]))
  }

  def setForeignStatus(ctx: JourneyContext, status: JourneyStatusData, countryCode: String): ITPEnvelope[Unit] = {
    ITPEnvelope.liftFuture(repository.setForeignStatus(ctx, status.status, countryCode).map(_ => ().asRight[ServiceError]))
  }
}
