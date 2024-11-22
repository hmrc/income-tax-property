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

package services

import cats.data.EitherT
import cats.syntax.either._
import connectors.IntegrationFrameworkConnector
import models.common._
import models.errors._
import models.request.foreign.ForeignPropertySelectCountry
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ForeignPropertyService @Inject() (
  mergeService: MergeService,
  connector: IntegrationFrameworkConnector,
  repository: MongoJourneyAnswersRepository
)(implicit ec: ExecutionContext)
    extends Logging {

  def persistAnswers[A](ctx: JourneyContext, answers: A)(implicit
    writes: Writes[A]
  ): EitherT[Future, ServiceError, Boolean] =
    EitherT(
      repository.upsertAnswers(ctx, Json.toJson(answers)).map {
        case false => RepositoryError.asLeft[Boolean]
        case true  => true.asRight[ServiceError]
      }
    )

  def saveForeignPropertySelectCountry(
    ctx: JourneyContext,
    nino: Nino,
    foreignPropertySelectCountry: ForeignPropertySelectCountry
  )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] =
    persistAnswers(
      ctx,
      foreignPropertySelectCountry
    )

}
