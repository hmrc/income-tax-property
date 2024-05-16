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

package repositories

import models.common.{JourneyContext, JourneyStatus}
import models.domain.JourneyAnswers
import org.mongodb.scala._
import org.mongodb.scala.bson._
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import org.mongodb.scala.result.UpdateResult
import play.api.libs.json.{JsValue, Json}
import repositories.ExpireAtCalculator.calculateExpireAt
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import utils.Logging

import java.time.{Clock, Instant}
import java.util.concurrent.TimeUnit
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MongoJourneyAnswersRepository @Inject() (mongo: MongoComponent, clock: Clock)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[JourneyAnswers](
      collectionName = "journey-answers",
      mongoComponent = mongo,
      domainFormat = JourneyAnswers.formats,
      replaceIndexes = true,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("expireAt"),
          IndexOptions()
            .name("expireAt")
            .expireAfter(0, TimeUnit.SECONDS)
        ),
        IndexModel(
          Indexes.ascending("mtditid", "taxYear", "incomeSourceId", "journey"),
          IndexOptions().name("mtditid_taxYear_incomeSourceId_journey")
        )// Todo: Uniqueness, unique index should be created
      )
    ) with Logging {

  private def filterJourney(ctx: JourneyContext) = Filters.and(
    Filters.eq("mtditid", ctx.mtditid.value),
    Filters.eq("taxYear", ctx.taxYear.endYear),
    Filters.eq("incomeSourceId", ctx.incomeSourceId.value),
    Filters.eq("journey", ctx.journey.entryName)
  )

  def upsertAnswers(ctx: JourneyContext, newData: JsValue): Future[Boolean] = {
    val filter = filterJourney(ctx)
    val bson = BsonDocument(Json.stringify(newData))
    val update = createUpsert(ctx)("data", bson, JourneyStatus.NotStarted)
    val options = new UpdateOptions().upsert(true)

    collection.updateOne(filter, update, options).toFuture().map(_ => true)
  }

  def fetch(ctx: JourneyContext): Future[Seq[JourneyAnswers]] = {
    val filter: Bson = Filters
      .and(
        Filters.equal("incomeSourceId", ctx.incomeSourceId.value),
        Filters.equal("taxYear", ctx.taxYear.endYear),
        Filters.equal("mtditid", ctx.mtditid.value),
        Filters.equal("journey", ctx.journey.entryName)
      )
    //Todo: How is this indexed?
    collection.find(filter).toFuture()
  }

  def fetchAllJourneys(ctx: JourneyContext): Future[Seq[JourneyAnswers]] = {
    val filter: Bson = Filters
      .and(
        Filters.equal("incomeSourceId", ctx.incomeSourceId.value),
        Filters.equal("taxYear", ctx.taxYear.endYear),
        Filters.equal("mtditid", ctx.mtditid.value)
      )
    //Todo: How is this indexed?
    collection.find(filter).toFuture()
  }
  private[repositories] def createUpsert(ctx: JourneyContext)(fieldName: String, value: BsonValue, statusOnInsert: JourneyStatus) = {
    val now = Instant.now(clock)
    val expireAt = calculateExpireAt(now)

    Updates.combine(
      Updates.set(fieldName, value),
      Updates.set("updatedAt", now),
      Updates.setOnInsert("mtditid", ctx.mtditid.value),
      Updates.setOnInsert("taxYear", ctx.taxYear.endYear),
      Updates.setOnInsert("incomeSourceId", ctx.incomeSourceId.value),
      Updates.setOnInsert("status", statusOnInsert.entryName),
      Updates.setOnInsert("journey", ctx.journey.entryName),
      Updates.setOnInsert("createdAt", now),
      Updates.setOnInsert("expireAt", expireAt)
    )
  }

  private[repositories] def createUpsertStatus(ctx: JourneyContext)(status: JourneyStatus) = {
    val now      = Instant.now(clock)
    val expireAt = calculateExpireAt(now)

    Updates.combine(
      Updates.set("status", status.entryName),
      Updates.set("updatedAt", now),
      Updates.setOnInsert("expireAt", expireAt)
    )
  }

  private[repositories] def updateStatus(ctx: JourneyContext, status: JourneyStatus): Future[UpdateResult] = {
    val filter  = filterJourney(ctx)
    val update  = createUpsertStatus(ctx)(status)
    val options = new UpdateOptions().upsert(true)

    collection.updateOne(filter, update, options).toFuture()
  }

  def setStatus(ctx: JourneyContext, status: JourneyStatus): Future[Unit] = {
    logger.info(s"Repository: ctx=${ctx.toString} persisting new status=$status")
    val result = updateStatus(ctx, status)
    //TODO return a more descriptive data type
    result.map(_ => ())
  }
}
