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

import config.AppConfig
import models.common.{JourneyContext, JourneyStatus}
import models.domain.JourneyAnswers
import org.mongodb.scala.bson._
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import org.mongodb.scala.result.{DeleteResult, UpdateResult}
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository
import utils.Logging

import java.time.{Clock, Instant}
import java.util.concurrent.TimeUnit
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MongoJourneyAnswersRepository @Inject() (mongo: MongoComponent, appConfig: AppConfig, clock: Clock)(implicit
  ec: ExecutionContext
) extends PlayMongoRepository[JourneyAnswers](
      collectionName = "journey-answers",
      mongoComponent = mongo,
      domainFormat = JourneyAnswers.formats,
      replaceIndexes = true,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("updatedAt"),
          IndexOptions()
            .expireAfter(appConfig.timeToLive, TimeUnit.DAYS)
            .name("TTL_UpdatedAt_Index")
        ),
        IndexModel(
          Indexes.ascending("mtditid", "taxYear", "incomeSourceId", "journey", "countryCode"),
          IndexOptions().name("mtditid_taxYear_incomeSourceId_journey_countryCode").unique(true)
        )
      )
    ) with Logging {

  private def filterJourney(ctx: JourneyContext) = Filters.and(
    Filters.eq("mtditid", ctx.mtditid.value),
    Filters.eq("taxYear", ctx.taxYear.endYear),
    Filters.eq("incomeSourceId", ctx.incomeSourceId.value),
    Filters.eq("journey", ctx.journey.entryName)
  )
  private def foreignFilterJourney(ctx: JourneyContext, countryCode: String) = Filters.and(
    Filters.eq("mtditid", ctx.mtditid.value),
    Filters.eq("taxYear", ctx.taxYear.endYear),
    Filters.eq("incomeSourceId", ctx.incomeSourceId.value),
    Filters.eq("journey", ctx.journey.entryName),
    Filters.eq("countryCode", countryCode)
  )

  def upsertAnswers(ctx: JourneyContext, newData: JsValue): Future[Boolean] = {
    val filter = filterJourney(ctx)
    val bson = BsonDocument(Json.stringify(newData))
    val update = createUpsert(ctx)("data", bson, JourneyStatus.InProgress)
    val options = new UpdateOptions().upsert(true)

    collection.updateOne(filter, update, options).toFuture().map(_ => true)
  }

  def foreignUpsertAnswers(ctx: JourneyContext, newData: JsValue, countryCode: String): Future[Boolean] = {
    val filter = foreignFilterJourney(ctx, countryCode)
    val bson = BsonDocument(Json.stringify(newData))
    val update = foreignCreateUpsert(ctx)("data", bson, JourneyStatus.InProgress, countryCode)
    val options = new UpdateOptions().upsert(true)

    collection.updateOne(filter, update, options).toFuture().map(_ => true)
  }

  def deleteAnswers(
    ctx: JourneyContext,
    journeyNames: Seq[String]
  ): Future[DeleteResult] = {
    val credentialsFilter: Bson = Filters
      .and(
        Filters.equal("incomeSourceId", ctx.incomeSourceId.value),
        Filters.equal("taxYear", ctx.taxYear.endYear),
        Filters.equal("mtditid", ctx.mtditid.value)
      )

    val filter: Bson = Filters.and(
      credentialsFilter,
      Filters.or(journeyNames.map(journeyName => Filters.equal("journey", journeyName)):_*)
    )

    collection.deleteMany(filter).toFuture()
  }

  def fetchAllJourneysUserTaxYear(taxYear: Int, mtditid: String): Future[Seq[JourneyAnswers]] = {
    val filter: Bson = Filters
      .and(
        Filters.equal("taxYear", taxYear),
        Filters.equal("mtditid", mtditid)
      )

    collection.find(filter).toFuture()
  }

  def fetchAllJourneys(ctx: JourneyContext): Future[Seq[JourneyAnswers]] = {
    val filter: Bson = Filters
      .and(
        Filters.equal("taxYear", ctx.taxYear.endYear),
        Filters.equal("mtditid", ctx.mtditid.value)
      )
    logger.debug(s"[fetchAllJourneys] Getting all journeys: ${collection.find(filter)} ")
    collection.find(filter).toFuture()
  }
  private[repositories] def createUpsert(
    ctx: JourneyContext
  )(fieldName: String, bsonValue: BsonValue, statusOnInsert: JourneyStatus) = {
    val now = Instant.now(clock)

    Updates.combine(
      Updates.set(fieldName, bsonValue),
      Updates.set("updatedAt", now),
      Updates.setOnInsert("mtditid", ctx.mtditid.value),
      Updates.setOnInsert("taxYear", ctx.taxYear.endYear),
      Updates.setOnInsert("incomeSourceId", ctx.incomeSourceId.value),
      Updates.setOnInsert("status", statusOnInsert.entryName),
      Updates.setOnInsert("journey", ctx.journey.entryName),
      Updates.setOnInsert("createdAt", now)
    )
  }

  private[repositories] def foreignCreateUpsert(
    ctx: JourneyContext
  )(fieldName: String, bsonValue: BsonValue, statusOnInsert: JourneyStatus, countryCode: String) = {
    val now = Instant.now(clock)

    Updates.combine(
      Updates.set(fieldName, bsonValue),
      Updates.set("updatedAt", now),
      Updates.setOnInsert("mtditid", ctx.mtditid.value),
      Updates.setOnInsert("taxYear", ctx.taxYear.endYear),
      Updates.setOnInsert("incomeSourceId", ctx.incomeSourceId.value),
      Updates.setOnInsert("status", statusOnInsert.entryName),
      Updates.setOnInsert("journey", ctx.journey.entryName),
      Updates.setOnInsert("countryCode", countryCode),
      Updates.setOnInsert("createdAt", now)
    )
  }

  private[repositories] def createUpsertStatus(status: JourneyStatus) = {
    val now = Instant.now(clock)
    Updates.combine(
      Updates.set("status", status.entryName),
      Updates.set("updatedAt", now),
      Updates.setOnInsert("createdAt", now)
    )
  }

  private[repositories] def updateStatus(ctx: JourneyContext, status: JourneyStatus): Future[UpdateResult] = {
    val filter = filterJourney(ctx)
    val update = createUpsertStatus(status)
    val options = new UpdateOptions().upsert(true)

    collection.updateOne(filter, update, options).toFuture()
  }

  private[repositories] def updateForeignStatus(
    ctx: JourneyContext,
    status: JourneyStatus,
    countryCode: String
  ): Future[UpdateResult] = {
    val filter = foreignFilterJourney(ctx, countryCode)
    val update = createUpsertStatus(status)
    val options = new UpdateOptions().upsert(true)

    collection.updateOne(filter, update, options).toFuture()
  }

  def setStatus(journeyContext: JourneyContext, status: JourneyStatus): Future[Unit] = {
    logger.info(s"Repository: journeyContext=${journeyContext.toString} persisting new status=$status")
    val result = updateStatus(journeyContext, status)
    // TODO return a more descriptive data type
    result.map(_ => ())
  }

  def setForeignStatus(journeyContext: JourneyContext, status: JourneyStatus, countryCode: String): Future[Unit] = {
    logger.info(s"Repository: journeyContext=${journeyContext.toString} persisting new foreign status=$status")
    val result = updateForeignStatus(journeyContext, status, countryCode)
    // TODO return a more descriptive data type
    result.map(_ => ())
  }
}
