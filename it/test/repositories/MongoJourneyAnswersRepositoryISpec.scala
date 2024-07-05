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
import models.common.JourneyName.About
import models.common.JourneyStatus.InProgress
import models.common._
import models.domain.JourneyAnswers
import org.mockito.Mockito.when
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.bson.{BsonDocument, BsonString}
import org.mongodb.scala.model.Filters
import org.scalatestplus.mockito.MockitoSugar.mock
import play.api.libs.json.Json
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import java.time.Instant.now
import java.time.temporal.ChronoUnit
import java.time.{Clock, Instant, ZoneId, ZoneOffset}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MongoJourneyAnswersRepositoryISpec extends MongoSpec with DefaultPlayMongoRepositorySupport[JourneyAnswers] {

  private val instant = Instant.now.truncatedTo(ChronoUnit.MILLIS)
  private val stubClock: Clock = Clock.fixed(instant, ZoneId.systemDefault)

  val mockAppConfig: AppConfig = mock[AppConfig]
  when(mockAppConfig.mongoTTL) thenReturn 28

  private val TTLinSeconds = mockAppConfig.mongoTTL * 3600 * 24

  override val repository = new MongoJourneyAnswersRepository(mongoComponent, mockAppConfig, stubClock)

  val taxYear: TaxYear = TaxYear(2024)
  val incomeSourceId: IncomeSourceId = IncomeSourceId("incomeSourceId")
  val nino: Nino = Nino("nino")
  val mtditid: Mtditid = Mtditid("1234567890")

  override def beforeEach(): Unit =
    await(removeAll(repository.collection))

  def removeAll(collection: MongoCollection[_]): Future[Unit] =
    collection
      .deleteMany(Filters.empty())
      .toFuture()
      .map(_ => ())

  val ctx: JourneyContext = JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino).toJourneyContext(About)

  val filters: Bson = Filters.and(
    Filters.eq("mtditid", ctx.mtditid.value),
    Filters.eq("taxYear", ctx.taxYear.endYear),
    Filters.eq("incomeSourceId", ctx.incomeSourceId.value),
    Filters.eq("journey", ctx.journey.entryName)
  )

  "createUpsert status" should {
    "" in {
      val bson = BsonDocument(Json.stringify(Json.obj("foo" -> "bar")))
      val result: BsonDocument = repository.createUpsert(ctx)("data", bson, JourneyStatus.NotStarted).toBsonDocument

      result.getDocument("$set").getDocument("data").getString("foo") shouldBe BsonString("bar")
    }
  }

  "upsertData" should {

    "insert a new journey answers in in-progress status and calculate dates" in {
      val result = repository.upsertAnswers(ctx, Json.obj("field" -> "value")).futureValue

      val updatedRecord = find(filters).futureValue.headOption.value

      result shouldBe true

      val expectedExpireAt = now.atZone(ZoneOffset.UTC).plusDays(mockAppConfig.mongoTTL).truncatedTo(ChronoUnit.DAYS).toInstant

      updatedRecord shouldBe JourneyAnswers(
        mtditid,
        incomeSourceId,
        taxYear,
        JourneyName.About,
        InProgress,
        Json.obj("field" -> "value"),
        expectedExpireAt,
        instant,
        instant
      )
    }

    "update already existing answers (values, updateAt)" in {
      val result = for {
        _       <- repository.upsertAnswers(ctx, Json.obj("field" -> "value"))
        _       <- repository.upsertAnswers(ctx, Json.obj("field" -> "updated"))
        updated <- find(filters)
      } yield updated.headOption.value

      val expectedExpireAt = now.atZone(ZoneOffset.UTC).plusDays(mockAppConfig.mongoTTL).truncatedTo(ChronoUnit.DAYS).toInstant

      result.futureValue shouldBe JourneyAnswers(
        mtditid,
        incomeSourceId,
        taxYear,
        JourneyName.About,
        InProgress,
        Json.obj("field" -> "updated"),
        expectedExpireAt,
        instant,
        instant
      )
    }
  }
}
