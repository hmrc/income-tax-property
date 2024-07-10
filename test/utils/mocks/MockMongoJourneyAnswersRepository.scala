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

package utils.mocks

import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import repositories.MongoJourneyAnswersRepository
import services.journeyAnswers.JourneyStatusService
import uk.gov.hmrc.mongo.test.CleanMongoCollectionSupport
import utils.AppConfigStub

import java.time.temporal.ChronoUnit
import java.time.{Clock, Instant, ZoneId}
import scala.concurrent.ExecutionContext.Implicits.global

trait MockMongoJourneyAnswersRepository extends MockFactory with CleanMongoCollectionSupport with GuiceOneAppPerSuite with OptionValues {

  val mockAppConfig = new AppConfigStub().config()

  private val instant = Instant.now.truncatedTo(ChronoUnit.MILLIS)
  private val stubClock: Clock = Clock.fixed(instant, ZoneId.systemDefault)
  protected val repository = new MongoJourneyAnswersRepository(mongoComponent, mockAppConfig, stubClock)
  protected val journeyStatusService: JourneyStatusService = new JourneyStatusService(repository)
}
