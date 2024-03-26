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

import com.typesafe.config.ConfigFactory
import models.common.JourneyContext
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import play.api.Configuration
import play.api.libs.json.JsValue
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.mongo.test.CleanMongoCollectionSupport

import java.time.Clock
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait MockMongoJourneyAnswersRepository extends MockFactory with CleanMongoCollectionSupport {

  protected val mockMongoJourneyAnswersRepository: MongoJourneyAnswersRepository = new MongoJourneyAnswersRepository(
    mongoComponent,
    Clock.systemUTC()
  )

//  def mockSaveAnswersForCreatePeriodicSubmission(): CallHandler2[JourneyContext, JsValue, Future[Boolean]] = {
//    (mockMongoJourneyAnswersRepository.upsertAnswers(_: JourneyContext, _: JsValue))
//      .expects(*, *)
//      .returning(Future.successful(true))
//  }
}
