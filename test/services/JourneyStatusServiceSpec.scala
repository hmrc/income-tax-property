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

import models.common.{IncomeSourceId, JourneyContext, JourneyName, JourneyStatus, Mtditid, Nino, SubmissionId, TaxYear}
import models.domain.{ApiResultT, JourneyAnswers}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.mongo.test.{DefaultPlayMongoRepositorySupport, MongoSupport}
import utils.UnitTest

import java.time.temporal.ChronoUnit
import java.time.{Clock, Instant, ZoneId}
import scala.concurrent.ExecutionContext.Implicits.global

class JourneyStatusServiceSpec extends UnitTest
  /*extends UnitTest
  with MockIntegrationFrameworkConnector
  with MockMongoJourneyAnswersRepository
  with MockPropertyService
  with HttpClientSupport*/
  {

  private val instant = Instant.now.truncatedTo(ChronoUnit.MILLIS)
  private val stubClock: Clock = Clock.fixed(instant, ZoneId.systemDefault)

  //val repository = new MongoJourneyAnswersRepository(mongoComponent, stubClock)

  val taxYear: TaxYear = TaxYear(2024)
  val incomeSourceId: IncomeSourceId = IncomeSourceId("incomeSourceId")
  val incomeSubmissionId: SubmissionId = SubmissionId("submissionId")
  val nino: Nino = Nino("nino")
  val mtditid: Mtditid = Mtditid("1234567890")

  "JourneyStatusService" should {
    val ctx = JourneyContext(taxYear = taxYear, incomeSourceId = incomeSourceId, mtditid = mtditid, journey = JourneyName.RentARoom)
    //val result: ApiResultT[Unit] = repository.setStatus(ctx, JourneyStatus.InProgress)

    ".setStatus" in {
       true shouldBe true
    }
  }
}