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

import config.AppConfig
import models.common.{IncomeSourceId, JourneyContext, JourneyName, Mtditid, TaxYear}
import models.errors.RepositoryError
import models.request.ukandforeign._
import org.mockito.Mockito.{doReturn, spy}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.{AppConfigStub, UnitTest}
import utils.mocks._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UkAndForeignPropertyServiceSpec
  extends UnitTest with MockIntegrationFrameworkConnector with MockMongoJourneyAnswersRepository with MockMergeService
    with HttpClientSupport with ScalaCheckPropertyChecks {

  private val ukAndForeignAbout = UkAndForeignAbout(TotalPropertyIncome.LessThan, Some(ReportIncome.DoNoWantToReport))
  private val incomeSourceId = IncomeSourceId("UkAndForeignProperty")
  private val taxYear: TaxYear = TaxYear(2025)

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()
  private val underTest = new UkAndForeignPropertyService(repository)

  implicit val defaultPatience: PatienceConfig = PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))

  val mtditid = "1234567890"
  val ctx: JourneyContext = JourneyContext(
    taxYear,
    incomeSourceId,
    Mtditid(mtditid),
    JourneyName.UkAndForeignPropertyAbout
  )
  "save foreign properties about information" should {

    "persist the uk and foreign selected properties supporting answers" in {

      await(
        underTest
          .saveUkAndForeignPropertyAbout(
            ctx,
            ukAndForeignAbout
          )
          .value
      ) shouldBe Right(true)
    }
  }

  "return an error when repository fails to persist the answers" in {

    val spyRepository = spy(repository)

    doReturn(Future.successful(false)).when(spyRepository).upsertAnswers(ctx, Json.toJson(ukAndForeignAbout))

    val result = new UkAndForeignPropertyService(spyRepository).saveUkAndForeignPropertyAbout(ctx, ukAndForeignAbout)

    whenReady(result.value, Timeout(Span(1500, Millis))) { response =>
      response shouldBe Left(RepositoryError)
    }
  }

}