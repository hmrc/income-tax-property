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

import cats.implicits.catsSyntaxEitherId
import com.mongodb.client.result.DeleteResult
import models.UKPropertySelect.PropertyRentals
import models.common._
import models.errors.{RepositoryError, ServiceError}
import models.request.PropertyAbout
import org.mockito.ArgumentMatchers.{any, eq => meq}
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar.mock
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import repositories.MongoJourneyAnswersRepository
import utils.UnitTest

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MongoJourneyAnswersServiceSpec extends UnitTest with ScalaCheckPropertyChecks {

  val mockRepository: MongoJourneyAnswersRepository = mock[MongoJourneyAnswersRepository]
  val underTest = new MongoJourneyAnswersService(mockRepository)
  val taxYear: TaxYear = TaxYear(2024)
  val incomeSourceId: IncomeSourceId = IncomeSourceId("some-income-source-id")
  val mtditid: Mtditid = Mtditid("some-mtditid")
  val journey: JourneyName = JourneyName.About
  val ctx: JourneyContext = JourneyContext(taxYear, incomeSourceId, mtditid, journey)
  val journeyNames: Seq[String] = JourneyName.ukPropertyJourneyNames.map(_.toString)

  ".persistForeignAnswers" should {
    val countryCode = "ESP"
    val journeyAnswers = PropertyAbout("foo", Some(Seq(PropertyRentals)), reportPropertyIncome = None)
    "return true if persistence is successful" in {
      when(mockRepository.foreignUpsertAnswers(any(), any(), meq(countryCode))).thenReturn(Future.successful(true))
      await(underTest.persistForeignAnswers(ctx, journeyAnswers, countryCode).value) shouldBe true.asRight[ServiceError]
    }
    "result in an error if persistence is unsuccessful" in {
      when(mockRepository.foreignUpsertAnswers(any(), any(), meq(countryCode))).thenReturn(Future.successful(false))
      await(underTest.persistForeignAnswers(ctx, journeyAnswers, countryCode).value) shouldBe RepositoryError.asLeft[Boolean]
    }
  }

  ".deleteAnswers" should {
    "return true if deletion is successful" in {
      when(mockRepository.deleteAnswers(any(), any())).thenReturn(Future.successful(DeleteResult.acknowledged(1)))
      await(underTest.deleteAnswers(ctx, journeyNames).value) shouldBe true.asRight[ServiceError]
    }
    "result in an error if deletion is unsuccessful" in {
      when(mockRepository.deleteAnswers(any(), any())).thenReturn(Future.successful(DeleteResult.unacknowledged()))
      await(underTest.deleteAnswers(ctx, journeyNames).value) shouldBe RepositoryError.asLeft[Boolean]
    }
  }

}
