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

import cats.data.EitherT
import models.ITPEnvelope
import models.ITPEnvelope.ITPEnvelope
import models.common.{IncomeSourceId, JourneyContext, JourneyName, JourneyStatus, JourneyStatusData, Mtditid, TaxYear}
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import play.api.http.Status.{BAD_REQUEST, NO_CONTENT}
import play.api.mvc.Results.InternalServerError
import repositories.MongoJourneyAnswersRepository
import services.journeyAnswers.JourneyStatusService
import uk.gov.hmrc.mongo.test.CleanMongoCollectionSupport

import java.time.Clock
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure

trait MockMongoJourneyAnswersRepository extends MockFactory with CleanMongoCollectionSupport {

  protected val mockJourneyStatusService: JourneyStatusService = mock[JourneyStatusService]

  protected val mockMongoJourneyAnswersRepository: MongoJourneyAnswersRepository = new MongoJourneyAnswersRepository(
    mongoComponent,
    Clock.systemUTC()
  )

  def mockSaveJourneyStatusNoContent[A](ctx: JourneyContext, status: JourneyStatusData):
  CallHandler2[JourneyContext, JourneyStatusData, ITPEnvelope[Unit]] = {
    (mockJourneyStatusService.setStatus(_: JourneyContext, _: JourneyStatusData))
      .expects(
        JourneyContext(
          taxYear = TaxYear(2023),
          incomeSourceId = IncomeSourceId("incomeSourceId"),
          mtditid = Mtditid("1234567890"),
          journey = JourneyName.RentARoom
        ),
          JourneyStatusData(JourneyStatus.InProgress))
      .returning(ITPEnvelope.liftPure(NO_CONTENT))
  }

  def mockSaveJourneyStatusBadRequest[A](ctx: JourneyContext, status: JourneyStatusData):
  CallHandler2[JourneyContext, JourneyStatusData, ITPEnvelope[Unit]] = {
    (mockJourneyStatusService.setStatus(_: JourneyContext, _: JourneyStatusData))
      .expects(
        JourneyContext(
          taxYear = TaxYear(2023),
          incomeSourceId = IncomeSourceId("incomeSourceId"),
          mtditid = Mtditid("1234567890"),
          journey = JourneyName.RentARoom
        ),
        JourneyStatusData(JourneyStatus.Completed))
      .returning(ITPEnvelope.liftPure(BAD_REQUEST))
  }
}