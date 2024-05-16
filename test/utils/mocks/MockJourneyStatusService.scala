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

import models.ITPEnvelope
import models.ITPEnvelope.ITPEnvelope
import models.common.{IncomeSourceId, JourneyContext, JourneyName, JourneyStatus, JourneyStatusData, Mtditid, TaxYear}
import org.scalamock.handlers.CallHandler2
import org.scalamock.scalatest.MockFactory
import play.api.http.Status.{BAD_REQUEST, NO_CONTENT}
import play.api.libs.json.Json
import play.api.mvc.Results.{BadRequest, InternalServerError}
import repositories.MongoJourneyAnswersRepository
import services.PropertyService
import services.journeyAnswers.JourneyStatusService
import uk.gov.hmrc.mongo.test.{CleanMongoCollectionSupport, MongoSupport}

import java.time.Clock
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockJourneyStatusService extends MockFactory with CleanMongoCollectionSupport{

  val repository = mock[MongoJourneyAnswersRepository]
  protected val mockJourneyStatusService: JourneyStatusService = new JourneyStatusService(repository)

  def mockRepositorySetStatus[A](ctx: JourneyContext, status: JourneyStatus):
  CallHandler2[JourneyContext, JourneyStatus, ITPEnvelope[Unit]] = {
    (repository.setStatus(_: JourneyContext, _: JourneyStatus))
      .expects(
        JourneyContext(
          taxYear = TaxYear(2023),
          incomeSourceId = IncomeSourceId("incomeSourceId"),
          mtditid = Mtditid("1234567890"),
          journey = JourneyName.RentARoom
        ),
        JourneyStatus.InProgress)
      .returning(ITPEnvelope.liftPure())
  }

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
      .returning(
        ITPEnvelope.liftPure(Future.successful(BadRequest(
          Json.obj("code" -> BAD_REQUEST, "reason" -> "Cannot read JSON: List((/status,List(JsonValidationError(List(error.path.missing),List()))))")))))
  }
}