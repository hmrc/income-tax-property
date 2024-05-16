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

import models.ITPEnvelope
import models.common.{IncomeSourceId, JourneyContext, JourneyName, JourneyStatus, JourneyStatusData, Mtditid, TaxYear}
import repositories.MongoJourneyAnswersRepository
import services.journeyAnswers.JourneyStatusService
import utils.UnitTest
import utils.mocks.{MockIntegrationFrameworkConnector, MockJourneyStatusService, MockPropertyService}

import scala.concurrent.ExecutionContext.Implicits.global


class JourneyStatusServiceSpec extends UnitTest with MockJourneyStatusService {


  "JourneyStatusService" should {
    ".setStatus" in {

      mockRepositorySetStatus(JourneyContext(
        taxYear = TaxYear(2023),
        incomeSourceId = IncomeSourceId("incomeSourceId"),
        mtditid = Mtditid("1234567890"),
        journey = JourneyName.RentARoom
      ), JourneyStatus.InProgress)

      val result = await(
          mockJourneyStatusService.setStatus(JourneyContext(
          taxYear = TaxYear(2023),
          incomeSourceId = IncomeSourceId("incomeSourceId"),
          mtditid = Mtditid("1234567890"),
          journey = JourneyName.RentARoom
        ),
          JourneyStatusData(JourneyStatus.InProgress)
        ).value
      )

      result shouldBe ITPEnvelope.liftPure(())
    }
  }
}
