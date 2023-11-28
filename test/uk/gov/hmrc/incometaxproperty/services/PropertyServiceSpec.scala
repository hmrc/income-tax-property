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

package uk.gov.hmrc.incometaxproperty.services


import play.api.http.Status.INTERNAL_SERVER_ERROR
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import uk.gov.hmrc.incometaxproperty.config.AppConfig
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiError, ApiServiceError, DataNotFoundError, SingleErrorBody}
import uk.gov.hmrc.incometaxproperty.models.responses.{PeriodicSubmissionIdModel, PeriodicSubmissionModel}
import uk.gov.hmrc.incometaxproperty.models.{PeriodicSubmission, PeriodicSubmissionResponse}
import uk.gov.hmrc.incometaxproperty.utils.mocks.MockIntegrationFrameworkConnector
import uk.gov.hmrc.incometaxproperty.utils.{AppConfigStub, UnitTest}

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class PropertyServiceSpec extends UnitTest
  with MockIntegrationFrameworkConnector
  with HttpClientSupport {
  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()

  private val underTest = new PropertyService(mockIntegrationFrameworkConnector)

  ".GetPeriodicSubmission" should {

    "return error when GetPeriodicSubmission fails" in {
      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("code", "error"))))
      await(underTest.getAllPeriodicSubmission(2024, "A34324", "Rental")) shouldBe Left(ApiServiceError("500"))
    }

    "return Data Not Found error when there is no data" in {

      val aPeriodicSubmissionModel = PeriodicSubmissionModel(List.empty[PeriodicSubmissionIdModel])

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getAllPeriodicSubmission(2024, "A34324", "Rental")) shouldBe Left(DataNotFoundError)
    }

    "return data when GetPeriodicSubmission has valid values" in {
      val aPeriodicSubmissionModel = PeriodicSubmissionModel {
        List(
          PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11")),
          PeriodicSubmissionIdModel("2", LocalDate.parse("2022-02-02"), LocalDate.parse("2022-12-12"))
        )
      }

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      val expectedPeriodicSubmissionResponse = PeriodicSubmissionResponse {
        List(
          PeriodicSubmission("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11")),
          PeriodicSubmission("2", LocalDate.parse("2022-02-02"), LocalDate.parse("2022-12-12")))
      }
      await(underTest.getAllPeriodicSubmission(2024, "A34324", "Rental")) shouldBe Right(expectedPeriodicSubmissionResponse)
    }
  }
}