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

import play.api.http.Status.INTERNAL_SERVER_ERROR
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import config.AppConfig
import models.errors.{ApiError, ApiServiceError, DataNotFoundError, SingleErrorBody}
import models.responses.IncomeSourceDetailsModel.TaxPayerDisplayResponse
import models.responses.{IncomeSourceDetailsModel, PropertyDetailsModel}
import models.{BusinessDetailsResponse, PropertyDetails}
import utils.{AppConfigStub, UnitTest}
import utils.mocks.MockIntegrationFrameworkConnector

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class IntegrationFrameworkServiceSpec extends UnitTest
  with MockIntegrationFrameworkConnector
  with HttpClientSupport
{
  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()

  private val underTest = new IntegrationFrameworkService(mockIntegrationFrameworkConnector)

  ".GetBusinessDetails" should {
    "return error when GetBusinessDetails fails" in {
      mockGetBusinessDetails( "some-nino", Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("code", "error"))))
      await(underTest.getBusinessDetails("some-nino")) shouldBe Left(ApiServiceError(500))
    }

    "return error when GetBusinessDetails does not return data" in {
      val  incomeSourceModel = IncomeSourceDetailsModel(
        LocalDateTime.now(),
        TaxPayerDisplayResponse("safeID", "Nino", "mtdID", None, propertyIncome = true, None, None)
      )

      mockGetBusinessDetails("some-nino", Right(Some(incomeSourceModel)))

      await(underTest.getBusinessDetails("some-nino")) shouldBe Left(DataNotFoundError)
    }

    "return uk property details when user only has uk property" in {
      val tradingStartDate = LocalDate.parse("2020-12-10")
      val propertyDetailsModel = PropertyDetailsModel(
        Some("uk-property"), "", LocalDate.now(), LocalDate.now(),
        Some(tradingStartDate), Some(true),
        None, None, None, None, None, None, None, None, None, None, None)

      val incomeSourceModel = IncomeSourceDetailsModel(
        LocalDateTime.now(),
        TaxPayerDisplayResponse("safeID", "Nino", "mtdID", None, propertyIncome = true, None, Some(Seq(propertyDetailsModel)))
      )

      mockGetBusinessDetails("some-nino", Right(Some(incomeSourceModel)))

      await(underTest.getBusinessDetails("some-nino")) shouldBe Right(
        BusinessDetailsResponse(Seq(PropertyDetails(Some("uk-property"),Some(tradingStartDate), Some(true)))))
    }


    "return uk and foreign property details when user has both" in {
      val tradingStartDate = LocalDate.parse("2020-12-10")
      val ukPropertyDetailsModel = PropertyDetailsModel(
        Some("uk-property"), "", LocalDate.now(), LocalDate.now(),
        Some(tradingStartDate), Some(true),
        None, None, None, None, None, None, None, None, None, None, None)
      val foreignPropertyDetailsModel = PropertyDetailsModel(
        Some("foreign-property"), "", LocalDate.now(), LocalDate.now(),
        Some(tradingStartDate), Some(false),
        None, None, None, None, None, None, None, None, None, None, None)

      val  incomeSourceModel = IncomeSourceDetailsModel(
        LocalDateTime.now(),
        TaxPayerDisplayResponse("safeID", "Nino", "mtdID", None, propertyIncome = true, None, Some(Seq(ukPropertyDetailsModel, foreignPropertyDetailsModel)))
      )

      mockGetBusinessDetails("some-nino", Right(Some(incomeSourceModel)))

      await(underTest.getBusinessDetails("some-nino")) shouldBe Right(
        BusinessDetailsResponse(
          Seq(
            PropertyDetails(Some("uk-property"),Some(tradingStartDate), Some(true)),
            PropertyDetails(Some("foreign-property"),Some(tradingStartDate), Some(false))
          )))
    }
  }
}
