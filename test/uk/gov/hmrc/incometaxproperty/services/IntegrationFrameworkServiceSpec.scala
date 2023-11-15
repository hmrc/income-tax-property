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
import uk.gov.hmrc.incometaxproperty.models.{PropertyData, BusinessDetailsResponse}
import uk.gov.hmrc.incometaxproperty.utils.{AppConfigStub, UnitTest}
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiError, ApiServiceError, ServiceError, SingleErrorBody}
import uk.gov.hmrc.incometaxproperty.models.responses.{IncomeSourceDetailsModel, PropertyDetailsModel}
import uk.gov.hmrc.incometaxproperty.models.responses.IncomeSourceDetailsModel.TaxPayerDisplayResponse
import uk.gov.hmrc.incometaxproperty.utils.mocks.MockIntegrationFrameworkConnector

import scala.concurrent.ExecutionContext.Implicits.global
import java.time.{LocalDate, LocalDateTime}

class IntegrationFrameworkServiceSpec extends UnitTest
  with MockIntegrationFrameworkConnector
  with HttpClientSupport
{
  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()

  private val underTest = new IntegrationFrameworkService(mockIntegrationFrameworkConnector)

  ".GetBusinessDetails" should {
    "return error when GetBusinessDetails fails" in {
      mockGetBusinessDetails( "some-nino", Left(ApiServiceError("error")))
      await(underTest.getBusinessDetails("some-nino")) shouldBe Left(ApiServiceError("error"))
    }

    "return uk property details when user only has uk property" in {
      val tradingStartDate = LocalDate.parse("2020-12-10")
      val propertyDetailsModel = PropertyDetailsModel(
        Some("uk-property"), "", LocalDate.now(), LocalDate.now(),
        Some(tradingStartDate), Some(true),
        None, None, None, None, None, None, None, None, None, None, None)

      val  incomeSourceModel = IncomeSourceDetailsModel(
        LocalDateTime.now(),
        TaxPayerDisplayResponse("safeID", "Nino", "mtdID", None, true, None, Some(Seq(propertyDetailsModel)))
      )

      mockGetBusinessDetails("some-nino", Right(incomeSourceModel))

      await(underTest.getBusinessDetails("some-nino")) shouldBe Right(
        BusinessDetailsResponse(Seq(PropertyData(Some("uk-property"),Some(tradingStartDate), Some(true)))))
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
        TaxPayerDisplayResponse("safeID", "Nino", "mtdID", None, true, None, Some(Seq(ukPropertyDetailsModel, foreignPropertyDetailsModel)))
      )

      mockGetBusinessDetails("some-nino", Right(incomeSourceModel))

      await(underTest.getBusinessDetails("some-nino")) shouldBe Right(
        BusinessDetailsResponse(
          Seq(
            PropertyData(Some("uk-property"),Some(tradingStartDate), Some(true)),
            PropertyData(Some("foreign-property"),Some(tradingStartDate), Some(false))
          )))
    }
  }
}
