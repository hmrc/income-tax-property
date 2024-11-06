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

import config.AppConfig
import models.common._
import models.request.foreign.ForeignPropertiesSelectCountry
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.mocks.{MockIntegrationFrameworkConnector, MockMergeService, MockMongoJourneyAnswersRepository}
import utils.{AppConfigStub, UnitTest}

import scala.concurrent.ExecutionContext.Implicits.global

class ForeignPropertyServiceSpec
    extends UnitTest with MockIntegrationFrameworkConnector with MockMongoJourneyAnswersRepository with MockMergeService
    with HttpClientSupport with ScalaCheckPropertyChecks {

  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()

  private val underTest = new ForeignPropertyService(mergeService, mockIntegrationFrameworkConnector, repository)
  private val nino = Nino("A34324")
  private val incomeSourceId = IncomeSourceId("Rental")
  val taxYear: TaxYear = TaxYear(2024)

  "save foreign properties select country information" should {

    val mtditid = "1234567890"
    val ctx = JourneyContext(
      taxYear,
      incomeSourceId,
      Mtditid(mtditid),
      JourneyName.ForeignPropertySelectCountry
    )

    "persist the foreign selected properties supporting answers" in {

      val foreignPropertiesSelectCountry =
        ForeignPropertiesSelectCountry(List("AUS", "ESP", "USA"), "More than £1,000", "Property income allowance")

      await(
        underTest
          .saveForeignPropertiesSelectCountry(
            ctx,
            nino,
            foreignPropertiesSelectCountry
          )
          .value
      ) shouldBe Right(true)
    }
  }
}
