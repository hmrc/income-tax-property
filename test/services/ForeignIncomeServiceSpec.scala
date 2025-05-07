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
import models.common._
import models.request.foreignIncome.{ForeignIncomeSubmission, ForeignIncomeDividends, ForeignIncome, ForeignIncomeDividendsWithCountryCode}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.mocks.{MockMergeService, MockMongoJourneyAnswersRepository, MockIntegrationFrameworkConnector}
import utils.{AppConfigStub, UnitTest}

import scala.concurrent.ExecutionContext.Implicits.global

class ForeignIncomeServiceSpec
  extends UnitTest with MockIntegrationFrameworkConnector with MockMongoJourneyAnswersRepository with MockMergeService
    with HttpClientSupport with ScalaCheckPropertyChecks {

  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  private val nino = Nino("A34324")
  private val incomeSourceId = IncomeSourceId("ForeignIncome")
  private val taxYear: TaxYear = TaxYear(2024)
  private val foreignIncomeDividends = ForeignIncomeDividends(
    amountBeforeTax = Some(231.45),
    taxTakenOff = Some(321.54),
    specialWithholdingTax = Some(490.58),
    foreignTaxCreditRelief = true,
    taxableAmount = 80.80)

  val foreignIncome: Option[Seq[ForeignIncome]] = Some(
    Seq(
      ForeignIncome(
        "AUS",
        Some(foreignIncomeDividends)
      )
    )
  )

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()
  private val underTest = new ForeignIncomeService(mockIntegrationFrameworkConnector, repository)

  "save foreign income dividends" should {
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(
      taxYear = taxYear,
      incomeSourceId = incomeSourceId,
      mtditid = Mtditid(mtditid),
      nino = nino
    )

    val foreignIncomeDividendsWithCountryCode = ForeignIncomeDividendsWithCountryCode(
      countryCode = "AUS",
      amountBeforeTax = Some(231.45),
      taxTakenOff = Some(321.54),
      specialWithholdingTax = Some(490.58),
      foreignTaxCreditRelief = true,
      taxableAmount = 80.80
    )

    "persist the foreign dividends supporting answers into the backend mongo" when {
      val ctx = JourneyContext(
        taxYear,
        incomeSourceId,
        Mtditid(mtditid),
        JourneyName.ForeignIncomeDividends
      )
      val submissionId = "test-dividends-submission-id"
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)
      val foreignDividendsSubmission: ForeignIncomeSubmission = ForeignIncomeSubmission(
        foreignIncome
      )

      "return no content for valid request" in {

        val requestForCreateDividends =
          ForeignIncomeSubmission.fromForeignIncomeDividends(
            foreignIncomeDividendsWithCountryCode
          )

        mockCreateForeignIncomeSubmissionDividends(
          taxYear,
          nino,
          Right(requestForCreateDividends)
        )

        await(
          underTest
            .saveForeignIncomeDividends(
              ctx,
              nino,
              foreignIncomeDividendsWithCountryCode
            )
            .value
        ) shouldBe Right(true)
      }
    }

  }

}
