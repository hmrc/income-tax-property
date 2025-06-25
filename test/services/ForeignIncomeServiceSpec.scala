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
import config.AppConfig
import models.common._
import models.domain.FetchedForeignIncomeData
import models.errors.{ApiServiceError, DataNotFoundError, SingleErrorBody, ApiError}
import models.request.foreignincome.ForeignIncomeSubmission.{emptyForeignIncomeSubmission, emptyFetchedData, emptyFetchedPropertyData}
import models.request.foreignincome.{ForeignIncomeDividend, ForeignDividend, ForeignIncomeSubmission, ForeignDividendsAnswers, ForeignIncomeDividendsWithCountryCode}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.INTERNAL_SERVER_ERROR
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.mocks.{MockMergeService, MockHipConnector, MockMongoJourneyAnswersRepository, MockIntegrationFrameworkConnector}
import utils.{AppConfigStub, UnitTest}

import scala.concurrent.ExecutionContext.Implicits.global

class ForeignIncomeServiceSpec
  extends UnitTest with MockIntegrationFrameworkConnector with MockMongoJourneyAnswersRepository with MockMergeService
    with HttpClientSupport with ScalaCheckPropertyChecks with MockHipConnector{

  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  private val nino = Nino("A34324")
  private val incomeSourceId = IncomeSourceId("ForeignIncome")
  private val taxYear: TaxYear = TaxYear(2024)
  val mtditid = "1234567890"
  private val foreignDividend = ForeignDividend(
    countryCode = "AUS",
    amountBeforeTax = Some(231.45),
    taxTakenOff = Some(321.54),
    specialWithholdingTax = Some(490.58),
    foreignTaxCreditRelief = Some(true),
    taxableAmount = 80.80
  )
  private val fetchedForeignIncomeData = FetchedForeignIncomeData(Some(Map(foreignDividend.countryCode -> ForeignDividendsAnswers(
    amountBeforeTax = Some(231.45),
    taxTakenOff = Some(321.54),
    specialWithholdingTax = Some(490.58),
    foreignTaxCreditRelief = Some(true),
    taxableAmount = Some(80.80),
    foreignTaxDeductedFromDividendIncome = Some(true)))),
    List())

  override val mergeService = new MergeService
  val propertyService = new PropertyService(mergeService, mockIntegrationFrameworkConnector, journeyAnswersService, appConfigStub, mockHipConnector)


  val foreignDividends: Option[Seq[ForeignDividend]] = Some(Seq(foreignDividend))

  val foreignIncomeSubmission: ForeignIncomeSubmission = emptyForeignIncomeSubmission.copy(
    foreignDividend = foreignDividends
  )

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()
  private val underTest = new ForeignIncomeService(mockIntegrationFrameworkConnector, journeyAnswersService, mergeService, propertyService)
  val internalServerError: ApiError = ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("code", "error"))

  "getForeignIncomeSubmission" should {
    "return dividends income submissions when present" in {
      mockGetForeignIncomeSubmission(taxYear, nino, Right(Some(foreignIncomeSubmission)))
      await(underTest.getForeignIncomeSubmission(taxYear, nino).value) shouldBe
        Right(emptyForeignIncomeSubmission.copy(foreignDividend = Some(Seq(foreignDividend))))
    }

    "return DataNotFoundError when one is not present" in {
      mockGetForeignIncomeSubmission(taxYear, nino, Right(None))
      await(underTest.getForeignIncomeSubmission(taxYear, nino).value) shouldBe
        DataNotFoundError.asLeft[ForeignIncomeSubmission]
    }

    "return ApiError when IF call fails" in {
      mockGetForeignIncomeSubmission(taxYear, nino, Left(internalServerError))
      await(underTest.getForeignIncomeSubmission(taxYear, nino).value) shouldBe
        ApiServiceError(500).asLeft[ForeignIncomeSubmission]
    }
  }

  "createOrUpdateForeignIncomeSubmission" should {
    "create or update dividends income submissions" in {
      mockCreateOrUpdateForeignIncomeSubmission(taxYear, nino, foreignIncomeSubmission, Right(()))
      await(underTest.createOrUpdateForeignIncomeSubmission(taxYear, nino, foreignIncomeSubmission).value) shouldBe
        Right(true)
    }
    "return ApiError when IF call fails" in {
      mockCreateOrUpdateForeignIncomeSubmission(taxYear, nino, foreignIncomeSubmission, Left(internalServerError))
      await(underTest.createOrUpdateForeignIncomeSubmission(taxYear, nino, foreignIncomeSubmission).value) shouldBe
        ApiServiceError(500).asLeft[ForeignIncomeSubmission]
    }
  }

  "deleteForeignIncomeSubmission" should {
    "delete dividends income submissions" in {
      mockDeleteForeignIncomeSubmission(taxYear, nino, Right(()))
      await(underTest.deleteForeignIncomeSubmission(taxYear, nino).value) shouldBe
        Right(())
    }
    "return ApiError when IF call fails" in {
      mockDeleteForeignIncomeSubmission(taxYear, nino, Left(internalServerError))
      await(underTest.deleteForeignIncomeSubmission(taxYear, nino).value) shouldBe
        ApiServiceError(500).asLeft[ForeignIncomeSubmission]
    }
  }

  "saveForeignIncomeDividends" should {
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)
    val journeyContext = ctx.toJourneyContext(JourneyName.ForeignIncomeDividends)
    val foreignIncomeDividendsRequest: ForeignIncomeDividendsWithCountryCode = ForeignIncomeDividendsWithCountryCode(
      foreignIncomeDividends = Seq(
        ForeignIncomeDividend(
          countryCode = "USA",
          incomeBeforeForeignTaxDeducted = 12.34,
          foreignTaxDeductedFromDividendIncome = false,
          howMuchForeignTaxDeductedFromDividendIncome = None,
          claimForeignTaxCreditRelief = None
        )
      )
    )
    val updatedDividends = ForeignIncomeSubmission.fromForeignIncomeDividends(foreignIncomeSubmission, foreignIncomeDividendsRequest)

    "return true for successful API calls and persistence" in {
      mockGetForeignIncomeSubmission(taxYear, nino, Right(Some(foreignIncomeSubmission)))
      mockCreateOrUpdateForeignIncomeSubmission(taxYear, nino, updatedDividends, Right(()))

      await(underTest.saveForeignIncomeDividends(journeyContext, nino, foreignIncomeDividendsRequest).value) shouldBe
        Right(true)
    }

    "return ApiError when IF call fails" in {
      mockGetForeignIncomeSubmission(taxYear, nino, Left(internalServerError))
      await(underTest.saveForeignIncomeDividends(journeyContext, nino, foreignIncomeDividendsRequest).value) shouldBe
        ApiServiceError(500).asLeft[Boolean]
    }
  }

  "getFetchedIncomeDataMerged" should {
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)
    val journeyContext = ctx.toJourneyContext(JourneyName.ForeignPropertyIncome)

    "return true for successful API calls and persistence" in {
      mockGetForeignIncomeSubmission(taxYear, nino, Right(Some(foreignIncomeSubmission)))

      await(underTest.getFetchedIncomeDataMerged(journeyContext, nino).value) shouldBe
        Right(emptyFetchedData.copy(emptyFetchedPropertyData, fetchedForeignIncomeData))
    }

    "return ApiError when IF call fails" in {
      mockGetForeignIncomeSubmission(taxYear, nino, Left(internalServerError))
      await(underTest.getFetchedIncomeDataMerged(journeyContext, nino).value) shouldBe
        ApiServiceError(500).asLeft[Boolean]
    }
  }

}
