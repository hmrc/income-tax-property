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

import cats.implicits.catsSyntaxEitherId
import config.AppConfig
import models.PropertyPeriodicSubmissionResponse
import models.common._
import models.errors.{ApiError, ApiServiceError, SingleErrorBody}
import models.request.foreign._
import models.request.foreign.expenses.{ConsolidatedExpenses, ForeignPropertyExpenses}
import models.request.{CreatePropertyPeriodicSubmissionRequest, UpdatePropertyPeriodicSubmissionRequest}
import models.responses._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.mocks.{MockIntegrationFrameworkConnector, MockMergeService, MockMongoJourneyAnswersRepository}
import utils.{AppConfigStub, UnitTest}

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class ForeignPropertyServiceSpec
    extends UnitTest with MockIntegrationFrameworkConnector with MockMongoJourneyAnswersRepository with MockMergeService
    with HttpClientSupport with ScalaCheckPropertyChecks {

  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()

  private val underTest = new ForeignPropertyService(mergeService, mockIntegrationFrameworkConnector, repository)
  private val nino = Nino("A34324")
  private val incomeSourceId = IncomeSourceId("ForeignProperty")
  val taxYear: TaxYear = TaxYear(2024)
  val mtditid = "1234567890"
  val foreignProperty: Option[Seq[ForeignProperty]] = Some(
    Seq(
      ForeignProperty(
        "USA",
        Some(ForeignPropertyIncome(None, Some(true), None, None, Some(BigDecimal(54)), None)),
        None
      )
    )
  )

  "save foreign properties select country information" should {

    val mtditid = "1234567890"
    val ctx = JourneyContext(
      taxYear,
      incomeSourceId,
      Mtditid(mtditid),
      JourneyName.ForeignPropertySelectCountry
    )

    "persist the foreign selected properties supporting answers" in {

      val foreignPropertySelectCountry = ForeignPropertySelectCountry(
        ForeignTotalIncome.OneThousandAndMore,
        Some(true),
        Some(Array(Country("Brazil", "BRA"))),
        Some(false),
        Some(true)
      )

      await(
        underTest
          .saveForeignPropertySelectCountry(
            ctx,
            foreignPropertySelectCountry
          )
          .value
      ) shouldBe Right(true)
    }
  }

  "save foreign property tax" should {

    val foreignPropertyTaxWithCountryCode =
      ForeignPropertyTaxWithCountryCode(
        "ESP",
        Some(ForeignIncomeTax(foreignIncomeTaxYesNo = true, Some(BigDecimal(50)))),
        Some(false)
      )

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

    "return no content for valid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("", fromDate, toDate)).asRight[ApiError]
      )

      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(taxYear)),
          LocalDate.parse(TaxYear.endDate(taxYear)),
          None,
          None
        )

      val Right(requestForCreate: CreatePropertyPeriodicSubmissionRequest) =
        CreatePropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
          taxYear,
          Some(emptyPeriodicSubmission),
          foreignPropertyTaxWithCountryCode
        )
      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        Some(PeriodicSubmissionId("")).asRight[ApiError]
      )

      await(
        underTest
          .saveForeignPropertyTax(
            ctx.toJourneyContext(JourneyName.ForeignPropertyTax),
            nino,
            foreignPropertyTaxWithCountryCode
          )
          .value
      ) shouldBe Right(Some(PeriodicSubmissionId("")))
    }

    "save foreign property expenses" should {

      val mtditid = "1234567890"
      val ctx = JourneyContext(
        taxYear,
        incomeSourceId,
        Mtditid(mtditid),
        JourneyName.ForeignPropertyExpenses
      )

      "persist the foreign expenses" in {

        val foreignPropertyExpenses = ForeignPropertyExpenses(
          countryCode = "BRA",
          consolidatedExpenses = Some(ConsolidatedExpenses(consolidatedOrIndividualExpensesYesNo = false, None)),
          premisesRunningCosts = Some(50),
          repairsAndMaintenance = Some(60),
          financialCosts = Some(675),
          professionalFees = Some(85),
          costOfServices = Some(234),
          other = Some(99)
        )
        await(
          underTest
            .saveForeignPropertyExpenses(
              ctx,
              foreignPropertyExpenses
            )
            .value
        ) shouldBe Right(true)
      }
    }

    "return ApiError for invalid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)
      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("", fromDate, toDate)).asRight[ApiError]
      )
      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(taxYear)),
          LocalDate.parse(TaxYear.endDate(taxYear)),
          None,
          None
        )

      val Right(requestForCreate: CreatePropertyPeriodicSubmissionRequest) =
        CreatePropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
          taxYear,
          Some(emptyPeriodicSubmission),
          foreignPropertyTaxWithCountryCode
        )

      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        ApiError(BAD_REQUEST, SingleErrorBody("code", "error")).asLeft[Option[PeriodicSubmissionId]]
      )
      await(
        underTest
          .saveForeignPropertyTax(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            nino,
            foreignPropertyTaxWithCountryCode
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  ".getAllPropertyPeriodicSubmissions" should {

    "return data when GetPeriodicSubmission has ids and the period is for a year" in {
      val periodicSubmissionId = "1"
      val periodicSubmissionIds = List(
        PeriodicSubmissionIdModel(periodicSubmissionId, LocalDate.parse("2023-04-06"), LocalDate.parse("2024-04-05"))
      )
      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        Some(PeriodicSubmissionId(periodicSubmissionId)),
        submittedOn = Some(LocalDateTime.now),
        fromDate = LocalDate.now.minusDays(1),
        toDate = LocalDate.now,
        None,
        None
      )

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(periodicSubmissionIds))
      mockGetPropertyPeriodicSubmission(taxYear, nino, incomeSourceId, "1", Right(Some(propertyPeriodicSubmission)))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe
        Right(PropertyPeriodicSubmissionResponse(List(propertyPeriodicSubmission)))
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and the period is less than a year" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and there is no submission" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )
    }

    "return DataNotFoundError when GetPeriodicSubmission does not have ids" in {
      val aPeriodicSubmissionModel = List.empty[PeriodicSubmissionIdModel]

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )

    }

    "return ApiError when GetPeriodicSubmissionIds fails" in {
      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("code", "error")))
      )
      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Left(
        ApiServiceError(500)
      )
    }
  }

  val validCreatePropertyPeriodicSubmissionRequest: CreatePropertyPeriodicSubmissionRequest =
    CreatePropertyPeriodicSubmissionRequest(
      LocalDate.now(),
      LocalDate.now(),
      foreignProperty,
      Some(
        UkOtherProperty(
          Some(UkOtherPropertyIncome(Some(200.00), Some(200.00), Some(200.00), Some(200.00), Some(200.00), None)),
          None
        )
      )
    )
  val validUpdatePropertyPeriodicSubmissionRequest: UpdatePropertyPeriodicSubmissionRequest =
    UpdatePropertyPeriodicSubmissionRequest(
      foreignProperty,
      Some(
        UkOtherProperty(
          Some(UkOtherPropertyIncome(Some(200.00), Some(200.00), Some(200.00), Some(200.00), Some(200.00), None)),
          None
        )
      )
    )

  val propertyPeriodicSubmission: PropertyPeriodicSubmission = PropertyPeriodicSubmission(
    None,
    None,
    LocalDate.now(),
    LocalDate.now(),
    foreignProperty,
    Some(
      UkOtherProperty(
        Some(UkOtherPropertyIncome(Some(200.00), Some(200.00), Some(200.00), Some(200.00), Some(200.00), None)),
        None
      )
    )
  )

  "create periodic submission" should {

    "return submissionId for valid request" in {
      val periodicSubmissionId = PeriodicSubmissionId("submissionId")

      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        validCreatePropertyPeriodicSubmissionRequest,
        Right(Some(periodicSubmissionId))
      )

      await(
        underTest
          .createPeriodicSubmission(nino, incomeSourceId, taxYear, validCreatePropertyPeriodicSubmissionRequest)
          .value
      ) shouldBe
        Right(Some(periodicSubmissionId))
    }

    "return ApiError for invalid request" in {

      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        validCreatePropertyPeriodicSubmissionRequest,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest
          .createPeriodicSubmission(nino, incomeSourceId, taxYear, validCreatePropertyPeriodicSubmissionRequest)
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }

  }

}
