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
import models.request._
import models.request.foreign._
import models.request.foreign.expenses.{ConsolidatedExpenses, ForeignPropertyExpensesWithCountryCode}
import models.responses._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.mocks.{MockIntegrationFrameworkConnector, MockMergeService, MockMongoJourneyAnswersRepository}
import utils.{AppConfigStub, UnitTest}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.concurrent.ExecutionContext.Implicits.global

class ForeignPropertyServiceSpec
    extends UnitTest with MockIntegrationFrameworkConnector with MockMongoJourneyAnswersRepository with MockMergeService
    with HttpClientSupport with ScalaCheckPropertyChecks {

  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  private val nino = Nino("A34324")
  private val incomeSourceId = IncomeSourceId("ForeignProperty")
  private val taxYear: TaxYear = TaxYear(2024)
  private val foreignPropertyIncome = ForeignPropertyIncome(None, Some(true), None, None, Some(456.75),Some(678.95))
  val foreignProperty: Option[Seq[ForeignProperty]] = Some(
    Seq(
      ForeignProperty(
        "AUS",
        Some(foreignPropertyIncome),
        None
      )
    )
  )

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()
  private val underTest = new ForeignPropertyService(mockIntegrationFrameworkConnector, repository)

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

      val Right(requestForCreate: CreateForeignPropertyPeriodicSubmissionRequest) =
        CreateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
          taxYear,
          Some(emptyPeriodicSubmission),
          foreignPropertyTaxWithCountryCode
        )

      mockCreateForeignPeriodicSubmission(
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

    "update pre-existing periodic submissions" in {
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)
      val periodicSubmissionId = "1"
      val countryCode = "USA"
      val foreignIncomeTaxYesNo = true
      val foreignTaxCreditRelief = Some(foreignIncomeTaxYesNo)
      val foreignTaxPaidOrDeducted = Some(BigDecimal(56.78))
      val foreignProperty = ForeignProperty(
        countryCode = countryCode,
        income = Some(
          ForeignPropertyIncome(
            rentIncome = Some(ForeignPropertyRentIncome(rentAmount = BigDecimal(12.34))),
            foreignTaxCreditRelief = foreignTaxCreditRelief,
            premiumsOfLeaseGrant = Some(BigDecimal(13.34)),
            otherPropertyIncome = Some(BigDecimal(24.56)),
            foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted,
            specialWithholdingTaxOrUkTaxPaid = Some(BigDecimal(89.10))
          )
        ),
        expenses = Some(
          models.responses.ForeignPropertyExpenses(
            premisesRunningCosts = Some(BigDecimal(23.34)),
            repairsAndMaintenance = Some(BigDecimal(32.21)),
            financialCosts = Some(BigDecimal(54.32)),
            professionalFees = Some(BigDecimal(65.43)),
            travelCosts = Some(BigDecimal(22.22)),
            costOfServices = Some(BigDecimal(10.10)),
            residentialFinancialCost = Some(BigDecimal(11.11)),
            broughtFwdResidentialFinancialCost = Some(BigDecimal(23.22)),
            other = Some(BigDecimal(44.44)),
            consolidatedExpense = Some(BigDecimal(90.05)),
            consolidatedExpenseAmount = None
          )
        )
      )
      val foreignPropertyTaxWithCountryCode = ForeignPropertyTaxWithCountryCode(
        countryCode = countryCode,
        foreignIncomeTax = Some(
          ForeignIncomeTax(
            foreignIncomeTaxYesNo = foreignIncomeTaxYesNo,
            foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted
          )
        ),
        foreignTaxCreditRelief = foreignTaxCreditRelief
      )

      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        Some(PeriodicSubmissionId(periodicSubmissionId)),
        submittedOn = Some(LocalDateTime.now),
        fromDate = fromDate,
        toDate = toDate,
        foreignProperty = Some(Seq(foreignProperty)),
        None
      )

      val Right(updatePropertyPeriodicSubmissionRequest: UpdateForeignPropertyPeriodicSubmissionRequest) =
        UpdateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
          maybeSubmission = Some(propertyPeriodicSubmission),
          foreignPropertyTaxWithCountryCode = foreignPropertyTaxWithCountryCode
        )

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel(periodicSubmissionId, fromDate, toDate)).asRight[ApiError]
      )
      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        periodicSubmissionId,
        Some(propertyPeriodicSubmission).asRight[ApiError]
      )
      mockUpdateForeignPeriodicSubmission(
        taxYear = taxYear,
        taxableEntityId = nino,
        incomeSourceId = incomeSourceId,
        updateRequest = updatePropertyPeriodicSubmissionRequest,
        submissionId = periodicSubmissionId,
        result = Right(Some(periodicSubmissionId))
      )

      await(
        underTest
          .saveForeignPropertyTax(
            ctx.toJourneyContext(JourneyName.ForeignPropertyTax),
            nino,
            foreignPropertyTaxWithCountryCode
          )
          .value
      ) shouldBe Right(Some(PeriodicSubmissionId(periodicSubmissionId)))
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

      val Right(requestForCreate: CreateForeignPropertyPeriodicSubmissionRequest) =
        CreateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyTax(
          taxYear,
          Some(emptyPeriodicSubmission),
          foreignPropertyTaxWithCountryCode
        )

      mockCreateForeignPeriodicSubmission(
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

  "save foreign property expenses" should {

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(
      taxYear,
      incomeSourceId,
      Mtditid(mtditid),
      nino
    )

    val foreignPropertyExpenses = ForeignPropertyExpensesWithCountryCode(
      countryCode = "BRA",
      consolidatedExpenses = Some(ConsolidatedExpenses(consolidatedOrIndividualExpensesYesNo = false, None)),
      premisesRunningCosts = Some(50),
      repairsAndMaintenance = Some(60),
      financialCosts = Some(675),
      professionalFees = Some(85),
      costOfServices = Some(234),
      other = Some(99)
    )

    "call create foreign periodic submission request when periodic submission is empty and return no content for valid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)

      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(taxYear)),
          LocalDate.parse(TaxYear.endDate(taxYear)),
          None,
          None
        )

      val Right(requestForCreate: CreateForeignPropertyPeriodicSubmissionRequest) =
        CreateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyExpenses(
          taxYear,
          Some(emptyPeriodicSubmission),
          foreignPropertyExpenses
        )

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("", fromDate, toDate)).asRight[ApiError]
      )

      mockCreateForeignPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        Some(PeriodicSubmissionId("create123")).asRight[ApiError]
      )

      await(
        underTest
          .saveForeignPropertyExpenses(
            ctx.toJourneyContext(JourneyName.ForeignPropertyExpenses),
            nino,
            foreignPropertyExpenses
          )
          .value
      ) shouldBe Right(Some(PeriodicSubmissionId("create123")))
    }

    "call update periodic submission request when there is an existing periodic submission" in {
      val submissionId = "test-periodic-submission-id"
      val periodicSubmissionId = PeriodicSubmissionId(submissionId)
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)
      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(taxYear)),
          LocalDate.parse(TaxYear.endDate(taxYear)),
          None,
          None
        )
      val Right(requestForUpdate: UpdateForeignPropertyPeriodicSubmissionRequest) =
        UpdateForeignPropertyPeriodicSubmissionRequest.fromForeignPropertyExpenses(
          Some(emptyPeriodicSubmission),
          foreignPropertyExpenses
        )
      mockUpdateForeignPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForUpdate,
        submissionId,
        Some(submissionId).asRight[ApiError]
      )

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel(submissionId, fromDate, toDate)).asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(taxYear, nino, incomeSourceId, submissionId, Some(emptyPeriodicSubmission).asRight[ApiError])

      await(
        underTest
          .saveForeignPropertyExpenses(
            ctx.toJourneyContext(JourneyName.ForeignPropertyExpenses),
            nino,
            foreignPropertyExpenses
          )
          .value
      ) shouldBe Right(Some(periodicSubmissionId))
    }
  }

  "save foreign income should" should {

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

    val foreignIncome = ForeignIncome(
      countryCode = "AUS",
      rentIncome = 1.0,
      premiumsGrantLeaseReceived = true,
      reversePremiumsReceived = ReversePremiumsReceived(reversePremiumsReceived = true, Some(BigDecimal(2.50))),
      otherPropertyIncome = BigDecimal(54.94),
      calculatedPremiumLeaseTaxable =
        Some(CalculatedPremiumLeaseTaxable(calculatedPremiumLeaseTaxable = false, premiumsOfLeaseGrant = None)),
      receivedGrantLeaseAmount = Some(3.45),
      twelveMonthPeriodsInLease = Some(5),
      premiumsOfLeaseGrantAgreed =
        Some(PremiumsOfLeaseGrantAgreed(premiumsOfLeaseGrantAgreed = true, premiumsOfLeaseGrant = Some(54.9)))
    )

    "call create foreign periodic submission request when periodic submission is empty and return no content for valid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)

      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(taxYear)),
          LocalDate.parse(TaxYear.endDate(taxYear)),
          None,
          None
        )

      val Right(requestForCreate: CreateForeignPropertyPeriodicSubmissionRequest) =
        CreateForeignPropertyPeriodicSubmissionRequest.fromForeignIncome(
          taxYear,
          Some(emptyPeriodicSubmission),
          foreignIncome
        )

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("", fromDate, toDate)).asRight[ApiError]
      )

      mockCreateForeignPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        Some(PeriodicSubmissionId("create123")).asRight[ApiError]
      )

      await(
        underTest
          .saveForeignIncome(
            ctx.toJourneyContext(JourneyName.ForeignPropertyTax),
            nino,
            foreignIncome
          )
          .value
      ) shouldBe Right(Some(PeriodicSubmissionId("create123")))
    }

    "call update periodic submission request when there is an existing periodic submission" in {

      val existingPeriodicSubmission =
        PropertyPeriodicSubmission(
          submissionId = Some(PeriodicSubmissionId("456")),
          submittedOn = Some(LocalDateTime.of(LocalDate.parse(TaxYear.startDate(taxYear)), LocalTime.of(10, 11))),
          fromDate = LocalDate.parse(TaxYear.startDate(taxYear)),
          toDate = LocalDate.parse(TaxYear.endDate(taxYear)),
          foreignProperty = Some(
            Seq(
              ForeignProperty(
                countryCode = "AUS",
                income = Some(
                  ForeignPropertyIncome(
                    rentIncome = Some(ForeignPropertyRentIncome(rentAmount = 12345.75)),
                    foreignTaxCreditRelief = Some(true),
                    premiumsOfLeaseGrant = Some(234.50),
                    otherPropertyIncome = Some(345.65),
                    foreignTaxPaidOrDeducted = Some(456.75),
                    specialWithholdingTaxOrUkTaxPaid = Some(678.95)
                  )
                ),
                expenses = None
              )
            )
          ),
          ukOtherProperty = Some(
            UkOtherProperty(
              Some(UkOtherPropertyIncome(Some(200.0), Some(200.0), Some(200.0), Some(200.0), Some(200.0), None)),
              None
            )
          )
        )

      val Right(requestForUpdate: UpdateForeignPropertyPeriodicSubmissionRequest) =
        UpdateForeignPropertyPeriodicSubmissionRequest.fromForeignIncome(
          Some(existingPeriodicSubmission),
          foreignIncome
        )

      val fromDate = LocalDate.of(taxYear.endYear - 1, 4, 6)
      val toDate = LocalDate.of(taxYear.endYear, 4, 5)

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("456", fromDate, toDate)).asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "456",
        Some(propertyPeriodicSubmission).asRight[ApiError]
      )

      mockUpdateForeignPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForUpdate,
        "456",
        Some("456").asRight[ApiError]
      )

      await(
        underTest
          .saveForeignIncome(
            ctx.toJourneyContext(JourneyName.ForeignPropertyTax),
            nino,
            foreignIncome
          )
          .value
      ) shouldBe Right(Some(PeriodicSubmissionId("456")))
    }

    "persist the foreign income supporting answers into the backend mongo" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)

      val ctx = JourneyContext(
        taxYear,
        incomeSourceId,
        Mtditid(mtditid),
        JourneyName.ForeignPropertyIncome
      )

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("", fromDate, toDate)).asRight[ApiError]
      )

      val existingPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(taxYear)),
          LocalDate.parse(TaxYear.endDate(taxYear)),
          None,
          None
        )

      val Right(requestForCreate: CreateForeignPropertyPeriodicSubmissionRequest) =
        CreateForeignPropertyPeriodicSubmissionRequest.fromForeignIncome(
          taxYear,
          maybeSubmission = Some(existingPeriodicSubmission),
          foreignIncome = foreignIncome
        )

      mockCreateForeignPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        Some(PeriodicSubmissionId("25")).asRight[ApiError]
      )

      await(
        underTest
          .saveForeignIncome(
            ctx,
            nino,
            foreignIncome
          )
          .value
      ) shouldBe Right(Some(PeriodicSubmissionId("25")))
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

      val Right(requestForCreate: CreateForeignPropertyPeriodicSubmissionRequest) =
        CreateForeignPropertyPeriodicSubmissionRequest.fromForeignIncome(
          taxYear,
          Some(emptyPeriodicSubmission),
          foreignIncome
        )

      mockCreateForeignPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        ApiError(BAD_REQUEST, SingleErrorBody("code", "error")).asLeft[Option[PeriodicSubmissionId]]
      )

      await(
        underTest
          .saveForeignIncome(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            nino,
            foreignIncome
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

  val validCreateForeignPropertyPeriodicSubmissionRequest: CreateForeignPropertyPeriodicSubmissionRequest =
    CreateForeignPropertyPeriodicSubmissionRequest(
      LocalDate.now(),
      LocalDate.now(),
      foreignProperty
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

      mockCreateForeignPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        validCreateForeignPropertyPeriodicSubmissionRequest,
        Right(Some(periodicSubmissionId))
      )

      await(
        underTest
          .createForeignPeriodicSubmission(
            nino,
            incomeSourceId,
            taxYear,
            validCreateForeignPropertyPeriodicSubmissionRequest
          )
          .value
      ) shouldBe
        Right(Some(periodicSubmissionId))
    }

    "return ApiError for invalid request" in {

      mockCreateForeignPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        validCreateForeignPropertyPeriodicSubmissionRequest,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest
          .createForeignPeriodicSubmission(
            nino,
            incomeSourceId,
            taxYear,
            validCreateForeignPropertyPeriodicSubmissionRequest
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }

  }
}
