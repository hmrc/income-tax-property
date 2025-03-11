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
import models.errors.{ApiError, ApiServiceError, DataNotFoundError, SingleErrorBody}
import models.request._
import models.request.foreign._
import models.request.foreign.adjustments.{ForeignPropertyAdjustmentsWithCountryCode, ForeignUnusedResidentialFinanceCost, ForeignWhenYouReportedTheLoss, UnusedLossesPreviousYears}
import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.request.foreign.expenses.{ConsolidatedExpenses, ForeignPropertyExpensesWithCountryCode}
import models.request.foreign.sba.{ForeignPropertySbaWithCountryCode, ForeignStructureBuildingAllowance, ForeignStructureBuildingAllowanceAddress}
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
  private val foreignPropertyIncome = ForeignPropertyIncome(None, Some(true), None, None, Some(456.75), Some(678.95))

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

  "For Foreign Property Periodic Submissions getAllPropertyPeriodicSubmissions" should {

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

      mockGetAllPeriodicSubmissionIds(taxYear, nino, incomeSourceId, Right(periodicSubmissionIds))
      mockGetPropertyPeriodicSubmission(taxYear, nino, incomeSourceId, "1", Right(Some(propertyPeriodicSubmission)))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe
        Right(PropertyPeriodicSubmissionResponse(List(propertyPeriodicSubmission)))
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and the period is less than a year" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmissionIds(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and there is no submission" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmissionIds(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )
    }

    "return DataNotFoundError when GetPeriodicSubmission does not have ids" in {
      val aPeriodicSubmissionModel = List.empty[PeriodicSubmissionIdModel]

      mockGetAllPeriodicSubmissionIds(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )

    }

    "return ApiError when GetPeriodicSubmissionIds fails" in {
      mockGetAllPeriodicSubmissionIds(
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
        TotalIncome.Over,
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
        Some(ForeignIncomeTax(isForeignIncomeTax = true, Some(BigDecimal(50)))),
        Some(false)
      )

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

    "return no content for valid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)

      mockGetAllPeriodicSubmissionIds(
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
      val isForeignIncomeTax = true
      val foreignTaxCreditRelief = Some(isForeignIncomeTax)
      val foreignTaxPaidOrDeducted = Some(BigDecimal(56.78))
      val foreignProperty = ForeignProperty(
        countryCode = countryCode,
        income = Some(
          ForeignPropertyIncome(
            rentIncome = Some(ForeignPropertyRentIncome(rentAmount = BigDecimal(12.34))),
            isForeignTaxCreditRelief = foreignTaxCreditRelief,
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
            isForeignIncomeTax = isForeignIncomeTax,
            foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted
          )
        ),
        isForeignTaxCreditRelief = foreignTaxCreditRelief
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

      mockGetAllPeriodicSubmissionIds(
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
      mockGetAllPeriodicSubmissionIds(
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
      consolidatedExpenses = Some(ConsolidatedExpenses(isConsolidatedOrIndividualExpenses = false, None)),
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

      mockGetAllPeriodicSubmissionIds(
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

      mockGetAllPeriodicSubmissionIds(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel(submissionId, fromDate, toDate)).asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        submissionId,
        Some(emptyPeriodicSubmission).asRight[ApiError]
      )

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

    val foreignIncome = ForeignIncomeWithCountryCode(
      countryCode = "AUS",
      rentIncome = 1.0,
      isPremiumsGrantLeaseReceived = true,
      otherPropertyIncome = BigDecimal(54.94),
      calculatedPremiumLeaseTaxable =
        Some(CalculatedPremiumLeaseTaxable(isCalculatedPremiumLeaseTaxable = false, premiumsOfLeaseGrant = None)),
      receivedGrantLeaseAmount = Some(3.45),
      twelveMonthPeriodsInLease = Some(5),
      premiumsOfLeaseGrantAgreed =
        Some(PremiumsOfLeaseGrantAgreed(isPremiumsOfLeaseGrantAgreed = true, premiumsOfLeaseGrant = Some(54.9)))
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

      mockGetAllPeriodicSubmissionIds(
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
                    isForeignTaxCreditRelief = Some(true),
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

      mockGetAllPeriodicSubmissionIds(
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

      mockGetAllPeriodicSubmissionIds(
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
      mockGetAllPeriodicSubmissionIds(
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

  "save foreign allowances should" should {

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

    val foreignAllowancesWithCountryCode = ForeignPropertyAllowancesWithCountryCode(
      countryCode = "AUS",
      zeroEmissionsCarAllowance = Some(BigDecimal(1.5)),
      zeroEmissionsGoodsVehicleAllowance = Some(BigDecimal(2.5)),
      costOfReplacingDomesticItems = Some(BigDecimal(3.5)),
      otherCapitalAllowance = Some(BigDecimal(4.5)),
      capitalAllowancesForACar = None
    )

    "call create foreign annual submission request when annual submission is empty and return no content for valid request" in {

      val emptyAnnualForeignPropertySubmissionFromDownstream = AnnualForeignPropertySubmission(None)

      val requestForCreate: AnnualForeignPropertySubmissionAllowances =
        AnnualForeignPropertySubmission.fromForeignPropertyAllowances(
          Some(emptyAnnualForeignPropertySubmissionFromDownstream),
          foreignAllowancesWithCountryCode
        )

      mockGetAnnualForeignPropertySubmission(
        taxYear,
        nino,
        incomeSourceId,
        Right(Some(emptyAnnualForeignPropertySubmissionFromDownstream))
      )

      mockCreateAnnualForeignPropertySubmissionAllowances(
        taxYear,
        incomeSourceId,
        nino,
        Right(requestForCreate)
      )

      await(
        underTest
          .saveForeignPropertyAllowances(
            ctx.toJourneyContext(JourneyName.ForeignPropertyTax),
            nino,
            foreignAllowancesWithCountryCode
          )
          .value
      ) shouldBe Right(true)
    }

    "call update annual foreign property submission allowances when there is an existing annual submission" in {

      val mayBeAnnualForeignPropertySubmissionFromDownstream =
        AnnualForeignPropertySubmission(
          foreignProperty = Some(
            Seq(
              AnnualForeignProperty(
                countryCode = "AUS",
                allowances = Some(
                  ForeignPropertyAllowances(
                    zeroEmissionsCarAllowance = Some(21.5),
                    zeroEmissionsGoodsVehicleAllowance = Some(32.5),
                    costOfReplacingDomesticItems = Some(43.5),
                    otherCapitalAllowance = Some(54.5),
                    annualInvestmentAllowance = Some(64.5),
                    propertyAllowance = Some(74.5),
                    electricChargePointAllowance = Some(84.5),
                    structuredBuildingAllowance = None
                  )
                ),
                adjustments = None
              )
            )
          )
        )

      val requestForCreate: AnnualForeignPropertySubmissionAllowances =
        AnnualForeignPropertySubmission.fromForeignPropertyAllowances(
          Some(mayBeAnnualForeignPropertySubmissionFromDownstream),
          foreignAllowancesWithCountryCode
        )

      mockGetAnnualForeignPropertySubmission(
        taxYear,
        nino,
        incomeSourceId,
        Right(Some(mayBeAnnualForeignPropertySubmissionFromDownstream))
      )

      mockCreateAnnualForeignPropertySubmissionAllowances(
        taxYear,
        incomeSourceId,
        nino,
        Right(requestForCreate)
      )

      await(
        underTest
          .saveForeignPropertyAllowances(
            ctx.toJourneyContext(JourneyName.ForeignPropertyTax),
            nino,
            foreignAllowancesWithCountryCode
          )
          .value
      ) shouldBe Right(true)
    }

    "return ApiError BAD_REQUEST for invalid request body" in {

      mockGetAnnualForeignPropertySubmission(
        taxYear,
        nino,
        incomeSourceId,
        Right(None)
      )

      mockCreateAnnualForeignPropertySubmissionAllowances(
        taxYear,
        incomeSourceId,
        nino,
        ApiError(BAD_REQUEST, SingleErrorBody("code", "error")).asLeft[Unit]
      )

      await(
        underTest
          .saveForeignPropertyAllowances(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            nino,
            foreignAllowancesWithCountryCode
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save foreign property adjustments" should {
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(
      taxYear = taxYear,
      incomeSourceId = incomeSourceId,
      mtditid = Mtditid(mtditid),
      nino = nino
    )

    val foreignPropertyAdjustmentsWithCountryCode = ForeignPropertyAdjustmentsWithCountryCode(
      countryCode = "AUS",
      privateUseAdjustment = BigDecimal(25.25),
      balancingCharge = BalancingCharge(isBalancingCharge = true, balancingChargeAmount = Some(BigDecimal(50.50))),
      residentialFinanceCost = Some(BigDecimal(75.75)),
      unusedResidentialFinanceCost = Some(
        ForeignUnusedResidentialFinanceCost(
          isForeignUnusedResidentialFinanceCost = true,
          foreignUnusedResidentialFinanceCostAmount = Some(BigDecimal(101.01))
        )
      ),
      propertyIncomeAllowanceClaim = None,
      unusedLossesPreviousYears = UnusedLossesPreviousYears(
        isUnusedLossesPreviousYears = true,
        unusedLossesPreviousYearsAmount = Some(BigDecimal(80.8))
      ),
      whenYouReportedTheLoss = Some(ForeignWhenYouReportedTheLoss.y2018to2019)
    )

    "persist the foreign adjustments supporting answers into the backend mongo" when {
      val ctx = JourneyContext(
        taxYear,
        incomeSourceId,
        Mtditid(mtditid),
        JourneyName.ForeignPropertyAdjustments
      )
      val submissionId = "test-periodic-submission-id"
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)
      val mayBeAnnualForeignPropertySubmissionFromDownstream =
        AnnualForeignPropertySubmission(
          foreignProperty = Some(
            Seq(
              AnnualForeignProperty(
                countryCode = "AUS",
                allowances = Some(
                  ForeignPropertyAllowances(
                    zeroEmissionsCarAllowance = Some(21.5),
                    zeroEmissionsGoodsVehicleAllowance = Some(32.5),
                    costOfReplacingDomesticItems = Some(43.5),
                    otherCapitalAllowance = Some(54.5),
                    annualInvestmentAllowance = Some(64.5),
                    propertyAllowance = Some(74.5),
                    electricChargePointAllowance = Some(84.5),
                    structuredBuildingAllowance = None
                  )
                ),
                adjustments = None
              )
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

      "return no content for valid request" in {


        val requestForCreateAnnual =
          AnnualForeignPropertySubmission.fromForeignPropertyAdjustments(
            foreignPropertyAdjustmentsWithCountryCode
          )

        mockCreateAnnualForeignPropertySubmissionAdjustments(
          taxYear,
          incomeSourceId,
          nino,
          Right(requestForCreateAnnual)
        )
        mockGetAllPeriodicSubmissionIds(
          taxYear,
          nino,
          incomeSourceId,
          List(PeriodicSubmissionIdModel(submissionId, fromDate, toDate)).asRight[ApiError]
        )

        mockGetPropertyPeriodicSubmission(
          taxYear,
          nino,
          incomeSourceId,
          submissionId,
          Some(propertyPeriodicSubmission).asRight[ApiError]
        )

        val Right(requestForUpdatePeriodic: UpdateForeignPropertyPeriodicSubmissionRequest) =
          UpdateForeignPropertyPeriodicSubmissionRequest.fromForeignAdjustments(
            Some(propertyPeriodicSubmission),
            foreignPropertyAdjustmentsWithCountryCode
          )

        mockUpdateForeignPeriodicSubmission(
          taxYear = taxYear,
          taxableEntityId = nino,
          incomeSourceId = incomeSourceId,
          updateRequest = requestForUpdatePeriodic,
          submissionId = submissionId,
          result = Right(Some(submissionId))
        )

        await(
          underTest
            .saveForeignPropertyAdjustments(
              ctx,
              nino,
              foreignPropertyAdjustmentsWithCountryCode
            )
            .value
        ) shouldBe Right(true)
      }
      "claiming PIA" in {
        val foreignPropertyAdjustmentsClaimingPIA = foreignPropertyAdjustmentsWithCountryCode.copy(
          propertyIncomeAllowanceClaim = Some(85.85),
          residentialFinanceCost = None,
          unusedResidentialFinanceCost = None
        )

        val requestForCreateAnnual = AnnualForeignPropertySubmission.fromForeignPropertyAdjustmentsPIA(foreignPropertyAdjustmentsClaimingPIA)

        mockCreateAnnualForeignPropertySubmission(
          taxYear,
          incomeSourceId,
          nino,
          Some(requestForCreateAnnual),
          ().asRight[ApiError]
        )

        await(
          underTest
            .saveForeignPropertyAdjustments(
              ctx,
              nino,
              foreignPropertyAdjustmentsClaimingPIA
            )
            .value
        ) shouldBe Right(true)
      }
    }

  }

  "save foreign property sba should" should {

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

    val foreignPropertySbaWithCountryCode = ForeignPropertySbaWithCountryCode(
      countryCode = "AUS",
      isClaimStructureBuildingAllowance = true,
      allowances = Some(
        Seq(
          ForeignStructureBuildingAllowance(
            foreignStructureBuildingAllowanceClaim = BigDecimal(546.78),
            foreignStructureBuildingQualifyingDate = LocalDate.of(2024, 8, 7),
            foreignStructureBuildingQualifyingAmount = BigDecimal(28.95),
            foreignStructureBuildingAddress = ForeignStructureBuildingAllowanceAddress(
              "Aryan Cements",
              "45A",
              "110 001"
            )
          )
        )
      )
    )

    "return true if it persists successfully the foreign property sba into the BE mongo" in {

      val mayBeAnnualForeignPropertySubmissionFromDownstream =
        Some(
          AnnualForeignPropertySubmission(
            foreignProperty = Some(
              Seq(
                AnnualForeignProperty(
                  countryCode = "AUS",
                  allowances = Some(
                    ForeignPropertyAllowances(
                      zeroEmissionsCarAllowance = Some(21.5),
                      zeroEmissionsGoodsVehicleAllowance = Some(32.5),
                      costOfReplacingDomesticItems = Some(43.5),
                      otherCapitalAllowance = Some(54.5),
                      annualInvestmentAllowance = Some(64.5),
                      propertyAllowance = Some(74.5),
                      electricChargePointAllowance = Some(84.5),
                      structuredBuildingAllowance = None
                    )
                  ),
                  adjustments = None
                )
              )
            )
          )
        )

      mockGetAnnualForeignPropertySubmission(
        taxYear,
        nino,
        incomeSourceId,
        Right(mayBeAnnualForeignPropertySubmissionFromDownstream)
      )

      val Right(requestForCreate: AnnualForeignPropertySubmission) =
        AnnualForeignPropertySubmission.fromForeignPropertySbas(
          mayBeAnnualForeignPropertySubmissionFromDownstream,
          foreignPropertySbaWithCountryCode
        )

      mockCreateAnnualForeignPropertySubmission(
        taxYear,
        incomeSourceId,
        nino,
        Some(requestForCreate),
        ().asRight[ApiError]
      )

      await(
        underTest
          .saveForeignPropertySba(
            ctx.toJourneyContext(JourneyName.ForeignPropertySba),
            nino,
            foreignPropertySbaWithCountryCode
          )
          .value
      ) shouldBe Right(true)
    }

    "return true but not call downstream when not claiming SBA" in {
      val foreignPropertySbaWithCountryCodeNoClaim = ForeignPropertySbaWithCountryCode(
        countryCode = "AUS",
        isClaimStructureBuildingAllowance = false,
        allowances = None
      )

      await(
        underTest
          .saveForeignPropertySba(
            ctx.toJourneyContext(JourneyName.ForeignPropertySba),
            nino,
            foreignPropertySbaWithCountryCodeNoClaim
          )
          .value
      ) shouldBe Right(true)
    }
  }

  " For Foreign Property Periodic Submissions getAnnualForeignPropertySubmissionFromDownStream" should {

    "return data when successful" in {
      val mayBeAnnualForeignPropertySubmissionFromDownstream =
        AnnualForeignPropertySubmission(
          foreignProperty = Some(
            Seq(
              AnnualForeignProperty(
                countryCode = "AUS",
                allowances = Some(
                  ForeignPropertyAllowances(
                    zeroEmissionsCarAllowance = Some(21.5),
                    zeroEmissionsGoodsVehicleAllowance = Some(32.5),
                    costOfReplacingDomesticItems = Some(43.5),
                    otherCapitalAllowance = Some(54.5),
                    annualInvestmentAllowance = Some(64.5),
                    propertyAllowance = Some(74.5),
                    electricChargePointAllowance = Some(84.5),
                    structuredBuildingAllowance = None
                  )
                ),
                adjustments = None
              )
            )
          )
        )

      mockGetAnnualForeignPropertySubmission(
        taxYear,
        nino,
        incomeSourceId,
        Right(Some(mayBeAnnualForeignPropertySubmissionFromDownstream))
      )

      await(underTest.getAnnualForeignPropertySubmissionFromDownStream(taxYear, nino, incomeSourceId).value) shouldBe
        Right(mayBeAnnualForeignPropertySubmissionFromDownstream)
    }

    "return DataNotFoundError when GetAnnualForeignPropertySubmissionFromDownStream does not has an annual submission" in {

      mockGetAnnualForeignPropertySubmission(taxYear, nino, incomeSourceId, Right(None))

      await(
        underTest.getAnnualForeignPropertySubmissionFromDownStream(taxYear, nino, incomeSourceId).value
      ) shouldBe Left(DataNotFoundError)

    }

    "return when GetAnnualForeignPropertySubmissionFromDownStream has an annual submission without foreignProperty data" in {

      val annualForeignPropertySubmissionWithoutForeignPropertyData =
        AnnualForeignPropertySubmission(foreignProperty = None)

      mockGetAnnualForeignPropertySubmission(
        taxYear,
        nino,
        incomeSourceId,
        Right(Some(annualForeignPropertySubmissionWithoutForeignPropertyData))
      )

      await(
        underTest.getAnnualForeignPropertySubmissionFromDownStream(taxYear, nino, incomeSourceId).value
      ) shouldBe Right(annualForeignPropertySubmissionWithoutForeignPropertyData)
    }

    "return ApiError when GetAnnualForeignPropertySubmissionFromDownStream fails" in {
      mockGetAnnualForeignPropertySubmission(
        taxYear,
        nino,
        incomeSourceId,
        Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("code", "error")))
      )
      await(
        underTest.getAnnualForeignPropertySubmissionFromDownStream(taxYear, nino, incomeSourceId).value
      ) shouldBe Left(
        ApiServiceError(500)
      )
    }
  }

}
