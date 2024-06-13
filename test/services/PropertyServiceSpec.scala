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

import cats.data.EitherT
import cats.syntax.either._
import config.AppConfig
import models.PropertyPeriodicSubmissionResponse
import models.common._
import models.domain.JourneyAnswers
import models.errors._
import models.request._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba._
import models.responses._
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper
import org.scalatest.time.{Millis, Span}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR}
import play.api.libs.json.{JsObject, Json}
import repositories.ExpireAtCalculator.calculateExpireAt
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.mocks.{MockIntegrationFrameworkConnector, MockMongoJourneyAnswersRepository}
import utils.{AppConfigStub, UnitTest}

import java.time.{Clock, LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PropertyServiceSpec
    extends UnitTest with MockIntegrationFrameworkConnector with MockMongoJourneyAnswersRepository
    with HttpClientSupport with ScalaCheckPropertyChecks {
  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()
  private val underTest = new PropertyService(mockIntegrationFrameworkConnector, repository)
  private val nino = "A34324"
  private val incomeSourceId = "Rental"
  private val submissionId = "submissionId"
  private val taxableEntityId = "taxableEntityId"

  ".getAllPropertyPeriodicSubmissions" should {

    "return data when GetPeriodicSubmission has ids and the period is for a year" in {
      val periodicSubmissionId = "1"
      val periodicSubmissionIds = List(
        PeriodicSubmissionIdModel(periodicSubmissionId, LocalDate.parse("2023-01-01"), LocalDate.parse("2024-01-02"))
      )
      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        Some(PeriodicSubmissionId(periodicSubmissionId)),
        submittedOn = Some(LocalDateTime.now),
        fromDate = LocalDate.now.minusDays(1),
        toDate = LocalDate.now,
        None,
        None,
        None,
        None
      )
      val taxYear = 2024

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(periodicSubmissionIds))
      mockGetPropertyPeriodicSubmission(taxYear, nino, incomeSourceId, "1", Right(Some(propertyPeriodicSubmission)))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe
        Right(PropertyPeriodicSubmissionResponse(List(propertyPeriodicSubmission)))
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and the period is less than a year" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      val taxYear = 2024

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(PropertyPeriodicSubmissionResponse(List()))
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and there is no submission" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Right(PropertyPeriodicSubmissionResponse(List()))
    }

    "return DataNotFoundError when GetPeriodicSubmission does not have ids" in {
      val aPeriodicSubmissionModel = List.empty[PeriodicSubmissionIdModel]

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Right(PropertyPeriodicSubmissionResponse(List()))

    }

    "return ApiError when GetPeriodicSubmissionIds fails" in {
      mockGetAllPeriodicSubmission(
        2024,
        "A34324",
        "Rental",
        Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("code", "error")))
      )
      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Left(
        ApiServiceError(500)
      )
    }
  }

  ".getPropertyAnnualSubmission" should {

    "return data when successful" in {
      val aPropertyAnnualSubmission = PropertyAnnualSubmission(
        submittedOn = Some(LocalDateTime.now),
        Some(
          AnnualForeignFhlEea(
            ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
            ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
          )
        ),
        None,
        None,
        None
      )
      val taxYear = 2024

      mockGetPropertyAnnualSubmission(taxYear, nino, incomeSourceId, Right(Some(aPropertyAnnualSubmission)))

      await(underTest.getPropertyAnnualSubmission(taxYear, nino, incomeSourceId).value) shouldBe
        Right(aPropertyAnnualSubmission)
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and the period is less than a year" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      val taxYear = 2024

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(PropertyPeriodicSubmissionResponse(List()))
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and there is no submission" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Right(PropertyPeriodicSubmissionResponse(List()))
    }

    "return DataNotFoundError when GetPeriodicSubmission does not have ids" in {
      val aPeriodicSubmissionModel = List.empty[PeriodicSubmissionIdModel]

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Right(PropertyPeriodicSubmissionResponse(List()))

    }

    "return ApiError when GetPeriodicSubmissionIds fails" in {
      mockGetAllPeriodicSubmission(
        2024,
        "A34324",
        "Rental",
        Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("code", "error")))
      )
      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Left(
        ApiServiceError(500)
      )
    }
  }

  val validPropertyPeriodicSubmissionRequest = PropertyPeriodicSubmissionRequest(
    Some(
      ForeignFhlEea(
        ForeignFhlIncome(200.00),
        ForeignFhlExpenses(None, None, None, None, None, None, None, Some(1000.99))
      )
    ),
    None,
    None,
    None
  )

  "create periodic submission" should {

    "return submissionId for valid request" in {
      val periodicSubmissionId = PeriodicSubmissionId("submissionId")
      val taxYear = 2024

      mockCreatePeriodicSubmission(taxYear, nino, incomeSourceId, Right(Some(periodicSubmissionId)))

      await(
        underTest.createPeriodicSubmission(nino, incomeSourceId, taxYear, validPropertyPeriodicSubmissionRequest).value
      ) shouldBe
        Right(Some(periodicSubmissionId))
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024
      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest.createPeriodicSubmission(nino, incomeSourceId, taxYear, validPropertyPeriodicSubmissionRequest).value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }

  }

  "update periodic submission" should {

    "return no content for valid request" in {
      val taxYear = 2024

      mockUpdatePeriodicSubmission(taxYear, nino, incomeSourceId, submissionId, Right(None))

      await(
        underTest
          .updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, validPropertyPeriodicSubmissionRequest)
          .value
      ) shouldBe
        Right("")
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024
      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        submissionId,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest
          .updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, validPropertyPeriodicSubmissionRequest)
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }

  }

  "delete annual submission" should {

    "return no content for valid request" in {
      val taxYear = 2024
      mockDeleteAnnualSubmissions(incomeSourceId, taxableEntityId, taxYear, Right(None))

      await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear).value) shouldBe
        Right(())
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024
      mockDeleteAnnualSubmissions(
        incomeSourceId,
        taxableEntityId,
        taxYear,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear).value) shouldBe Left(
        ApiServiceError(BAD_REQUEST)
      )
    }
  }

  "create annual submission" should {
    val validRequest = PropertyAnnualSubmission(
      submittedOn = Some(LocalDateTime.now),
      Some(
        AnnualForeignFhlEea(
          ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
          ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
        )
      ),
      None,
      None,
      None
    )

    "return no content for valid request" in {
      val taxYear = 2024
      mockCreateAnnualSubmission2(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), Right())
      await(
        underTest
          .createOrUpdateAnnualSubmission(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), validRequest)
          .value
      ) shouldBe
        Right()
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024

      mockCreateAnnualSubmission2(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )

      await(
        underTest
          .createOrUpdateAnnualSubmission(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), validRequest)
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "create annual submission 2" should {
    val validRequestBody = PropertyAnnualSubmission(
      submittedOn = Some(LocalDateTime.now),
      Some(
        AnnualForeignFhlEea(
          ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
          ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
        )
      ),
      None,
      None,
      None
    )

    "return no content for valid request" in {
      val taxYear = 2024
      mockCreateAnnualSubmission2(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), Right())
      await(
        underTest
          .createOrUpdateAnnualSubmission(
            TaxYear(taxYear),
            IncomeSourceId(incomeSourceId),
            Nino(nino),
            validRequestBody
          )
          .value
      ) shouldBe
        Right()
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024
      mockCreateAnnualSubmission2(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest
          .createOrUpdateAnnualSubmission(
            TaxYear(taxYear),
            IncomeSourceId(incomeSourceId),
            Nino(nino),
            validRequestBody
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save property rental adjustments" should {

    val taxYear = 2024
    val mtditid = "89787469409"
    val journeyContextWithNino =
      JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))
    val propertyRentalAdjustments = PropertyRentalAdjustments(
      BigDecimal(12.34),
      BalancingCharge(balancingChargeYesNo = true, Some(108)),
      BigDecimal(34.56),
      RenovationAllowanceBalancingCharge(
        renovationAllowanceBalancingChargeYesNo = true,
        renovationAllowanceBalancingChargeAmount = Some(92)
      ),
      BigDecimal(56.78),
      BigDecimal(78.89)
    )

    "return a success with no content when the request is valid and data is persisted" in {
      val annualUkOtherProperty =
        AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(44), None, None, None, None, None)), None)

      val annualSubmission = PropertyAnnualSubmission(None, None, None, None, Some(annualUkOtherProperty))
      val updatedAnnualSubmission = PropertyAnnualSubmission(
        None,
        None,
        None,
        None,
        Some(
          AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(44), Some(108), Some(12.34), Some(92), None, None)), None)
        )
      )
      mockGetPropertyAnnualSubmission(taxYear, nino, incomeSourceId, Some(annualSubmission).asRight[ApiError])
      mockCreateAnnualSubmission(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Some(updatedAnnualSubmission),
        Right()
      )
      await(
        underTest.savePropertyRentalAdjustments(journeyContextWithNino, propertyRentalAdjustments).value
      ) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024

      val annualUkOtherProperty =
        AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(44), None, None, None, None, None)), None)
      val annualSubmission = PropertyAnnualSubmission(None, None, None, None, Some(annualUkOtherProperty))
      val updatedAnnualSubmission = PropertyAnnualSubmission(
        None,
        None,
        None,
        None,
        Some(
          AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(44), Some(108), Some(12.34), Some(92), None, None)), None)
        )
      )
      mockGetPropertyAnnualSubmission(taxYear, nino, incomeSourceId, Some(annualSubmission).asRight[ApiError])
      mockCreateAnnualSubmission(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Some(updatedAnnualSubmission),
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest.savePropertyRentalAdjustments(journeyContextWithNino, propertyRentalAdjustments).value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save property rental allowances" should {

    val taxYear = 2024
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))
    val allowances = RentalAllowances(
      Some(11),
      ElectricChargePointAllowance(electricChargePointAllowanceYesOrNo = true, Some(11)),
      Some(11),
      Some(11),
      Some(11),
      Some(11),
      Some(11)
    )
    "return no content for valid request" in {
      mockCreateAnnualSubmission2(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), Right())
      await(underTest.savePropertyRentalAllowances(ctx, allowances).value) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {
      mockCreateAnnualSubmission2(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(underTest.savePropertyRentalAllowances(ctx, allowances).value) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save esba" should {

    val taxYear = 2024
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))
    val allowances = RentalAllowances(
      Some(11),
      ElectricChargePointAllowance(electricChargePointAllowanceYesOrNo = true, Some(11)),
      Some(11),
      Some(11),
      Some(11),
      Some(11),
      Some(11)
    )
    "return no content for valid request" in {
      mockCreateAnnualSubmission2(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), Right())
      await(underTest.savePropertyRentalAllowances(ctx, allowances).value) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {
      mockCreateAnnualSubmission2(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(underTest.savePropertyRentalAllowances(ctx, allowances).value) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }
  "save income" should {

    val taxYear = 2024
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))

    val incomeToSave: Income = Income(
      false,
      25,
      true,
      ReversePremiumsReceived(true),
      None,
      None,
      None,
      None
    )

    val ukOtherPropertyIncome = UkOtherPropertyIncome(
      None,
      None,
      None,
      None,
      None,
      None
    )

    val ukOtherPropertyExpenses =
      UkOtherPropertyExpenses(Some(100), None, None, None, None, None, None, None, None, None, None)
    val saveIncome = SaveIncome(ukOtherPropertyIncome, incomeToSave)
    // Todo: Property Based Test Required for Update Case
    "return no content for valid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)
      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("", fromDate, toDate)).asRight[ApiError]
      )

      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(PeriodicSubmissionId("")).asRight[ApiError]
      )

      await(
        underTest
          .saveIncome(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            Nino(nino),
            incomeToSave,
            saveIncome
          )
          .value
      ) shouldBe Right(Some(PeriodicSubmissionId("")))
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

      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        ApiError(BAD_REQUEST, SingleErrorBody("code", "error")).asLeft[Option[PeriodicSubmissionId]]
      )
      await(
        underTest
          .saveIncome(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            Nino(nino),
            incomeToSave,
            saveIncome
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save expenses" should {
    // Todo: Property Based Test Required for Create Case
    val taxYear = 2024
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid("mtditid"), Nino(nino))
      .toJourneyContext(JourneyName.RentalExpenses)

    "return submissionId" in {

      val ukOtherPropertyIncome = UkOtherPropertyIncome(
        Some(0),
        None,
        None,
        None,
        None,
        None
      )
      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        None,
        None,
        LocalDate.now().minusMonths(2),
        LocalDate.now().plusMonths(1),
        None,
        None,
        None,
        Some(
          UkOtherProperty(
            Some(ukOtherPropertyIncome),
            Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
          )
        )
      )
      val fromDate = LocalDate.now().minusYears(2)
      val toDate = fromDate.plusYears(3)

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("1", fromDate, toDate)).asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        Some(propertyPeriodicSubmission).asRight[ApiError]
      )

      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        Some("1").asRight[ApiError]
      )

      await(
        underTest
          .saveExpenses(
            ctx,
            Nino(nino),
            Expenses(
              None,
              Some(200),
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None
            )
          )
          .value
      ) shouldBe Some(PeriodicSubmissionId("1")).asRight[ServiceError]

    }

    "downstream error when getAllPeriodicSubmission call fails" in {

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        ApiError(500, SingleErrorBody("500", "")).asLeft[List[PeriodicSubmissionIdModel]]
      )

      val result = underTest.saveExpenses(
        ctx,
        Nino(nino),
        Expenses(
          None,
          Some(200),
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None
        )
      )
      whenReady(result.value) { r =>
        r mustBe ApiServiceError(500).asLeft[Option[SubmissionId]]
      }
    }
    "downstream error when getPropertyPeriodicSubmission call fails" in {
      val fromDate = LocalDate.now().minusYears(2)
      val toDate = fromDate.plusYears(3)

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("1", fromDate, toDate)).asRight[ApiError]
      )
      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        ApiError(500, SingleErrorBody("500", "")).asLeft[Option[PropertyPeriodicSubmission]]
      )
      val result = underTest.saveExpenses(
        ctx,
        Nino(nino),
        Expenses(
          None,
          Some(200),
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None
        )
      )
      whenReady(result.value) { r =>
        r mustBe ApiServiceError(500).asLeft[Option[SubmissionId]]
      }
    }
    // Todo: PropertyTests for CreatePeriodicSubmission case too.
    "downstream error when create call fails" in {
      val fromDate = LocalDate.now().minusYears(2)
      val toDate = fromDate.plusYears(3)
      val ukOtherPropertyIncome = UkOtherPropertyIncome(
        Some(0),
        None,
        None,
        None,
        None,
        None
      )
      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        None,
        None,
        LocalDate.now().minusMonths(2),
        LocalDate.now().plusMonths(1),
        None,
        None,
        None,
        Some(
          UkOtherProperty(
            Some(ukOtherPropertyIncome),
            Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
          )
        )
      )
      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("1", fromDate, toDate)).asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        Some(propertyPeriodicSubmission).asRight[ApiError]
      )

      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        ApiError(500, SingleErrorBody("500", "reason")).asLeft[Option[String]]
      )

      val result = underTest.saveExpenses(
        ctx,
        Nino(nino),
        Expenses(
          None,
          Some(200),
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None
        )
      )
      whenReady(result.value) { r =>
        r mustBe ApiServiceError(500).asLeft[Option[SubmissionId]]
      }
    }
  }
  "fetch" should {
    val taxYear = 2024
    val mtditid = "1234567890"

    val esbaDate = LocalDate.parse("2024-01-01")
    val qualifyingAmountExpenditure = 35
    val amount = 25
    val address1 = "name1"
    val address2 = "name2"
    val postcode = "AB1 XY2"
    val aPropertyAnnualSubmission = PropertyAnnualSubmission(
      submittedOn = Some(LocalDateTime.now),
      None,
      None,
      None,
      Some(
        AnnualUkOtherProperty(
          None,
          Some(
            UkOtherAllowances(
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              Some(
                Seq(
                  Esba(
                    amount,
                    Some(
                      StructuredBuildingAllowanceDate(esbaDate, qualifyingAmountExpenditure)
                    ),
                    StructuredBuildingAllowanceBuilding(
                      Some(address1),
                      Some(address2),
                      postcode
                    )
                  )
                )
              ),
              None,
              None
            )
          )
        )
      )
    )
    "return successful with Esba Info fetched" in {
      val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))

      def generateEsbaInfo(
        claimEnhancedStructureBuildingAllowance: ClaimEnhancedStructureBuildingAllowance,
        esbaClaims: EsbaClaims
      ): EsbaInfo =
        EsbaInfo(
          claimEnhancedStructureBuildingAllowance,
          esbaClaims,
          List(
            EsbaInUpstream(
              esbaDate,
              qualifyingAmountExpenditure,
              amount,
              Address(
                BuildingName(address1),
                BuildingNumber(address2),
                Postcode(postcode)
              )
            )
          )
        )

      val scenarios = Table[Boolean, ClaimEnhancedStructureBuildingAllowance, EsbaClaims, Option[EsbaInfo]](
        ("isJourneyPresentInDb", "ClaimEnhancedStructureBuildingAllowance", "EsbaClaims", "EsbaInfoRetrieved"),
        (
          true,
          ClaimEnhancedStructureBuildingAllowance(true),
          EsbaClaims(true),
          Some(
            generateEsbaInfo(ClaimEnhancedStructureBuildingAllowance(true), EsbaClaims(true))
          )
        ),
        (
          true,
          ClaimEnhancedStructureBuildingAllowance(true),
          EsbaClaims(false),
          Some(
            generateEsbaInfo(ClaimEnhancedStructureBuildingAllowance(true), EsbaClaims(false))
          )
        ),
        (
          true,
          ClaimEnhancedStructureBuildingAllowance(false),
          EsbaClaims(true),
          Some(
            generateEsbaInfo(ClaimEnhancedStructureBuildingAllowance(false), EsbaClaims(true))
          )
        ),
        (
          true,
          ClaimEnhancedStructureBuildingAllowance(false),
          EsbaClaims(false),
          Some(
            generateEsbaInfo(
              ClaimEnhancedStructureBuildingAllowance(false),
              EsbaClaims(false)
            )
          )
        ),
        (
          false,
          ClaimEnhancedStructureBuildingAllowance(true),
          EsbaClaims(false),
          Some(
            generateEsbaInfo(
              ClaimEnhancedStructureBuildingAllowance(true),
              EsbaClaims(false)
            )
          )
        )
      )
      forAll(scenarios) {
        (
          isJourneyPresentInDb: Boolean,
          claimEnhancedStructureBuildingAllowance: ClaimEnhancedStructureBuildingAllowance,
          esbaClaims: EsbaClaims,
          esbaInfoRetrieved: Option[EsbaInfo]
        ) =>
          mockGetPropertyAnnualSubmission(
            taxYear,
            "A34324",
            incomeSourceId,
            Some(
              aPropertyAnnualSubmission
            ).asRight[ApiError]
          )

          val result: EitherT[Future, ServiceError, FetchedPropertyData] = for {
            _ <- if (isJourneyPresentInDb) {
                   EitherT(
                     repository
                       .upsertAnswers(
                         ctx.toJourneyContext(JourneyName.RentalESBA),
                         Json.toJson(
                           EsbaInfoToSave(claimEnhancedStructureBuildingAllowance, esbaClaims)
                         )
                       )
                       .map(_.asRight[ServiceError])
                   )
                 } else {
                   EitherT(
                     testOnlyRemove(repository, ctx.toJourneyContext(JourneyName.AllJourneys))
                       .map(_.asRight[ServiceError])
                   )
                 }

            r <- underTest.getFetchedPropertyDataMerged(
                   ctx.toJourneyContext(JourneyName.AllJourneys),
                   Nino(nino),
                   incomeSourceId
                 )
          } yield r
          whenReady(result.value, Timeout(Span(500, Millis))) { response =>
            response shouldBe FetchedPropertyData(None, None, None, esbaInfoRetrieved).asRight[ServiceError]
          }
      }

      def testOnlyRemove(
        mongoJourneyAnswersRepository: MongoJourneyAnswersRepository,
        ctx: JourneyContext
      ): Future[Unit] = {
        val filter: Bson = Filters
          .and(
            Filters.equal("incomeSourceId", ctx.incomeSourceId.value),
            Filters.equal("taxYear", ctx.taxYear.endYear),
            Filters.equal("mtditid", ctx.mtditid.value)
          )
        // Todo: How is this indexed?
        mongoJourneyAnswersRepository.collection.deleteMany(filter).toFuture().map(_ => ())
      }
    }
    "return Repo Error for wrong Journey Type" in {

      val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))

      val claimEnhancedStructureBuildingAllowance = ClaimEnhancedStructureBuildingAllowance(true)
      val esbaClaims = EsbaClaims(true)

      mockGetPropertyAnnualSubmission(
        taxYear,
        "A34324",
        incomeSourceId,
        Some(aPropertyAnnualSubmission).asRight[ApiError]
      )

      val result: EitherT[Future, ServiceError, FetchedPropertyData] = for {
        _ <- EitherT(
               repository
                 .upsertAnswers(
                   ctx.toJourneyContext(JourneyName.RentalESBA),
                   Json.toJson(
                     EsbaInfoToSave(claimEnhancedStructureBuildingAllowance, esbaClaims)
                   )
                 )
                 .map(_.asRight[ServiceError])
             )
        r <-
          underTest
            .getFetchedPropertyDataMerged(ctx.toJourneyContext(JourneyName.NoJourney), Nino(nino), incomeSourceId)
      } yield r
      whenReady(result.value, Timeout(Span(500, Millis))) { response =>
        response shouldBe InternalError("Journey Repo 'should' not be accessed, journey name: no-journey")
          .asLeft[FetchedPropertyData]

      }

    }
    "return ApiError for invalid request" in {
      val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))
      mockGetPropertyAnnualSubmission(
        taxYear,
        "A34324",
        incomeSourceId,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      val result: EitherT[Future, ServiceError, FetchedPropertyData] = for {
        _ <- EitherT(
               repository
                 .upsertAnswers(
                   ctx.toJourneyContext(JourneyName.RentalESBA),
                   Json.toJson(EsbaInfoToSave(ClaimEnhancedStructureBuildingAllowance(true), EsbaClaims(false)))
                 )
                 .map(_.asRight[ServiceError])
             )
        r <-
          underTest
            .getFetchedPropertyDataMerged(ctx.toJourneyContext(JourneyName.RentalESBA), Nino(nino), incomeSourceId)
      } yield r

      whenReady(result.value, Timeout(Span(500, Millis))) { response =>
        response shouldBe Left(ApiServiceError(BAD_REQUEST))
      }
    }
    "return ServiceError when repo has per key more than one entry" in {
      def testOnlyRemove(
        mongoJourneyAnswersRepository: MongoJourneyAnswersRepository,
        ctx: JourneyContext
      ): Future[Unit] = {
        val filter: Bson = Filters
          .and(
            Filters.equal("incomeSourceId", ctx.incomeSourceId.value),
            Filters.equal("taxYear", ctx.taxYear.endYear),
            Filters.equal("mtditid", ctx.mtditid.value)
          )
        // Todo: How is this indexed?
        mongoJourneyAnswersRepository.collection.deleteMany(filter).toFuture().map(_ => ())
      }

      def testOnlyAdd(
        clock: Clock,
        mongoJourneyAnswersRepository: MongoJourneyAnswersRepository,
        ctx: JourneyContext,
        newData: JsObject
      ) = {

        val now = clock.instant()
        val expireAt = calculateExpireAt(now.plusSeconds(1000))

        mongoJourneyAnswersRepository.collection
          .insertOne(
            JourneyAnswers(
              ctx.mtditid,
              ctx.incomeSourceId,
              ctx.taxYear,
              ctx.journey,
              JourneyStatus.NotStarted,
              newData,
              expireAt,
              now,
              now
            )
          )
          .toFuture()
          .map(_ => true)
      }

      val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))
      mockGetPropertyAnnualSubmission(
        taxYear,
        "A34324",
        incomeSourceId,
        Some(
          aPropertyAnnualSubmission
        ).asRight[ApiError]
      )
      val result: EitherT[Future, ServiceError, FetchedPropertyData] = for {
        _ <- EitherT(
               testOnlyRemove(repository, ctx.toJourneyContext(JourneyName.AllJourneys)).map(_.asRight[ServiceError])
             )
        _ <- EitherT(
               testOnlyAdd(
                 Clock.systemUTC(),
                 repository,
                 ctx.toJourneyContext(JourneyName.RentalESBA),
                 Json.toJsObject(EsbaInfoToSave(ClaimEnhancedStructureBuildingAllowance(true), EsbaClaims(false)))
               ).map(_.asRight[ServiceError])
             )
        _ <- EitherT(
               testOnlyAdd(
                 Clock.systemUTC(),
                 repository,
                 ctx.toJourneyContext(JourneyName.RentalESBA),
                 Json.toJsObject(EsbaInfoToSave(ClaimEnhancedStructureBuildingAllowance(true), EsbaClaims(false)))
               ).map(_.asRight[ServiceError])
             )
        r <-
          underTest
            .getFetchedPropertyDataMerged(ctx.toJourneyContext(JourneyName.AllJourneys), Nino(nino), incomeSourceId)
      } yield r

      whenReady(result.value, Timeout(Span(1500, Millis))) { response =>
        response shouldBe Left(RepositoryError)
      }
    }
  }
}
