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
import models.common._
import models.domain.JourneyAnswers
import models.errors._
import models.request._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba.EsbaInfoExtensions.EsbaExtensions
import models.request.esba._
import models.request.sba.SbaInfoExtensions.SbaExtensions
import models.request.sba.{Sba, SbaInfo}
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import models.{PropertyPeriodicSubmissionResponse, RentalsAndRaRAbout}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper
import org.scalatest.time.{Millis, Span}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR}
import play.api.libs.json.{JsObject, Json}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.mocks.{MockIntegrationFrameworkConnector, MockMergeService, MockMongoJourneyAnswersRepository}
import utils.{AppConfigStub, UnitTest}

import java.time.{Clock, LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PropertyServiceSpec
    extends UnitTest with MockIntegrationFrameworkConnector with MockMongoJourneyAnswersRepository with MockMergeService
    with HttpClientSupport with ScalaCheckPropertyChecks {
  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()

  private val underTest = new PropertyService(mergeService, mockIntegrationFrameworkConnector, repository)
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

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and there is no submission" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )
    }

    "return DataNotFoundError when GetPeriodicSubmission does not have ids" in {
      val aPeriodicSubmissionModel = List.empty[PeriodicSubmissionIdModel]

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )

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
        None,
        Some(
          AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(1), Some(2), Some(3), Some(4), Some(true), None)), None)
        )
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

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId).value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and there is no submission" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )
    }

    "return DataNotFoundError when GetPeriodicSubmission does not have ids" in {
      val aPeriodicSubmissionModel = List.empty[PeriodicSubmissionIdModel]

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental").value) shouldBe Right(
        PropertyPeriodicSubmissionResponse(List())
      )

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

  val validCreatePropertyPeriodicSubmissionRequest = CreatePropertyPeriodicSubmissionRequest(
    LocalDate.now(),
    LocalDate.now(),
    None,
    Some(
      UkOtherProperty(
        Some(UkOtherPropertyIncome(Some(200.00), Some(200.00), Some(200.00), Some(200.00), Some(200.00), None)),
        None
      )
    )
  )
  val validUpdatePropertyPeriodicSubmissionRequest = UpdatePropertyPeriodicSubmissionRequest(
    None,
    Some(
      UkOtherProperty(
        Some(UkOtherPropertyIncome(Some(200.00), Some(200.00), Some(200.00), Some(200.00), Some(200.00), None)),
        None
      )
    )
  )

  val propertyPeriodicSubmission = PropertyPeriodicSubmission(
    None,
    None,
    LocalDate.now(),
    LocalDate.now(),
    None,
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
      val taxYear = 2024

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
      val taxYear = 2024

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

  "update periodic submission" should {

    "return no content for valid request" in {
      val taxYear = 2024

      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        validUpdatePropertyPeriodicSubmissionRequest,
        submissionId,
        Right(None)
      )

      await(
        underTest
          .updatePeriodicSubmission(
            nino,
            incomeSourceId,
            taxYear,
            submissionId,
            validUpdatePropertyPeriodicSubmissionRequest
          )
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
        validUpdatePropertyPeriodicSubmissionRequest,
        submissionId,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest
          .updatePeriodicSubmission(
            nino,
            incomeSourceId,
            taxYear,
            submissionId,
            validUpdatePropertyPeriodicSubmissionRequest
          )
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
      None,
      Some(
        AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(1), Some(2), Some(3), Some(4), Some(true), None)), None)
      )
    )

    "return no content for valid request" in {
      val taxYear = 2024
      mockCreateAnnualSubmission(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), Right())
      await(
        underTest
          .createOrUpdateAnnualSubmission(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), validRequest)
          .value
      ) shouldBe
        Right()
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024

      mockCreateAnnualSubmission(
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
      None,
      Some(
        AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(1), Some(2), Some(3), Some(4), Some(true), None)), None)
      )
    )

    "return no content for valid request" in {
      val taxYear = 2024
      mockCreateAnnualSubmission(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), Right())
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
      mockCreateAnnualSubmission(
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
    val journeyContext = journeyContextWithNino.toJourneyContext(JourneyName.RentalAdjustments)
    val propertyRentalAdjustments = PropertyRentalAdjustments(
      BigDecimal(12.34),
      BalancingCharge(balancingChargeYesNo = true, Some(108)),
      Some(BigDecimal(34.56)),
      RenovationAllowanceBalancingCharge(
        renovationAllowanceBalancingChargeYesNo = true,
        renovationAllowanceBalancingChargeAmount = Some(92)
      ),
      BigDecimal(56.78),
      Some(BigDecimal(78.89))
    )

    "return a success with no content when the request is valid and data is persisted" in {
      val annualUkOtherProperty =
        AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(44), None, None, None, None, None)), None)

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(
          PeriodicSubmissionIdModel(
            "",
            LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
            LocalDate.parse(TaxYear.endDate(TaxYear(taxYear)))
          )
        )
          .asRight[ApiError]
      )

      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
          LocalDate.parse(TaxYear.endDate(TaxYear(taxYear))),
          None,
          None
        )

      val requestForCreate: CreatePropertyPeriodicSubmissionRequest =
        CreatePropertyPeriodicSubmissionRequest.fromPropertyRentalAdjustments(
          TaxYear(taxYear),
          Some(emptyPeriodicSubmission),
          propertyRentalAdjustments
        )
      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        Some(PeriodicSubmissionId("")).asRight[ApiError]
      )

      val annualSubmission = PropertyAnnualSubmission(None, None, Some(annualUkOtherProperty))
      val updatedAnnualSubmission = PropertyAnnualSubmission(
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
        underTest.savePropertyRentalAdjustments(journeyContext, Nino(nino), propertyRentalAdjustments).value
      ) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024

      val annualUkOtherProperty =
        AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(44), None, None, None, None, None)), None)
      val annualSubmission = PropertyAnnualSubmission(None, None, Some(annualUkOtherProperty))
      val updatedAnnualSubmission = PropertyAnnualSubmission(
        None,
        None,
        Some(
          AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(44), Some(108), Some(12.34), Some(92), None, None)), None)
        )
      )

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(
          PeriodicSubmissionIdModel(
            "",
            LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
            LocalDate.parse(TaxYear.endDate(TaxYear(taxYear)))
          )
        )
          .asRight[ApiError]
      )

      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
          LocalDate.parse(TaxYear.endDate(TaxYear(taxYear))),
          None,
          None
        )

      val requestForCreate: CreatePropertyPeriodicSubmissionRequest =
        CreatePropertyPeriodicSubmissionRequest.fromPropertyRentalAdjustments(
          TaxYear(taxYear),
          Some(emptyPeriodicSubmission),
          propertyRentalAdjustments
        )
      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        Some(PeriodicSubmissionId("")).asRight[ApiError]
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
        underTest.savePropertyRentalAdjustments(journeyContext, Nino(nino), propertyRentalAdjustments).value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save property rental allowances" should {

    val taxYear = 2024
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))
    val allowances = RentalAllowances(
      Some(11),
      Some(ElectricChargePointAllowance(electricChargePointAllowanceYesOrNo = true, Some(11))),
      Some(11),
      Some(11),
      Some(11),
      Some(11),
      Some(11)
    )
    val annualSubmission = createAnnualSubmission(None, None)
    "return no content for valid request" in {
      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )
      mockCreateAnnualSubmission(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Nino(nino), Right())
      await(underTest.savePropertyRentalAllowances(ctx, allowances).value) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {
      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )
      mockCreateAnnualSubmission(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(underTest.savePropertyRentalAllowances(ctx, allowances).value) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  def createAnnualSubmission(
    sbasMaybe: Option[List[StructuredBuildingAllowance]],
    esbasMaybe: Option[List[Esba]]
  ): PropertyAnnualSubmission =
    PropertyAnnualSubmission(
      None,
      None,
      Some(
        AnnualUkOtherProperty(
          Some(
            UkOtherAdjustments(
              Some(12.34),
              None,
              None,
              None,
              None,
              None
            )
          ),
          Some(
            UkOtherAllowances(
              None,
              None,
              None,
              None,
              None,
              None,
              sbasMaybe,
              esbasMaybe,
              Some(34.56),
              None
            )
          )
        )
      )
    )

  "save esbas" should {
    "call service method" in {

      val taxYear = 2024
      val mtditid = "1234567890"
      val ctx =
        JourneyContext(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), JourneyName.RentalESBA)

      val esbasToBeAdded = List(
        EsbaInUpstream(
          LocalDate.now(),
          12.34,
          56.78,
          Address(
            BuildingName("Building Name"),
            BuildingNumber("12"),
            Postcode("AB1 2CD")
          )
        )
      )

      val esbaInfo = EsbaInfo(
        claimEnhancedStructureBuildingAllowance = true,
        esbaClaims = Some(true),
        esbasToBeAdded
      )
      val annualSubmissionWithoutEsbas = createAnnualSubmission(None, None)

      val annualSubmissionAfterAdditionOfEsbas = createAnnualSubmission(None, Some(esbaInfo.toEsba))
      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmissionWithoutEsbas).asRight[ApiError]
      )

      mockCreateAnnualSubmission(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Some(annualSubmissionAfterAdditionOfEsbas),
        ().asRight[ApiError]
      )

      await(underTest.saveEsbas(ctx, Nino(nino), esbaInfo).value) shouldBe Right(())

    }
  }

  "save sbas" should {
    "call service method" in {

      val taxYear = 2024
      val mtditid = "1234567890"
      val ctx =
        JourneyContext(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), JourneyName.RentalESBA)

      val sbasToBeAdded = List(
        Sba(
          LocalDate.now(),
          12.34,
          56.78,
          Address(
            BuildingName("Building Name"),
            BuildingNumber("12"),
            Postcode("AB1 2CD")
          )
        )
      )

      val sbaInfo = SbaInfo(
        claimStructureBuildingAllowance = true,
        sbasToBeAdded
      )

      val annualSubmissionWithoutEsbas = createAnnualSubmission(None, None)

      val annualSubmissionAfterAdditionOfEsbas =
        createAnnualSubmission(Some(sbaInfo.toSba.toList), None)

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmissionWithoutEsbas).asRight[ApiError]
      )

      mockCreateAnnualSubmission(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Some(annualSubmissionAfterAdditionOfEsbas),
        ().asRight[ApiError]
      )

      await(underTest.saveSBA(ctx, Nino(nino), sbaInfo).value) shouldBe Right(())

    }
  }

  "save income" should {

    val taxYear = 2024
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))

    val propertyRentalsIncome = PropertyRentalsIncome(
      true,
      12.34,
      13.46,
      Some(DeductingTax(true, Some(14.51))),
      Some(CalculatedFigureYourself(true, Some(14.75))),
      Some(98.78),
      Some(64.23),
      Some(PremiumsGrantLease(true, Some(93.85))),
      Some(ReversePremiumsReceived(true, Some(913.84)))
    )
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
          LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
          LocalDate.parse(TaxYear.endDate(TaxYear(taxYear))),
          None,
          None
        )

      val Right(requestForCreate: CreatePropertyPeriodicSubmissionRequest) =
        CreatePropertyPeriodicSubmissionRequest.fromPropertyRentalsIncome(
          TaxYear(taxYear),
          Some(emptyPeriodicSubmission),
          propertyRentalsIncome
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
          .saveIncome(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            Nino(nino),
            propertyRentalsIncome
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
      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
          LocalDate.parse(TaxYear.endDate(TaxYear(taxYear))),
          None,
          None
        )

      val Right(requestForCreate: CreatePropertyPeriodicSubmissionRequest) =
        CreatePropertyPeriodicSubmissionRequest.fromPropertyRentalsIncome(
          TaxYear(taxYear),
          Some(emptyPeriodicSubmission),
          propertyRentalsIncome
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
          .saveIncome(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            Nino(nino),
            propertyRentalsIncome
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save expenses" should {

    val taxYear = 2024
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid("mtditid"), Nino(nino))
      .toJourneyContext(JourneyName.RentalExpenses)

    "return submissionId" in {
      val expenses = Expenses(
        None,
        Some(200),
        None,
        None,
        None,
        None,
        None,
        None
      )
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
      val Right(expensesForUpdate) =
        UpdatePropertyPeriodicSubmissionRequest.fromExpenses(Some(propertyPeriodicSubmission), expenses)
      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        expensesForUpdate,
        "1",
        Some("1").asRight[ApiError]
      )

      await(
        underTest
          .saveExpenses(
            ctx,
            Nino(nino),
            expenses
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
          None
        )
      )
      whenReady(result.value) { r =>
        r mustBe ApiServiceError(500).asLeft[Option[SubmissionId]]
      }
    }

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
        Some(
          UkOtherProperty(
            Some(ukOtherPropertyIncome),
            Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
          )
        )
      )

      val expenses = Expenses(
        None,
        Some(200),
        None,
        None,
        None,
        None,
        None,
        None
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

      val Right(expensesForUpdate) =
        UpdatePropertyPeriodicSubmissionRequest.fromExpenses(Some(propertyPeriodicSubmission), expenses)
      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        expensesForUpdate,
        "1",
        ApiError(500, SingleErrorBody("500", "reason")).asLeft[Option[String]]
      )

      val result = underTest.saveExpenses(
        ctx,
        Nino(nino),
        expenses
      )
      whenReady(result.value) { r =>
        r mustBe ApiServiceError(500).asLeft[Option[SubmissionId]]
      }
    }
  }

  "save rent a room expenses" should {

    val taxYear = 2024
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid("mtditid"), Nino(nino))
      .toJourneyContext(JourneyName.RentalExpenses)

    "return submissionId when creating" in {

      val ukOtherPropertyIncome = UkOtherPropertyIncome(
        Some(0),
        None,
        None,
        None,
        None,
        None
      )
      val raRExpenses = RentARoomExpenses(
        None,
        Some(200),
        None,
        None,
        None,
        None
      )
      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        None,
        None,
        LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
        LocalDate.parse(TaxYear.endDate(TaxYear(taxYear))),
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
        Nil.asRight[ApiError]
      )

      val Right(raRExpensesForCreate: CreatePropertyPeriodicSubmissionRequest) =
        CreatePropertyPeriodicSubmissionRequest.fromRaRExpenses(
          TaxYear(taxYear),
          None,
          raRExpenses
        )
      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        raRExpensesForCreate,
        Some(PeriodicSubmissionId("1")).asRight[ApiError]
      )

      await(
        underTest
          .saveRaRExpenses(
            ctx,
            Nino(nino),
            raRExpenses
          )
          .value
      ) shouldBe Some(PeriodicSubmissionId("1")).asRight[ServiceError]

    }

    "return submissionId when updating" in {

      val ukOtherPropertyIncome = UkOtherPropertyIncome(
        Some(0),
        None,
        None,
        None,
        None,
        None
      )
      val raRExpenses = RentARoomExpenses(
        None,
        Some(200),
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

      val Right(raRExpensesForUpdate) =
        UpdatePropertyPeriodicSubmissionRequest.fromRaRExpenses(Some(propertyPeriodicSubmission), raRExpenses)
      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        raRExpensesForUpdate,
        "1",
        Some("1").asRight[ApiError]
      )

      await(
        underTest
          .saveRaRExpenses(
            ctx,
            Nino(nino),
            raRExpenses
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
          None
        )
      )
      whenReady(result.value) { r =>
        r mustBe ApiServiceError(500).asLeft[Option[SubmissionId]]
      }
    }

    "downstream error when create call fails" in {
      val fromDate = LocalDate.now().minusYears(2)
      val toDate = fromDate.plusYears(3)

      val raRExpenses = RentARoomExpenses(
        None,
        Some(200),
        None,
        None,
        None,
        None
      )
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

      val Right(raRExpensesForUpdate) =
        UpdatePropertyPeriodicSubmissionRequest.fromRaRExpenses(Some(propertyPeriodicSubmission), raRExpenses)

      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        raRExpensesForUpdate,
        "1",
        ApiError(500, SingleErrorBody("500", "reason")).asLeft[Option[String]]
      )

      val result = underTest.saveRaRExpenses(
        ctx,
        Nino(nino),
        raRExpenses
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
        claimEnhancedStructureBuildingAllowance: Boolean,
        esbaClaims: Boolean
      ): EsbaInfo =
        EsbaInfo(
          claimEnhancedStructureBuildingAllowance,
          Some(esbaClaims),
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

      val scenarios = Table[Boolean, Boolean, Boolean, Option[EsbaInfo]](
        ("isJourneyPresentInDb", "ClaimEnhancedStructureBuildingAllowance", "EsbaClaims", "EsbaInfoRetrieved"),
        (
          true,
          true,
          true,
          Some(
            generateEsbaInfo(claimEnhancedStructureBuildingAllowance = true, esbaClaims = true)
          )
        ),
        (
          true,
          true,
          false,
          Some(
            generateEsbaInfo(claimEnhancedStructureBuildingAllowance = true, esbaClaims = false)
          )
        ),
        (
          true,
          false,
          true,
          Some(
            generateEsbaInfo(claimEnhancedStructureBuildingAllowance = false, esbaClaims = true)
          )
        ),
        (
          true,
          false,
          false,
          Some(
            generateEsbaInfo(
              claimEnhancedStructureBuildingAllowance = false,
              esbaClaims = false
            )
          )
        ),
        (
          false,
          true,
          false,
          Some(
            generateEsbaInfo(
              claimEnhancedStructureBuildingAllowance = true,
              esbaClaims = false
            )
          )
        )
      )

      def createFetchedPropertyData(esbaInfoRetrieved: Option[EsbaInfo]) =
        FetchedPropertyData(
          None,
          None,
          None,
          None,
          Some(RentalAllowances(None, Some(ElectricChargePointAllowance(false, None)), None, None, None, None, None)),
          esbaInfoRetrieved,
          None,
          None,
          None,
          None,
          None,
          None,
          None
        )
      forAll(scenarios) {
        (
          isJourneyPresentInDb: Boolean,
          claimEnhancedStructureBuildingAllowance: Boolean,
          esbaClaims: Boolean,
          esbaInfoRetrieved: Option[EsbaInfo]
        ) =>
          mockGetPropertyAnnualSubmission(
            taxYear,
            nino,
            incomeSourceId,
            Some(
              aPropertyAnnualSubmission
            ).asRight[ApiError]
          )
          mockGetAllPeriodicSubmission(
            taxYear,
            nino,
            incomeSourceId,
            List(PeriodicSubmissionIdModel("1", LocalDate.now().minusYears(2), LocalDate.now().plusYears(2)))
              .asRight[ApiError]
          )
          val fetchedPropertyData = createFetchedPropertyData(esbaInfoRetrieved)
          mockMergeServiceMergeAll(fetchedPropertyData)
          mockGetPropertyPeriodicSubmission(
            taxYear,
            nino,
            incomeSourceId,
            "1",
            Some(propertyPeriodicSubmission).asRight[ApiError]
          )
          val result: EitherT[Future, ServiceError, FetchedPropertyData] = for {
            _ <- if (isJourneyPresentInDb) {
                   EitherT(
                     repository
                       .upsertAnswers(
                         ctx.toJourneyContext(JourneyName.RentalESBA),
                         Json.toJson(
                           EsbaInfoToSave(claimEnhancedStructureBuildingAllowance, Some(esbaClaims))
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
            _ <- EitherT(
                   testOnlyRemove(repository, ctx.toJourneyContext(JourneyName.AllJourneys))
                     .map(_.asRight[ServiceError])
                 )
          } yield r
          whenReady(result.value, Timeout(Span(500, Millis))) { response =>
            response shouldBe fetchedPropertyData.asRight[ServiceError]
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
        mongoJourneyAnswersRepository.collection.deleteMany(filter).toFuture().map(_ => ())
      }
    }

    "return Repo Error for wrong Journey Type" in {
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
        mongoJourneyAnswersRepository.collection.deleteMany(filter).toFuture().map(_ => ())
      }
      val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))

      val claimEnhancedStructureBuildingAllowance = true
      val esbaClaims = true

      mockGetPropertyAnnualSubmission(
        taxYear,
        "A34324",
        incomeSourceId,
        Some(aPropertyAnnualSubmission).asRight[ApiError]
      )
      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("1", LocalDate.now().minusYears(2), LocalDate.now().plusYears(2)))
          .asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        Some(propertyPeriodicSubmission).asRight[ApiError]
      )
      val result: EitherT[Future, ServiceError, FetchedPropertyData] = for {
        _ <- EitherT(
               testOnlyRemove(repository, ctx.toJourneyContext(JourneyName.AllJourneys))
                 .map(_.asRight[ServiceError])
             )
        _ <- EitherT(
               repository
                 .upsertAnswers(
                   ctx.toJourneyContext(JourneyName.RentalESBA),
                   Json.toJson(
                     EsbaInfoToSave(claimEnhancedStructureBuildingAllowance, Some(esbaClaims))
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
      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("1", LocalDate.now().minusYears(2), LocalDate.now().plusYears(2)))
          .asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        Some(propertyPeriodicSubmission).asRight[ApiError]
      )
      val result: EitherT[Future, ServiceError, FetchedPropertyData] = for {
        _ <- EitherT(
               repository
                 .upsertAnswers(
                   ctx.toJourneyContext(JourneyName.RentalESBA),
                   Json.toJson(EsbaInfoToSave(claimEnhancedStructureBuildingAllowance = true, esbaClaims = Some(false)))
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
        mongoJourneyAnswersRepository.collection.deleteMany(filter).toFuture().map(_ => ())
      }

      def testOnlyAdd(
        clock: Clock,
        mongoJourneyAnswersRepository: MongoJourneyAnswersRepository,
        ctx: JourneyContext,
        newData: JsObject
      ) = {

        val now = clock.instant()

        mongoJourneyAnswersRepository.collection
          .insertOne(
            JourneyAnswers(
              ctx.mtditid,
              ctx.incomeSourceId,
              ctx.taxYear,
              ctx.journey,
              JourneyStatus.NotStarted,
              newData,
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

      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("1", LocalDate.now().minusYears(2), LocalDate.now().plusYears(2)))
          .asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        Some(propertyPeriodicSubmission).asRight[ApiError]
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
                 Json.toJsObject(
                   EsbaInfoToSave(claimEnhancedStructureBuildingAllowance = true, esbaClaims = Some(false))
                 )
               ).map(_.asRight[ServiceError])
             )
        _ <- EitherT(
               testOnlyAdd(
                 Clock.systemUTC(),
                 repository,
                 ctx.toJourneyContext(JourneyName.RentalESBA),
                 Json.toJsObject(
                   EsbaInfoToSave(claimEnhancedStructureBuildingAllowance = true, esbaClaims = Some(false))
                 )
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

  "save uk rent a room about" should {

    val taxYear = 2024
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))

    val ukRaRAbout = RaRAbout(
      true,
      55.22,
      ClaimExpensesOrRelief(
        true,
        Some(22.55)
      )
    )
    val annualSubmission = PropertyAnnualSubmission(None, None, None)

    "return no content for valid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)
      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("", fromDate, toDate)).asRight[ApiError]
      )

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )

      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
          LocalDate.parse(TaxYear.endDate(TaxYear(taxYear))),
          None,
          None
        )

      val Right(requestForCreate: CreatePropertyPeriodicSubmissionRequest) =
        CreatePropertyPeriodicSubmissionRequest.fromUkRaRAbout(
          TaxYear(taxYear),
          Some(emptyPeriodicSubmission),
          ukRaRAbout
        )

      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        Some(PeriodicSubmissionId("")).asRight[ApiError]
      )

      mockCreateAnnualSubmission(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Some(
          annualSubmission.copy(ukOtherProperty =
            Some(
              AnnualUkOtherProperty(
                Some(
                  UkOtherAdjustments(
                    None,
                    None,
                    None,
                    None,
                    Some(false),
                    Some(UkRentARoom(ukRaRAbout.jointlyLetYesOrNo))
                  )
                ),
                None
              )
            )
          )
        ),
        ().asRight[ApiError]
      )
      await(
        underTest
          .saveRaRAbout(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            Nino(nino),
            ukRaRAbout
          )
          .value
      ) shouldBe Right(true)
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
          LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
          LocalDate.parse(TaxYear.endDate(TaxYear(taxYear))),
          None,
          None
        )

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )
      val Right(requestForCreate: CreatePropertyPeriodicSubmissionRequest) =
        CreatePropertyPeriodicSubmissionRequest.fromUkRaRAbout(
          TaxYear(taxYear),
          Some(emptyPeriodicSubmission),
          ukRaRAbout
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
          .saveRaRAbout(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            Nino(nino),
            ukRaRAbout
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save rentals and rent a room about" should {

    val taxYear = 2024
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(TaxYear(taxYear), IncomeSourceId(incomeSourceId), Mtditid(mtditid), Nino(nino))

    val rentalsAndRaRAbout = RentalsAndRaRAbout(
      true,
      55.22,
      true,
      22.33,
      ClaimExpensesOrRelief(
        true,
        Some(22.55)
      )
    )
    val annualSubmission = PropertyAnnualSubmission(None, None, None)

    "return no content for valid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)
      mockGetAllPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("", fromDate, toDate)).asRight[ApiError]
      )

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )

      val emptyPeriodicSubmission =
        PropertyPeriodicSubmission(
          None,
          None,
          LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
          LocalDate.parse(TaxYear.endDate(TaxYear(taxYear))),
          None,
          None
        )

      val Right(requestForCreate: CreatePropertyPeriodicSubmissionRequest) =
        CreatePropertyPeriodicSubmissionRequest.fromRentalsAndRaRAbout(
          TaxYear(taxYear),
          Some(emptyPeriodicSubmission),
          rentalsAndRaRAbout
        )

      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        requestForCreate,
        Some(PeriodicSubmissionId("")).asRight[ApiError]
      )

      mockCreateAnnualSubmission(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Some(
          annualSubmission.copy(ukOtherProperty =
            Some(
              AnnualUkOtherProperty(
                Some(
                  UkOtherAdjustments(
                    None,
                    None,
                    None,
                    None,
                    Some(false),
                    Some(UkRentARoom(rentalsAndRaRAbout.jointlyLetYesOrNo))
                  )
                ),
                None
              )
            )
          )
        ),
        ().asRight[ApiError]
      )
      await(
        underTest
          .saveRentalsAndRaRAbout(
            ctx.toJourneyContext(JourneyName.RentalsAndRaRAbout),
            Nino(nino),
            rentalsAndRaRAbout
          )
          .value
      ) shouldBe Right(true)
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
          LocalDate.parse(TaxYear.startDate(TaxYear(taxYear))),
          LocalDate.parse(TaxYear.endDate(TaxYear(taxYear))),
          None,
          None
        )

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )
      val Right(requestForCreate: CreatePropertyPeriodicSubmissionRequest) =
        CreatePropertyPeriodicSubmissionRequest.fromRentalsAndRaRAbout(
          TaxYear(taxYear),
          Some(emptyPeriodicSubmission),
          rentalsAndRaRAbout
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
          .saveRentalsAndRaRAbout(
            ctx.toJourneyContext(JourneyName.RentalsAndRaRAbout),
            Nino(nino),
            rentalsAndRaRAbout
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save uk rent a room adjustments" should {

    val taxYear = 2024
    val mtditid = "1234567890"
    val ctx = JourneyContext(
      TaxYear(taxYear),
      IncomeSourceId(incomeSourceId),
      Mtditid(mtditid),
      JourneyName.RentARoomAdjustments
    )

    val ukRaRAdjustments = RaRAdjustments(
      Some(BalancingCharge(balancingChargeYesNo = true, Some(12.34))),
      None
    )

    val annualSubmission = PropertyAnnualSubmission(None, None, None)

    "return no content for valid request" in {

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )

      mockCreateAnnualSubmission(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Some(
          annualSubmission.copy(ukOtherProperty =
            Some(
              AnnualUkOtherProperty(
                Some(
                  UkOtherAdjustments(
                    None,
                    ukRaRAdjustments.balancingCharge.flatMap(_.balancingChargeAmount),
                    None,
                    None,
                    Some(false),
                    None
                  )
                ),
                None
              )
            )
          )
        ),
        ().asRight[ApiError]
      )
      await(
        underTest
          .saveRaRAdjustments(
            ctx,
            Nino(nino),
            ukRaRAdjustments
          )
          .value
      ) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )
      mockCreateAnnualSubmission(
        TaxYear(taxYear),
        IncomeSourceId(incomeSourceId),
        Nino(nino),
        Some(
          annualSubmission.copy(ukOtherProperty =
            Some(
              AnnualUkOtherProperty(
                Some(
                  UkOtherAdjustments(
                    None,
                    ukRaRAdjustments.balancingCharge.flatMap(_.balancingChargeAmount),
                    None,
                    None,
                    Some(false),
                    None
                  )
                ),
                None
              )
            )
          )
        ),
        ApiError(BAD_REQUEST, SingleErrorBody("code", "error")).asLeft[Unit]
      )
      await(
        underTest
          .saveRaRAdjustments(
            ctx,
            Nino(nino),
            ukRaRAdjustments
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }
}
