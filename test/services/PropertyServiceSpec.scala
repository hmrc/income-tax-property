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
import models.IncomeSourceType.UKPropertyOther
import models.LossType.UKProperty
import models.common.TaxYear.asTyBefore24
import models.common._
import models.domain._
import models.errors._
import models.request.WhenYouReportedTheLoss.toTaxYear
import models.request._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba.EsbaInfoExtensions.EsbaExtensions
import models.request.esba._
import models.request.foreign.{ForeignPropertySelectCountry, TotalIncome}
import models.request.sba.SbaInfoExtensions.SbaExtensions
import models.request.sba.{Sba, SbaInfo}
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import models.{IncomeSourceType, PropertyPeriodicSubmissionResponse, RentalsAndRaRAbout}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper
import org.scalatest.time.{Millis, Span}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.http.Status._
import play.api.libs.json.{JsObject, Json}
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.mocks.{MockHipConnector, MockIntegrationFrameworkConnector, MockMergeService, MockMongoJourneyAnswersRepository}
import utils.providers.AppConfigStubProvider
import utils.{AppConfigStub, FeatureSwitchConfig, UnitTest}

import java.time.{Clock, LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PropertyServiceSpec
    extends UnitTest with MockIntegrationFrameworkConnector with MockMongoJourneyAnswersRepository with MockMergeService
    with MockHipConnector with HttpClientSupport with ScalaCheckPropertyChecks with AppConfigStubProvider {
  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
  private val underTest = new PropertyService(mergeService, mockIntegrationFrameworkConnector, journeyAnswersService, appConfigStub, mockHipConnector)
  private val hipApisEnabledFSConfig = FeatureSwitchConfig(enableHipApis = true)
  private val appConfigWithHipApisEnabled: AppConfig = new AppConfigStub().config(featureSwitchConfig = Some(hipApisEnabledFSConfig))
  private val underTestWithHipApisEnabled = new PropertyService(mergeService, mockIntegrationFrameworkConnector, journeyAnswersService, appConfigWithHipApisEnabled, mockHipConnector)
  private val nino = Nino("A34324")
  private val incomeSourceId = IncomeSourceId("Rental")
  private val submissionId = "submissionId"
  private val taxableEntityId = Nino("taxableEntityId")
  private val taxYear: TaxYear = TaxYear(2024)
  private val whenYouReportedTheLoss: WhenYouReportedTheLoss = WhenYouReportedTheLoss.y2018to2019
  private val lossId = "lossId"
  private val lossAmount = BigDecimal(32.47)
  private val businessId = "some-business-id"
  private val typeOfLossIF = UKProperty
  private val typeOfLossHiP = UKPropertyOther
  private val taxYearBroughtForwardFrom = TaxYear(2019)
  private val lastModified = LocalDate.now


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

  ".getPropertyAnnualSubmission" should {

    "return data when successful" in {
      val aPropertyAnnualSubmission = PropertyAnnualSubmission(
        submittedOn = Some(LocalDateTime.now),
        None,
        Some(
          AnnualUkOtherProperty(
            Some(
              UkOtherAdjustments(
                Some(1),
                Some(2),
                Some(3),
                Some(4),
                Some(true),
                None,
                None,
                Some(WhenYouReportedTheLoss.y2018to2019)
              )
            ),
            None
          )
        )
      )

      mockGetPropertyAnnualSubmission(taxYear, nino, incomeSourceId, Right(Some(aPropertyAnnualSubmission)))

      await(underTest.getPropertyAnnualSubmission(taxYear, nino, incomeSourceId).value) shouldBe
        Right(aPropertyAnnualSubmission)
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

  val validCreateUKPropertyPeriodicSubmissionRequest: CreateUKPropertyPeriodicSubmissionRequest =
    CreateUKPropertyPeriodicSubmissionRequest(
      LocalDate.now(),
      LocalDate.now(),
      Some(
        UkOtherProperty(
          Some(UkOtherPropertyIncome(Some(200.00), Some(200.00), Some(200.00), Some(200.00), Some(200.00), None)),
          None
        )
      )
    )
  val validUpdateUKPropertyPeriodicSubmissionRequest: UpdateUKPropertyPeriodicSubmissionRequest =
    UpdateUKPropertyPeriodicSubmissionRequest(
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

      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        validCreateUKPropertyPeriodicSubmissionRequest,
        Right(Some(periodicSubmissionId))
      )

      await(
        underTest
          .createPeriodicSubmission(nino, incomeSourceId, taxYear, validCreateUKPropertyPeriodicSubmissionRequest)
          .value
      ) shouldBe
        Right(Some(periodicSubmissionId))
    }

    "return ApiError for invalid request" in {

      mockCreatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        validCreateUKPropertyPeriodicSubmissionRequest,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest
          .createPeriodicSubmission(nino, incomeSourceId, taxYear, validCreateUKPropertyPeriodicSubmissionRequest)
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }

  }

  "update periodic submission" should {

    "return no content for valid request" in {

      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        validUpdateUKPropertyPeriodicSubmissionRequest,
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
            validUpdateUKPropertyPeriodicSubmissionRequest
          )
          .value
      ) shouldBe
        Right("")
    }

    "return ApiError for invalid request" in {

      mockUpdatePeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        validUpdateUKPropertyPeriodicSubmissionRequest,
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
            validUpdateUKPropertyPeriodicSubmissionRequest
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }

  }

  "delete annual submission" should {

    "return no content for valid request" in {

      mockDeleteAnnualSubmissions(incomeSourceId, taxableEntityId, taxYear, Right(None))

      await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear).value) shouldBe
        Right(())
    }

    "return ApiError for invalid request" in {

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
        AnnualUkOtherProperty(
          Some(
            UkOtherAdjustments(
              Some(1),
              Some(2),
              Some(3),
              Some(4),
              Some(true),
              None,
              None,
              Some(WhenYouReportedTheLoss.y2018to2019)
            )
          ),
          None
        )
      )
    )

    "return no content for valid request" in {

      mockCreateAnnualSubmission(taxYear, incomeSourceId, nino, Right((): Unit))
      await(
        underTest
          .createOrUpdateAnnualSubmission(taxYear, incomeSourceId, nino, validRequest)
          .value
      ) shouldBe
        ().asRight[ApiError]
    }

    "return ApiError for invalid request" in {

      mockCreateAnnualSubmission(
        taxYear,
        incomeSourceId,
        nino,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )

      await(
        underTest
          .createOrUpdateAnnualSubmission(taxYear, incomeSourceId, nino, validRequest)
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "create annual submission 2" should {
    val validRequestBody = PropertyAnnualSubmission(
      submittedOn = Some(LocalDateTime.now),
      None,
      Some(
        AnnualUkOtherProperty(
          Some(
            UkOtherAdjustments(
              Some(1),
              Some(2),
              Some(3),
              Some(4),
              Some(true),
              None,
              None,
              Some(WhenYouReportedTheLoss.y2018to2019)
            )
          ),
          None
        )
      )
    )

    "return no content for valid request" in {
      mockCreateAnnualSubmission(taxYear, incomeSourceId, nino, ().asRight[ApiError])
      await(
        underTest
          .createOrUpdateAnnualSubmission(
            taxYear,
            incomeSourceId,
            nino,
            validRequestBody
          )
          .value
      ) shouldBe
        ().asRight[ApiError]
    }

    "return ApiError for invalid request" in {

      mockCreateAnnualSubmission(
        taxYear,
        incomeSourceId,
        nino,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest
          .createOrUpdateAnnualSubmission(
            taxYear,
            incomeSourceId,
            nino,
            validRequestBody
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save property rental adjustments" should {

    val mtditid = "89787469409"
    val journeyContextWithNino =
      JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)
    val journeyContext = journeyContextWithNino.toJourneyContext(JourneyName.RentalAdjustments)
    val propertyRentalAdjustments = PropertyRentalAdjustments(
      BigDecimal(12.34),
      BalancingCharge(isBalancingCharge = true, Some(108)),
      Some(BigDecimal(34.56)),
      RenovationAllowanceBalancingCharge(
        isRenovationAllowanceBalancingCharge = true,
        renovationAllowanceBalancingChargeAmount = Some(92)
      ),
      BigDecimal(56.78),
      Some(BigDecimal(78.89)),
      UnusedLossesBroughtForward(
        isUnusedLossesBroughtForward = true,
        unusedLossesBroughtForwardAmount = Some(32.47)),
      Some(WhenYouReportedTheLoss.y2018to2019)
    )

    "return a success with no content when the request is valid and data is persisted" in {
      val annualUkOtherProperty =
        AnnualUkOtherProperty(
          Some(
            UkOtherAdjustments(Some(44), None, None, None, None, None, None, Some(WhenYouReportedTheLoss.y2018to2019))
          ),
          None
        )

      mockGetAllPeriodicSubmissionIds(
        taxYear,
        nino,
        incomeSourceId,
        List(
          PeriodicSubmissionIdModel(
            "1",
            LocalDate.parse(TaxYear.startDate(TaxYear(taxYear.endYear - 1))),
            LocalDate.parse(TaxYear.endDate(taxYear))
          )
        )
          .asRight[ApiError]
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

      val requestForCreate: CreateUKPropertyPeriodicSubmissionRequest =
        CreateUKPropertyPeriodicSubmissionRequest.fromPropertyRentalAdjustments(
          taxYear,
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
          AnnualUkOtherProperty(
            Some(
              UkOtherAdjustments(
                Some(44),
                Some(108),
                Some(12.34),
                Some(92),
                None,
                None,
                None,
                Some(WhenYouReportedTheLoss.y2018to2019)
              )
            ),
            None
          )
        )
      )
      val broughtForwardLosses = BroughtForwardLosses(Seq(BroughtForwardLossResponseWithId("lossId", "businessId", UKProperty, BigDecimal(32.32), "2022", "2022")))
      val broughtForwardLossResponse = BroughtForwardLossResponse("businessId", UKProperty, BigDecimal(32.32), "2022", "2022")
      mockGetPropertyAnnualSubmission(taxYear, nino, incomeSourceId, Some(annualSubmission).asRight[ApiError])
      mockCreateAnnualSubmission(
        taxYear,
        incomeSourceId,
        nino,
        Some(updatedAnnualSubmission),
        ().asRight[ApiError]
      )
      mockGetBroughtForwardLosses(
        whenYouReportedTheLoss,
        nino,
        incomeSourceId,
        broughtForwardLosses.asRight[ApiError]
      )
      mockUpdateBroughtForwardLoss(
        whenYouReportedTheLoss,
        nino,
        lossId,
        lossAmount,
        broughtForwardLossResponse.asRight[ApiError]
      )
      await(
        underTest.savePropertyRentalAdjustments(journeyContext, nino, propertyRentalAdjustments).value
      ) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {

      val annualUkOtherProperty =
        AnnualUkOtherProperty(
          Some(
            UkOtherAdjustments(Some(44), None, None, None, None, None, None, Some(WhenYouReportedTheLoss.y2018to2019))
          ),
          None
        )
      val annualSubmission = PropertyAnnualSubmission(None, None, Some(annualUkOtherProperty))
      val updatedAnnualSubmission = PropertyAnnualSubmission(
        None,
        None,
        Some(
          AnnualUkOtherProperty(
            Some(
              UkOtherAdjustments(
                Some(44),
                Some(108),
                Some(12.34),
                Some(92),
                None,
                None,
                None,
                Some(WhenYouReportedTheLoss.y2018to2019)
              )
            ),
            None
          )
        )
      )

      mockGetAllPeriodicSubmissionIds(
        taxYear,
        nino,
        incomeSourceId,
        List(
          PeriodicSubmissionIdModel(
            "",
            LocalDate.parse(TaxYear.startDate(TaxYear(taxYear.endYear - 1))),
            LocalDate.parse(TaxYear.endDate(taxYear))
          )
        )
          .asRight[ApiError]
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

      val requestForCreate: CreateUKPropertyPeriodicSubmissionRequest =
        CreateUKPropertyPeriodicSubmissionRequest.fromPropertyRentalAdjustments(
          taxYear,
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
        taxYear,
        incomeSourceId,
        nino,
        Some(updatedAnnualSubmission),
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(
        underTest.savePropertyRentalAdjustments(journeyContext, nino, propertyRentalAdjustments).value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save property rental allowances" should {

    val mtditid = "1234567890"
    val ctx = JourneyContext(taxYear, incomeSourceId, Mtditid(mtditid), JourneyName.RentalAllowances)
    val allowances = RentalAllowances(
      None,
      Some(11),
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
      mockCreateAnnualSubmission(taxYear, incomeSourceId, nino, ().asRight[ApiError])
      await(underTest.savePropertyRentalAllowances(ctx, nino, allowances).value) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {
      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )
      mockCreateAnnualSubmission(
        taxYear,
        incomeSourceId,
        nino,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(underTest.savePropertyRentalAllowances(ctx, nino, allowances).value) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save property rent a room allowances" should {

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)
    val allowances = RentARoomAllowances(
      None,
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
      mockCreateAnnualSubmission(taxYear, incomeSourceId, nino, ().asRight[ApiError])
      await(underTest.saveRentARoomAllowances(ctx, allowances).value) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {
      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )
      mockCreateAnnualSubmission(
        taxYear,
        incomeSourceId,
        nino,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      await(underTest.saveRentARoomAllowances(ctx, allowances).value) shouldBe Left(ApiServiceError(BAD_REQUEST))
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
              None,
              None,
              Some(WhenYouReportedTheLoss.y2018to2019)
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

      val mtditid = "1234567890"
      val ctx =
        JourneyContext(taxYear, incomeSourceId, Mtditid(mtditid), JourneyName.RentalESBA)

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
        enhancedStructureBuildingAllowanceClaims = Some(true),
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
        taxYear,
        incomeSourceId,
        nino,
        Some(annualSubmissionAfterAdditionOfEsbas),
        ().asRight[ApiError]
      )

      await(underTest.saveEsbas(ctx, nino, esbaInfo).value) shouldBe Right(())

    }
  }

  "save sbas" should {
    "call service method" in {

      val mtditid = "1234567890"
      val ctx =
        JourneyContext(taxYear, incomeSourceId, Mtditid(mtditid), JourneyName.RentalESBA)

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
        createAnnualSubmission(Some(sbaInfo.toSba), None)

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmissionWithoutEsbas).asRight[ApiError]
      )

      mockCreateAnnualSubmission(
        taxYear,
        incomeSourceId,
        nino,
        Some(annualSubmissionAfterAdditionOfEsbas),
        ().asRight[ApiError]
      )

      await(underTest.saveSBA(ctx, nino, sbaInfo).value) shouldBe Right(())

    }
  }

  "save income" should {

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

    val propertyRentalsIncome = PropertyRentalsIncome(
      isNonUKLandlord = true,
      12.34,
      13.46,
      Some(DeductingTax(isTaxDeducted = true, Some(14.51))),
      Some(CalculatedFigureYourself(calculatedFigureYourself = true, Some(14.75))),
      Some(98.78),
      Some(64.23),
      Some(PremiumsGrantLease(premiumsGrantLeaseReceived = true, Some(93.85))),
      Some(ReversePremiumsReceived(reversePremiumsReceived = true, Some(913.84)))
    )
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

      val Right(requestForCreate: CreateUKPropertyPeriodicSubmissionRequest) =
        CreateUKPropertyPeriodicSubmissionRequest.fromPropertyRentalsIncome(
          taxYear,
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
            nino,
            propertyRentalsIncome
          )
          .value
      ) shouldBe Right(Some(PeriodicSubmissionId("")))
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

      val Right(requestForCreate: CreateUKPropertyPeriodicSubmissionRequest) =
        CreateUKPropertyPeriodicSubmissionRequest.fromPropertyRentalsIncome(
          taxYear,
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
            nino,
            propertyRentalsIncome
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save rentals and rar income" should {

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

    val rentalsAndRaRIncome = RentalsAndRaRIncome(
      isNonUKLandlord = true,
      otherIncomeFromProperty = 98.45,
      deductingTax = Some(DeductingTax(isTaxDeducted = true, Some(125.50))),
      calculatedFigureYourself = Some(CalculatedFigureYourself(calculatedFigureYourself = true, Some(58.75))),
      yearLeaseAmount = Some(55.78),
      receivedGrantLeaseAmount = Some(65.05),
      premiumsGrantLease = Some(PremiumsGrantLease(premiumsGrantLeaseReceived = true, Some(93.85))),
      reversePremiumsReceived = Some(ReversePremiumsReceived(reversePremiumsReceived = true, Some(913.84)))
    )

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

      val Right(requestForCreate: CreateUKPropertyPeriodicSubmissionRequest) =
        CreateUKPropertyPeriodicSubmissionRequest.fromRentalsAndRaRIncome(
          taxYear,
          Some(emptyPeriodicSubmission),
          rentalsAndRaRIncome
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
          .saveRentalsAndRaRIncome(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            nino,
            rentalsAndRaRIncome
          )
          .value
      ) shouldBe Right(Some(PeriodicSubmissionId("")))
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

      val Right(requestForCreate: CreateUKPropertyPeriodicSubmissionRequest) =
        CreateUKPropertyPeriodicSubmissionRequest.fromRentalsAndRaRIncome(
          taxYear,
          Some(emptyPeriodicSubmission),
          rentalsAndRaRIncome
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
          .saveRentalsAndRaRIncome(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            nino,
            rentalsAndRaRIncome
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save expenses" should {

    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid("mtditid"), nino)
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
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)

      mockGetAllPeriodicSubmissionIds(
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
        UpdateUKPropertyPeriodicSubmissionRequest.fromExpenses(Some(propertyPeriodicSubmission), expenses)
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
            nino,
            expenses
          )
          .value
      ) shouldBe Some(PeriodicSubmissionId("1")).asRight[ServiceError]

    }

    "downstream error when getAllPeriodicSubmission call fails" in {

      mockGetAllPeriodicSubmissionIds(
        taxYear,
        nino,
        incomeSourceId,
        ApiError(500, SingleErrorBody("500", "")).asLeft[List[PeriodicSubmissionIdModel]]
      )

      val result = underTest.saveExpenses(
        ctx,
        nino,
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
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)

      mockGetAllPeriodicSubmissionIds(
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
        nino,
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
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)

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

      mockGetAllPeriodicSubmissionIds(
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
        UpdateUKPropertyPeriodicSubmissionRequest.fromExpenses(Some(propertyPeriodicSubmission), expenses)
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
        nino,
        expenses
      )
      whenReady(result.value) { r =>
        r mustBe ApiServiceError(500).asLeft[Option[SubmissionId]]
      }
    }
  }

  "save rent a room expenses" should {

    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid("mtditid"), nino)
      .toJourneyContext(JourneyName.RentalExpenses)

    "return submissionId when creating" in {

      val raRExpenses = RentARoomExpenses(
        None,
        Some(200),
        None,
        None,
        None,
        None
      )

      mockGetAllPeriodicSubmissionIds(
        taxYear,
        nino,
        incomeSourceId,
        Nil.asRight[ApiError]
      )

      val Right(raRExpensesForCreate: CreateUKPropertyPeriodicSubmissionRequest) =
        CreateUKPropertyPeriodicSubmissionRequest.fromRaRExpenses(
          taxYear,
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
            nino,
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
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)

      mockGetAllPeriodicSubmissionIds(
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
        UpdateUKPropertyPeriodicSubmissionRequest.fromRaRExpenses(Some(propertyPeriodicSubmission), raRExpenses)
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
            nino,
            raRExpenses
          )
          .value
      ) shouldBe Some(PeriodicSubmissionId("1")).asRight[ServiceError]

    }

    "downstream error when getAllPeriodicSubmission call fails" in {

      mockGetAllPeriodicSubmissionIds(
        taxYear,
        nino,
        incomeSourceId,
        ApiError(500, SingleErrorBody("500", "")).asLeft[List[PeriodicSubmissionIdModel]]
      )

      val result = underTest.saveExpenses(
        ctx,
        nino,
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
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)

      mockGetAllPeriodicSubmissionIds(
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
        nino,
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
      val fromDate = TaxYear.startDate(taxYear.endYear)
      val toDate = TaxYear.endDate(taxYear.endYear)

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
      mockGetAllPeriodicSubmissionIds(
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
        UpdateUKPropertyPeriodicSubmissionRequest.fromRaRExpenses(Some(propertyPeriodicSubmission), raRExpenses)

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
        nino,
        raRExpenses
      )
      whenReady(result.value) { r =>
        r mustBe ApiServiceError(500).asLeft[Option[SubmissionId]]
      }
    }
  }

  "fetch" should {

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
      val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

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

      def createFetchedPropertyData(esbaInfoRetrieved: Option[EsbaInfo]) = {
        val ukPropertyData = FetchedUKPropertyData(
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          Some(RentalAllowances(None, None, None, None, None, None, None)),
          esbaInfoRetrieved,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          List(),
          Some(ForeignPropertySelectCountry(TotalIncome.Under, Some(false), None, None, None))
        )
        val foreignPropertyData = FetchedForeignPropertyData(None, None, None, None, None, None, None)
        val fetchedUkAndForeignPropertyData = FetchedUkAndForeignPropertyData(None)
        val foreignIncomeData = FetchedForeignIncomeData(None, List())
        FetchedData(
          propertyData = FetchedPropertyData(
            ukPropertyData = ukPropertyData,
            foreignPropertyData = foreignPropertyData,
            ukAndForeignPropertyData = fetchedUkAndForeignPropertyData
          ),
          incomeData = foreignIncomeData
        )
      }

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
          mockGetAllPeriodicSubmissionIds(
            taxYear,
            nino,
            incomeSourceId,
            List(PeriodicSubmissionIdModel("1", TaxYear.startDate(taxYear.endYear), TaxYear.endDate(taxYear.endYear)))
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
          val result: EitherT[Future, ServiceError, FetchedData] = for {
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
              nino,
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

      val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

      val claimEnhancedStructureBuildingAllowance = true
      val esbaClaims = true

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(aPropertyAnnualSubmission).asRight[ApiError]
      )
      mockGetAllPeriodicSubmissionIds(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("1", TaxYear.startDate(taxYear.endYear), TaxYear.endDate(taxYear.endYear)))
          .asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        Some(propertyPeriodicSubmission).asRight[ApiError]
      )
      val result: EitherT[Future, ServiceError, FetchedData] = for {
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
            .getFetchedPropertyDataMerged(ctx.toJourneyContext(JourneyName.NoJourney), nino, incomeSourceId)
      } yield r
      whenReady(result.value, Timeout(Span(500, Millis))) { response =>
        response shouldBe InternalError(
          "Journey Repo could not be accessed, journey name: no-journey"
        )
          .asLeft[FetchedPropertyData]

      }

    }
    "return ApiError for invalid request" in {
      val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)
      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
      )
      mockGetAllPeriodicSubmissionIds(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("1", TaxYear.startDate(taxYear.endYear), TaxYear.endDate(taxYear.endYear)))
          .asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        Some(propertyPeriodicSubmission).asRight[ApiError]
      )
      val result: EitherT[Future, ServiceError, FetchedData] = for {
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
            .getFetchedPropertyDataMerged(ctx.toJourneyContext(JourneyName.RentalESBA), nino, incomeSourceId)
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
              None,
              JourneyStatus.NotStarted,
              newData,
              now,
              now
            )
          )
          .toFuture()
          .map(_ => true)
      }

      val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)
      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(
          aPropertyAnnualSubmission
        ).asRight[ApiError]
      )

      mockGetAllPeriodicSubmissionIds(
        taxYear,
        nino,
        incomeSourceId,
        List(PeriodicSubmissionIdModel("1", TaxYear.startDate(taxYear.endYear), TaxYear.endDate(taxYear.endYear)))
          .asRight[ApiError]
      )

      mockGetPropertyPeriodicSubmission(
        taxYear,
        nino,
        incomeSourceId,
        "1",
        Some(propertyPeriodicSubmission).asRight[ApiError]
      )

      val result: EitherT[Future, ServiceError, FetchedData] = for {
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
            .getFetchedPropertyDataMerged(ctx.toJourneyContext(JourneyName.AllJourneys), nino, incomeSourceId)
      } yield r

      whenReady(result.value, Timeout(Span(1500, Millis))) { response =>
        response shouldBe Left(RepositoryError)
      }
    }
  }

  "save uk rent a room about" should {

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

    val ukRaRAbout = RaRAbout(
      isJointlyLet = true,
      55.22,
      ClaimExpensesOrRelief(
        isClaimExpensesOrRelief = true,
        Some(22.55)
      )
    )
    val annualSubmission = PropertyAnnualSubmission(None, None, None)

    "return no content for valid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)
      mockGetAllPeriodicSubmissionIds(
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
          LocalDate.parse(TaxYear.startDate(taxYear)),
          LocalDate.parse(TaxYear.endDate(taxYear)),
          None,
          None
        )

      val Right(requestForCreate: CreateUKPropertyPeriodicSubmissionRequest) =
        CreateUKPropertyPeriodicSubmissionRequest.fromUkRaRAbout(
          taxYear,
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
        taxYear,
        incomeSourceId,
        nino,
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
                    Some(UkRentARoom(ukRaRAbout.isJointlyLet)),
                    None,
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
          .saveRaRAbout(
            ctx.toJourneyContext(JourneyName.RentalIncome),
            nino,
            ukRaRAbout
          )
          .value
      ) shouldBe Right(true)
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

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )
      val Right(requestForCreate: CreateUKPropertyPeriodicSubmissionRequest) =
        CreateUKPropertyPeriodicSubmissionRequest.fromUkRaRAbout(
          taxYear,
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
            nino,
            ukRaRAbout
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save rentals and rent a room about" should {

    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(taxYear, incomeSourceId, Mtditid(mtditid), nino)

    val rentalsAndRaRAbout = RentalsAndRaRAbout(
      isJointlyLet = true,
      55.22,
      isClaimPropertyIncomeAllowance = true,
      22.33,
      ClaimExpensesOrRelief(
        isClaimExpensesOrRelief = true,
        Some(22.55)
      )
    )
    val annualSubmission = PropertyAnnualSubmission(None, None, None)

    "return no content for valid request" in {
      val fromDate = LocalDate.now().minusMonths(1)
      val toDate = fromDate.plusMonths(3)
      mockGetAllPeriodicSubmissionIds(
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
          LocalDate.parse(TaxYear.startDate(taxYear)),
          LocalDate.parse(TaxYear.endDate(taxYear)),
          None,
          None
        )

      val Right(requestForCreate: CreateUKPropertyPeriodicSubmissionRequest) =
        CreateUKPropertyPeriodicSubmissionRequest.fromRentalsAndRaRAbout(
          taxYear,
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
        taxYear,
        incomeSourceId,
        nino,
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
                    Some(UkRentARoom(rentalsAndRaRAbout.isJointlyLet)),
                    None,
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
          .saveRentalsAndRaRAbout(
            ctx.toJourneyContext(JourneyName.RentalsAndRaRAbout),
            nino,
            rentalsAndRaRAbout
          )
          .value
      ) shouldBe Right(true)
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

      mockGetPropertyAnnualSubmission(
        taxYear,
        nino,
        incomeSourceId,
        Some(annualSubmission).asRight[ApiError]
      )
      val Right(requestForCreate: CreateUKPropertyPeriodicSubmissionRequest) =
        CreateUKPropertyPeriodicSubmissionRequest.fromRentalsAndRaRAbout(
          taxYear,
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
            nino,
            rentalsAndRaRAbout
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "save uk rent a room adjustments" should {

    val mtditid = "1234567890"
    val ctx = JourneyContext(
      taxYear,
      incomeSourceId,
      Mtditid(mtditid),
      JourneyName.RentARoomAdjustments
    )

    val ukRaRAdjustments = RaRAdjustments(
      Some(BalancingCharge(isBalancingCharge = true, Some(12.34))),
      None,
      None,
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
        taxYear,
        incomeSourceId,
        nino,
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
                    None,
                    None,
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
            nino,
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
        taxYear,
        incomeSourceId,
        nino,
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
                    None,
                    None,
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
            nino,
            ukRaRAdjustments
          )
          .value
      ) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "update brought forward loss" when {
    "feature switch for hip api 1501 is disabled" should {
      "use the IF API#1501 and return the updated BFL loss Id for valid request" in {
        val updatePropertyBFLResult = Right(BroughtForwardLossResponse(businessId, typeOfLossIF, lossAmount, taxYearBroughtForwardFrom.endYear.toString, lastModified.toString))

        mockUpdatePropertyBroughtForwardLoss(whenYouReportedTheLoss, nino, lossId, lossAmount, updatePropertyBFLResult)

        val result = await(
          underTest.updateBroughtForwardLoss(
            whenYouReportedTheLoss,
            nino,
            lossId,
            lossAmount,
            incomeSourceId
          ).value
        )
        result shouldBe Right(BroughtForwardLossResponse(businessId, typeOfLossIF, lossAmount, taxYearBroughtForwardFrom.endYear.toString, lastModified.toString))
      }
      "return ApiError for invalid request" in {
        val apiError = SingleErrorBody("code", "reason")
        val apiErrorCodes = Seq(BAD_REQUEST, UNAUTHORIZED, NOT_FOUND, UNPROCESSABLE_ENTITY, INTERNAL_SERVER_ERROR, NOT_IMPLEMENTED, BAD_GATEWAY, SERVICE_UNAVAILABLE)

        apiErrorCodes.foreach { apiErrorCode =>
          val updatePropertyBFLResult = Left(ApiError(apiErrorCode, apiError))
          mockUpdatePropertyBroughtForwardLoss(whenYouReportedTheLoss, nino, lossId, lossAmount, updatePropertyBFLResult)

          val result = await(
            underTest.updateBroughtForwardLoss(
              whenYouReportedTheLoss,
              nino,
              lossId,
              lossAmount,
              incomeSourceId
            ).value
          )
          result shouldBe Left(ApiServiceError(apiErrorCode))
        }
      }

    }

    "feature switch for hip api 1501 is enabled" should {
      "use the HIP API#1501 and return the updated BFL loss Id for valid request" in {
        val updatePropertyBFLResult = Right(HipPropertyBFLResponse(businessId, typeOfLossHiP, lossAmount, toTaxYear(whenYouReportedTheLoss).endYear, lossId, lastModified))

        mockHipUpdatePropertyBroughtForwardLossSubmission(nino, lossAmount, WhenYouReportedTheLoss.y2018to2019, BroughtForwardLossId(lossId), updatePropertyBFLResult)

        val result = await(
          underTestWithHipApisEnabled.updateBroughtForwardLoss(
            whenYouReportedTheLoss,
            nino,
            lossId,
            lossAmount,
            incomeSourceId
          ).value
        )
        result shouldBe Right(BroughtForwardLossResponse(businessId, typeOfLossIF, lossAmount, asTyBefore24(toTaxYear(whenYouReportedTheLoss)),  lastModified.toString))
      }
      "return ApiError for invalid request" in {
        val apiError = SingleErrorBody("code", "reason")
        val apiErrorCodes = Seq(BAD_REQUEST, UNAUTHORIZED, NOT_FOUND, UNPROCESSABLE_ENTITY, INTERNAL_SERVER_ERROR, NOT_IMPLEMENTED, BAD_GATEWAY, SERVICE_UNAVAILABLE)

        apiErrorCodes.foreach { apiErrorCode =>
          val updatePropertyBFLResult = Left(ApiError(apiErrorCode, apiError))
          mockHipUpdatePropertyBroughtForwardLossSubmission(nino, lossAmount, whenYouReportedTheLoss, BroughtForwardLossId(lossId), updatePropertyBFLResult)

          val result = await(
            underTestWithHipApisEnabled.updateBroughtForwardLoss(
              whenYouReportedTheLoss,
              nino,
              lossId,
              lossAmount,
              incomeSourceId
            ).value
          )
          result shouldBe Left(ApiServiceError(apiErrorCode))
        }
      }
    }
  }

  "create brought forward loss" when {
    "feature switch for hip api 1500 is disabled" should {
      "use the IF API#1500 and return the created BFL loss Id for valid request" in {
        val createPropertyBFLResult = Right(BroughtForwardLossId(lossId))

        mockCreatePropertyBroughtForwardLoss(whenYouReportedTheLoss, nino, incomeSourceId, lossAmount, createPropertyBFLResult)

        val result = await(
          underTest.createBroughtForwardLoss(
            whenYouReportedTheLoss,
            nino,
            incomeSourceId,
            lossAmount
          ).value
        )
        result shouldBe Right(lossId)
      }
      "return ApiError for invalid request" in {
        val apiError = SingleErrorBody("code", "reason")
        val apiErrorCodes = Seq(NOT_FOUND, BAD_REQUEST, UNPROCESSABLE_ENTITY, INTERNAL_SERVER_ERROR, SERVICE_UNAVAILABLE)

        apiErrorCodes.foreach { apiErrorCode =>
          val createPropertyBFLResult = Left(ApiError(apiErrorCode, apiError))
          mockCreatePropertyBroughtForwardLoss(whenYouReportedTheLoss, nino, incomeSourceId, lossAmount, createPropertyBFLResult)

          val result = await(
            underTest.createBroughtForwardLoss(
              whenYouReportedTheLoss,
              nino,
              incomeSourceId,
              lossAmount
            ).value
          )
          result shouldBe Left(ApiServiceError(apiErrorCode))
        }
      }

    }
  }
  "feature switch for hip api 1500 is enabled" should {
    "use the HIP API#1500 and return the created BFL loss Id for valid request" in {
      val incomeSourceType: IncomeSourceType = IncomeSourceType.UKPropertyOther
      val createPropertyBFLResult = Right(BroughtForwardLossId(lossId))

      mockHipCreatePropertyBroughtForwardLossSubmission(nino, incomeSourceId, incomeSourceType, lossAmount, whenYouReportedTheLoss, createPropertyBFLResult)

      val result = await(
        underTestWithHipApisEnabled.createBroughtForwardLoss(
          whenYouReportedTheLoss,
          nino,
          incomeSourceId,
          lossAmount
        ).value
      )
      result shouldBe Right(lossId)
    }
    "return ApiError for invalid request" in {
      val incomeSourceType: IncomeSourceType = IncomeSourceType.UKPropertyOther
      val apiError = SingleErrorBody("code", "reason")
      val apiErrorCodes = Seq(NOT_FOUND, BAD_REQUEST, UNPROCESSABLE_ENTITY, INTERNAL_SERVER_ERROR, SERVICE_UNAVAILABLE)

      apiErrorCodes.foreach { apiErrorCode =>
        val createPropertyBFLResult = Left(ApiError(apiErrorCode, apiError))
        mockHipCreatePropertyBroughtForwardLossSubmission(nino, incomeSourceId, incomeSourceType, lossAmount, whenYouReportedTheLoss, createPropertyBFLResult)

        val result = await(
          underTestWithHipApisEnabled.createBroughtForwardLoss(
            whenYouReportedTheLoss,
            nino,
            incomeSourceId,
            lossAmount
          ).value
        )
        result shouldBe Left(ApiServiceError(apiErrorCode))
      }
    }
  }

  "get brought forward loss" when {
    val lossAmount = 100.00
    val lossId = "some-loss-id"
    val hipPropertyBFLResponse = HipPropertyBFLResponse(
      incomeSourceId.toString,
      incomeSourceType = UKPropertyOther,
      broughtForwardLossAmount = lossAmount,
      taxYearBroughtForwardFrom = toTaxYear(whenYouReportedTheLoss).endYear,
      lossId = lossId,
      submissionDate = LocalDate.now
    )
    val getPropertyBFLResult = BroughtForwardLossResponse(
      businessId = incomeSourceId.toString,
      typeOfLoss = UKProperty,
      lossAmount = lossAmount,
      taxYearBroughtForwardFrom = asTyBefore24(toTaxYear(whenYouReportedTheLoss)),
      lastModified = LocalDate.now.toString
    )
    "feature switch for hip api 1502 is disabled" should {
      "use the IF API#1502 and return the retrieved BFL for valid request" in {
        mockGetPropertyBroughtForwardLoss(nino, lossId, getPropertyBFLResult.asRight[ApiError])
        val result = await(underTest.getBroughtForwardLoss(nino, lossId).value)
        result shouldBe getPropertyBFLResult.asRight[ApiError]
      }
      "return ApiError for invalid request" in {
        val apiError = SingleErrorBody("code", "reason")
        val apiErrorCodes = Seq(NOT_FOUND, BAD_REQUEST, UNPROCESSABLE_ENTITY, INTERNAL_SERVER_ERROR, SERVICE_UNAVAILABLE)
        apiErrorCodes.foreach { apiErrorCode =>
          val getPropertyBFLErrorResult = Left(ApiError(apiErrorCode, apiError))
          mockGetPropertyBroughtForwardLoss(nino, lossId, getPropertyBFLErrorResult)
          val result = await(underTest.getBroughtForwardLoss(nino, lossId).value)
          result shouldBe Left(ApiServiceError(apiErrorCode))
        }
      }
    }
    "feature switch for hip api 1502 is enabled" should {
      "use the HIP API#1502 and return the retrieved BFL for valid request" in {
        mockHipGetPropertyBroughtForwardLossSubmission(nino, lossId, hipPropertyBFLResponse.asRight[ApiError])
        val result = await(underTestWithHipApisEnabled.getBroughtForwardLoss(nino, lossId).value)
        result shouldBe getPropertyBFLResult.asRight[ApiError]
      }
      "return ApiError for invalid request" in {
        val apiError = SingleErrorBody("code", "reason")
        val apiErrorCodes = Seq(NOT_FOUND, BAD_REQUEST, UNPROCESSABLE_ENTITY, INTERNAL_SERVER_ERROR, SERVICE_UNAVAILABLE)
        apiErrorCodes.foreach { apiErrorCode =>
          val getPropertyBFLErrorResult = Left(ApiError(apiErrorCode, apiError))
          mockHipGetPropertyBroughtForwardLossSubmission(nino, lossId, getPropertyBFLErrorResult)
          val result = await(underTestWithHipApisEnabled.getBroughtForwardLoss(nino, lossId).value)
          result shouldBe Left(ApiServiceError(apiErrorCode))
        }
      }
    }
  }
}
