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

import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR}
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.test.HttpClientSupport
import config.AppConfig
import models.PropertyPeriodicSubmissionResponse
import models.common.{BusinessId, JourneyContextWithNino, JourneyName, Mtditid, Nino, TaxYear}
import models.errors.{ApiError, ApiServiceError, DataNotFoundError, SingleErrorBody}
import models.request.{ElectricChargePointAllowance, PropertyAbout, RentalAllowances}
import models.responses._
import utils.{AppConfigStub, UnitTest}
import utils.mocks.{MockIntegrationFrameworkConnector, MockMongoJourneyAnswersRepository, MockPropertyService}

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class PropertyServiceSpec extends UnitTest
  with MockIntegrationFrameworkConnector
  with MockMongoJourneyAnswersRepository
  with MockPropertyService
  with HttpClientSupport {
  private implicit val headerCarrier: HeaderCarrier = HeaderCarrier()

  lazy val appConfigStub: AppConfig = new AppConfigStub().config()
  private val underTest = new PropertyService(mockIntegrationFrameworkConnector, mockMongoJourneyAnswersRepository)
  private val nino = "A34324"
  private val incomeSourceId = "Rental"
  private val submissionId = "submissionId"
  private val taxableEntityId = "taxableEntityId"

  ".getAllPropertyPeriodicSubmissions" should {

    "return data when GetPeriodicSubmission has ids and the period is for a year" in {
      val periodicSubmissionIds = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2023-01-01"), LocalDate.parse("2024-01-02"))
      )
      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        submittedOn = Some(LocalDateTime.now),
        fromDate = LocalDate.now.minusDays(1),
        toDate = LocalDate.now,
        None, None, None, None
      )
      val taxYear = 2024

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(periodicSubmissionIds))
      mockGetPropertyPeriodicSubmission(taxYear, nino, incomeSourceId, "1", Right(Some(propertyPeriodicSubmission)))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId)) shouldBe
        Right(PropertyPeriodicSubmissionResponse(List(propertyPeriodicSubmission)))
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and the period is less than a year" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11")),
      )

      val taxYear = 2024

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId)) shouldBe Left(DataNotFoundError)
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and there is no submission" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental")) shouldBe Left(DataNotFoundError)
    }

    "return DataNotFoundError when GetPeriodicSubmission does not have ids" in {
      val aPeriodicSubmissionModel = List.empty[PeriodicSubmissionIdModel]

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental")) shouldBe Left(DataNotFoundError)

    }

    "return ApiError when GetPeriodicSubmissionIds fails" in {
      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("code", "error"))))
      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental")) shouldBe Left(ApiServiceError(500))
    }
  }

  ".getPropertyAnnualSubmission" should {

    "return data when successful" in {
      val aPropertyAnnualSubmission = PropertyAnnualSubmission(
        submittedOn = Some(LocalDateTime.now),
        Some(AnnualForeignFhlEea(
          ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
          ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
        )), None, None, None
      )
      val taxYear = 2024

      mockGetPropertyAnnualSubmission(taxYear, nino, incomeSourceId, Right(Some(aPropertyAnnualSubmission)))

      await(underTest.getPropertyAnnualSubmission(taxYear, nino, incomeSourceId)) shouldBe
        Right(aPropertyAnnualSubmission)
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and the period is less than a year" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11")),
      )

      val taxYear = 2024

      mockGetAllPeriodicSubmission(taxYear, nino, incomeSourceId, Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(taxYear, nino, incomeSourceId)) shouldBe Left(DataNotFoundError)
    }

    "return DataNotFoundError when GetPeriodicSubmission has ids and there is no submission" in {
      val aPeriodicSubmissionModel = List(
        PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11"))
      )

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental")) shouldBe Left(DataNotFoundError)
    }

    "return DataNotFoundError when GetPeriodicSubmission does not have ids" in {
      val aPeriodicSubmissionModel = List.empty[PeriodicSubmissionIdModel]

      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Right(aPeriodicSubmissionModel))

      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental")) shouldBe Left(DataNotFoundError)

    }

    "return ApiError when GetPeriodicSubmissionIds fails" in {
      mockGetAllPeriodicSubmission(2024, "A34324", "Rental", Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("code", "error"))))
      await(underTest.getPropertyPeriodicSubmissions(2024, "A34324", "Rental")) shouldBe Left(ApiServiceError(500))
    }
  }

  "create periodic submission" should {

     val validRequestBody: JsValue = Json.parse(
      """
        |{
        |   "fromDate": "1933-03-31",
        |   "toDate": "2000-02-29",
        |   "foreignFhlEea": {
        |      "income": {
        |         "rentAmount": 200.00
        |      },
        |      "expenses": {
        |         "consolidatedExpenseAmount": 1000.99
        |       }
        |   }
        |}
        |""".stripMargin)

    "return submissionId for valid request" in {
      val periodicSubmissionId = PeriodicSubmissionId("submissionId")
      val taxYear = 2024

      mockCreatePeriodicSubmission(taxYear, nino, incomeSourceId, Right(Some(periodicSubmissionId)))

      await(underTest.createPeriodicSubmission(nino, incomeSourceId, taxYear, Some(validRequestBody))) shouldBe
        Right(periodicSubmissionId)
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024
      mockCreatePeriodicSubmission(taxYear, nino, incomeSourceId, Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error"))))
      await(underTest.createPeriodicSubmission(nino, incomeSourceId, taxYear, Some(validRequestBody))) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }

  }

  "update periodic submission" should {

    val validRequestBody: JsValue = Json.parse(
      """
        |{
        |   "foreignFhlEea": {
        |      "income": {
        |         "rentAmount": 200.00
        |      },
        |      "expenses": {
        |         "consolidatedExpense": 1000.99
        |       }
        |   }
        |}
        |""".stripMargin)

    "return no content for valid request" in {
      val taxYear = 2024

      mockUpdatePeriodicSubmission(taxYear, nino, incomeSourceId, submissionId, Right(None))

      await(underTest.updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, Some(validRequestBody))) shouldBe
        Right("")
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024
      mockUpdatePeriodicSubmission(taxYear, nino, incomeSourceId, submissionId, Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error"))))
      await(underTest.updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, Some(validRequestBody))) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }

  }

  "delete annual submission" should {

  "return no content for valid request" in {
    val taxYear = 2024
    mockDeleteAnnualSubmissions(incomeSourceId, taxableEntityId, taxYear, Right(None))

    await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear)) shouldBe
      Right(())
  }

  "return ApiError for invalid request" in {
    val taxYear = 2024
    mockDeleteAnnualSubmissions(incomeSourceId, taxableEntityId, taxYear, Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error"))))
    await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear)) shouldBe Left(ApiServiceError(BAD_REQUEST))
  }
}


  "create annual submission" should {
    val validRequestBody: JsValue = Json.toJson(PropertyAnnualSubmission(
      submittedOn = Some(LocalDateTime.now),
      Some(AnnualForeignFhlEea(
        ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
        ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
      )), None, None, None))


    "return no content for valid request" in {
      val taxYear = 2024
      mockCreateAnnualSubmission(taxYear, nino, incomeSourceId, Right())
      await(underTest.createOrUpdateAnnualSubmission(nino, incomeSourceId, taxYear, Some(validRequestBody))) shouldBe
        Right()
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024
      mockUpdatePeriodicSubmission(taxYear, nino, incomeSourceId, submissionId, Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error"))))
      await(underTest.updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, Some(validRequestBody))) shouldBe Left(ApiServiceError(BAD_REQUEST))
    }
  }

  "create annual submission 2" should {
    val validRequestBody = PropertyAnnualSubmission(
      submittedOn = Some(LocalDateTime.now),
      Some(AnnualForeignFhlEea(
        ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
        ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
      )), None, None, None)


    "return no content for valid request" in {
      val taxYear = 2024
      mockCreateAnnualSubmission2(TaxYear(taxYear), BusinessId(incomeSourceId), Nino(nino), Right())
      await(underTest.createOrUpdateAnnualSubmission(TaxYear(taxYear), BusinessId(incomeSourceId), Nino(nino), validRequestBody)) shouldBe
        Right()
    }

    "return ApiError for invalid request" in {
      val taxYear = 2024
      mockCreateAnnualSubmission2(TaxYear(taxYear), BusinessId(incomeSourceId), Nino(nino), Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error"))))
      await(underTest.createOrUpdateAnnualSubmission(TaxYear(taxYear),
        BusinessId(incomeSourceId), Nino(nino), validRequestBody)) shouldBe Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error")))
    }
  }

  "save property rental allowances" should {

    val taxYear = 2024
    val mtditid = "1234567890"
    val ctx = JourneyContextWithNino(TaxYear(taxYear), BusinessId(incomeSourceId), Mtditid(mtditid), Nino(nino))
    val allowances = RentalAllowances(
      Some(11),
      ElectricChargePointAllowance(electricChargePointAllowanceYesNo = true, Some(11)),
      Some(11),
      Some(11),
      Some(11),
      Some(11),
      Some(11)
    )
    "return no content for valid request" in {
      mockCreateAnnualSubmission2(TaxYear(taxYear), BusinessId(incomeSourceId), Nino(nino), Right())
      await(underTest.savePropertyRentalAllowances(ctx, allowances)) shouldBe Right(true)
    }

    "return ApiError for invalid request" in {
      mockCreateAnnualSubmission2(TaxYear(taxYear), BusinessId(incomeSourceId), Nino(nino), Left(ApiError(BAD_REQUEST, SingleErrorBody("code", "error"))))
      await(underTest.savePropertyRentalAllowances(ctx, allowances)) shouldBe Right(true)
    }
  }


}

