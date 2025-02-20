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

package connectors

import models.common.{IncomeSourceId, Nino, TaxYear}
import models.errors.{SingleErrorBody, ApiError}
import models.request.foreign.{AnnualForeignPropertySubmission, AnnualForeignPropertyAdjustments, AnnualForeignPropertyAllowances, AnnualForeignProperty, AnnualForeignPropertySubmissionAdjustments, AnnualForeignPropertySubmissionAllowances, ForeignPropertyAdjustments, ForeignPropertyAllowances}
import models.request.{CreateUKPropertyPeriodicSubmissionRequest, UpdateUKPropertyPeriodicSubmissionRequest, WhenYouReportedTheLoss}
import models.responses._
import org.scalamock.scalatest.MockFactory
import play.api.http.Status._
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId}

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class IntegrationFrameworkConnectorSpec extends ConnectorIntegrationSpec with MockFactory {

  private val nino = Nino("some-nino")
  private val taxableEntityId = Nino("some-taxable-entity-id")
  private val incomeSourceId = IncomeSourceId("some-income-source-id")
  private val submissionId = "some-submission-id"
  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))

  private val underTest = new IntegrationFrameworkConnector(httpClientV2, appConfigStub)
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
  val countryCode = "BRA"
  val foreignPropertyAdjustments: ForeignPropertyAdjustments =
    ForeignPropertyAdjustments(Some(BigDecimal(55.00)), Some(BigDecimal(654.00)))
  val annualForeignPropertyAdjustments: AnnualForeignPropertyAdjustments =
    AnnualForeignPropertyAdjustments(countryCode, Some(foreignPropertyAdjustments))

  val annualForeignPropertySubmissionAdjustments: AnnualForeignPropertySubmissionAdjustments =
    AnnualForeignPropertySubmissionAdjustments(Some(Seq(annualForeignPropertyAdjustments)))

  val foreignPropertyAllowances: ForeignPropertyAllowances = ForeignPropertyAllowances(
    Some(BigDecimal(99.00)),
    Some(BigDecimal(150.00)),
    Some(BigDecimal(45.00)),
    Some(BigDecimal(768.00)),
    None,
    None,
    None,
    None
  )

  val annualForeignPropertyAllowances: AnnualForeignPropertyAllowances =
    AnnualForeignPropertyAllowances(countryCode, Some(foreignPropertyAllowances))
  val annualForeignPropertySubmissionAllowances: AnnualForeignPropertySubmissionAllowances =
    AnnualForeignPropertySubmissionAllowances(Some(Seq(annualForeignPropertyAllowances)))

  "Given a need to get Periodic Submission Data" when {

    val aPeriodicSubmissionModel = List(
      PeriodicSubmissionIdModel("1", LocalDate.parse("2021-01-01"), LocalDate.parse("2021-11-11")),
      PeriodicSubmissionIdModel("2", LocalDate.parse("2022-02-02"), LocalDate.parse("2022-12-12"))
    )

    "a call is made to the backend API it" should {
      "return correct submissions data for the APIs used before TaxYear(2024)" in {
        val httpResponse = HttpResponse(OK, Json.toJson(aPeriodicSubmissionModel).toString())
        val taxYear = TaxYear(2021)

        stubGetHttpClientCall(
          s"/income-tax/business/property/$taxableEntityId/$incomeSourceId/period\\?taxYear=2020-21",
          httpResponse
        )

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Right(
          aPeriodicSubmissionModel
        )
      }

      "return correct submissions data for TaxYear(2024) onwards" in {
        val httpResponse = HttpResponse(OK, Json.toJson(aPeriodicSubmissionModel).toString())
        val taxYear = TaxYear(2024)

        stubGetHttpClientCall(
          s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/period",
          httpResponse
        )

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Right(
          aPeriodicSubmissionModel
        )
      }

      "return Data Not Found from Upstream" in {
        val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(List.empty[PeriodicSubmissionIdModel]).toString())
        val taxYear = TaxYear(2024)

        stubGetHttpClientCall(
          s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/period",
          httpResponse
        )

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Right(
          List.empty
        )
      }

      "return Service Unavailable Error from Upstream" in {
        val httpResponse =
          HttpResponse(SERVICE_UNAVAILABLE, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())
        val taxYear = TaxYear(2024)

        stubGetHttpClientCall(
          s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/period",
          httpResponse
        )

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Left(
          ApiError(
            SERVICE_UNAVAILABLE,
            SingleErrorBody("some-code", "some-reason")
          )
        )
      }

      "handle Any Other Error from Upstream" in {
        val httpResponse =
          HttpResponse(BAD_GATEWAY, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())

        val taxYear = TaxYear(2024)

        stubGetHttpClientCall(
          s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/period",
          httpResponse
        )

        await(underTest.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe Left(
          ApiError(
            BAD_GATEWAY,
            SingleErrorBody("some-code", "some-reason")
          )
        )
      }
    }
  }

  ".getPropertyPeriodicSubmission" when {

    val aPropertyPeriodicSubmission = PropertyPeriodicSubmission(
      None,
      submittedOn = Some(LocalDateTime.now),
      fromDate = LocalDate.now.minusDays(1),
      toDate = LocalDate.now,
      None,
      None
    )

    "when we call the IF" should {
      "return correct IF data when correct parameters are passed before TaxYear(2024)" in {
        val httpResponse = HttpResponse(OK, Json.toJson(aPropertyPeriodicSubmission).toString())
        val taxYear = TaxYear(2021)

        stubGetHttpClientCall(
          s"/income-tax/business/property/periodic\\?" +
            s"taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId&submissionId=$submissionId",
          httpResponse
        )

        await(
          underTest.getPropertyPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId, submissionId)(hc)
        ) shouldBe
          Right(Some(aPropertyPeriodicSubmission))
      }

      "return correct submissions data for TaxYear(2024) onwards" in {
        val httpResponse = HttpResponse(OK, Json.toJson(aPropertyPeriodicSubmission).toString())
        val taxYear = TaxYear(2024)

        stubGetHttpClientCall(
          s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/periodic/$submissionId",
          httpResponse
        )

        await(
          underTest.getPropertyPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId, submissionId)(hc)
        ) shouldBe
          Right(Some(aPropertyPeriodicSubmission))
      }

      "return IF error when Left is returned" in {
        val httpResponse =
          HttpResponse(INTERNAL_SERVER_ERROR, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())
        val taxYear = TaxYear(2024)

        stubGetHttpClientCall(
          s"/income-tax/business/property/23-24/$taxableEntityId/$incomeSourceId/periodic/$submissionId",
          httpResponse
        )

        await(
          underTest.getPropertyPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId, submissionId)(hc)
        ) shouldBe
          Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("some-code", "some-reason")))
      }
    }
  }

  ".getPropertyAnnualSubmission" when {
    val aPropertyAnnualSubmission = PropertyAnnualSubmission(
      submittedOn = None,
      None,
      Some(
        AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(1), Some(2), Some(3), Some(4), Some(true), None, None, Some(WhenYouReportedTheLoss.y2018to2019))), None)
      )
    )

    "when we call the IF" should {
      "return correct IF data when correct parameters are passed before TaxYear(2024)" in {
        val httpResponse = HttpResponse(OK, Json.toJson(aPropertyAnnualSubmission).toString())
        val taxYear = TaxYear(2021)

        stubGetHttpClientCall(
          s"/income-tax/business/property/annual\\?" +
            s"taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId",
          httpResponse
        )

        await(underTest.getPropertyAnnualSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe
          Right(Some(aPropertyAnnualSubmission))
      }

      "return correct submissions data for TaxYear(2024) onwards" in {
        val httpResponse = HttpResponse(OK, Json.toJson(aPropertyAnnualSubmission).toString())
        val taxYear = TaxYear(2024)

        stubGetHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$taxableEntityId/$incomeSourceId",
          httpResponse
        )

        await(underTest.getPropertyAnnualSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe
          Right(Some(aPropertyAnnualSubmission))
      }

      "return IF error when Left is returned" in {
        val httpResponse =
          HttpResponse(INTERNAL_SERVER_ERROR, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())
        val taxYear = TaxYear(2024)

        stubGetHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$taxableEntityId/$incomeSourceId",
          httpResponse
        )

        await(underTest.getPropertyAnnualSubmission(taxYear, taxableEntityId, incomeSourceId)(hc)) shouldBe
          Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("some-code", "some-reason")))
      }
    }
  }

  "Delete annual submission" should {
    "update submissions data for the APIs used before TaxYear(2024)" in {
      val httpResponse = HttpResponse(NO_CONTENT, "")
      val taxYear = TaxYear(2021)

      stubDeleteHttpClientCall(
        s"/income-tax/business/property/annual\\?taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId",
        httpResponse
      )

      await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear)(hc)) shouldBe Right(())
    }

    "Delete annual submission for TaxYear(2024) onwards" in {
      val httpResponse = HttpResponse(NO_CONTENT, "")
      val taxYear = TaxYear(2024)

      stubDeleteHttpClientCall(
        s"/income-tax/business/property/annual/23-24\\?taxableEntityId=$taxableEntityId&incomeSourceId=$incomeSourceId",
        httpResponse
      )

      await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear)(hc)) shouldBe Right(())
    }

    "return not found from Upstream" in {
      val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(SingleErrorBody("some-code", "NotFound")).toString())
      val taxYear = TaxYear(2024)

      stubDeleteHttpClientCall(
        s"/income-tax/business/property/annual/23-24\\?taxableEntityId=$taxableEntityId&incomeSourceId=$incomeSourceId",
        httpResponse
      )

      await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear)(hc)) shouldBe Left(
        ApiError(NOT_FOUND, SingleErrorBody("some-code", "NotFound"))
      )
    }

    "return unprocessable-entity from Upstream" in {
      val httpResponse =
        HttpResponse(UNPROCESSABLE_ENTITY, Json.toJson(SingleErrorBody("some-code", "unprocessable-entity")).toString())
      val taxYear = TaxYear(2024)

      stubDeleteHttpClientCall(
        s"/income-tax/business/property/annual/23-24\\?taxableEntityId=$taxableEntityId&incomeSourceId=$incomeSourceId",
        httpResponse
      )

      await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear)(hc)) shouldBe Left(
        ApiError(UNPROCESSABLE_ENTITY, SingleErrorBody("some-code", "unprocessable-entity"))
      )
    }

    "return Service Unavailable Error from Upstream" in {
      val httpResponse =
        HttpResponse(SERVICE_UNAVAILABLE, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())
      val taxYear = TaxYear(2024)

      stubDeleteHttpClientCall(
        s"/income-tax/business/property/annual/23-24\\?taxableEntityId=$taxableEntityId&incomeSourceId=$incomeSourceId",
        httpResponse
      )

      await(underTest.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear)(hc)) shouldBe
        Left(ApiError(SERVICE_UNAVAILABLE, SingleErrorBody("some-code", "some-reason")))
    }
  }

  ".createOrUpdateAnnualSubmission" when {

    val aPropertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(
      submittedOn = None,
      None,
      Some(
        AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(1), Some(2), Some(3), Some(4), Some(true), None, None, Some(WhenYouReportedTheLoss.y2018to2019))), None)
      )
    )

    "create Annual Submission" should {

      "create submissions data for the APIs used before TaxYear(2024)" in {
        val taxYear = TaxYear(2021)
        val httpResponse = HttpResponse(NO_CONTENT, Json.toJson(aPropertyAnnualSubmission).toString())
        stubPutHttpClientCall(
          s"/income-tax/business/property/annual\\?taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        val propertyAnnualSubmission: PropertyAnnualSubmission =
          Json.toJson(aPropertyAnnualSubmission).as[PropertyAnnualSubmission]
        await(
          underTest.createOrUpdateAnnualSubmission(
            taxYear,
            incomeSourceId,
            taxableEntityId,
            propertyAnnualSubmission
          )(hc)
        ) shouldBe Right((): Unit)
      }

      "create submissions data for TaxYear(2024) onwards" in {
        val taxYear = TaxYear(2024)

        val httpResponse = HttpResponse(NO_CONTENT, Json.toJson(aPropertyAnnualSubmission).toString())

        stubPutHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        await(
          underTest
            .createOrUpdateAnnualSubmission(
              taxYear,
              incomeSourceId,
              nino,
              aPropertyAnnualSubmission
            )(hc)
        ) shouldBe Right((): Unit)
      }

      "return Conflict from Upstream" in {
        val httpResponse = HttpResponse(CONFLICT, Json.toJson(SingleErrorBody("some-code", "Conflict")).toString())
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        await(
          underTest
            .createOrUpdateAnnualSubmission(
              taxYear,
              incomeSourceId,
              nino,
              aPropertyAnnualSubmission
            )(hc)
        ) shouldBe Left(ApiError(500, SingleErrorBody("some-code", "Conflict")))
      }

      "return not found from Upstream" in {
        val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(SingleErrorBody("some-code", "NotFound")).toString())
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        await(
          underTest
            .createOrUpdateAnnualSubmission(
              taxYear,
              incomeSourceId,
              nino,
              aPropertyAnnualSubmission
            )(hc)
        ) shouldBe Left(ApiError(NOT_FOUND, SingleErrorBody("some-code", "NotFound")))
      }

      "return Service Unavailable Error from Upstream" in {
        val httpResponse =
          HttpResponse(SERVICE_UNAVAILABLE, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        await(
          underTest
            .createOrUpdateAnnualSubmission(
              taxYear,
              incomeSourceId,
              nino,
              aPropertyAnnualSubmission
            )(hc)
        ) shouldBe
          Left(ApiError(SERVICE_UNAVAILABLE, SingleErrorBody("some-code", "some-reason")))
      }
    }

  }

  "create Or Update Annual Submission" when {

    val aPropertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(
      submittedOn = None,
      None,
      Some(
        AnnualUkOtherProperty(Some(UkOtherAdjustments(Some(1), Some(2), Some(3), Some(4), Some(true), None, None,  Some(WhenYouReportedTheLoss.y2018to2019))), None)
      )
    )

    "create Annual Submission" should {

      "create submissions data for the APIs used before TaxYear(2024)" in {
        val taxYear = TaxYear(2021)
        val httpResponse = HttpResponse(NO_CONTENT, Json.toJson(aPropertyAnnualSubmission).toString())
        stubPutHttpClientCall(
          s"/income-tax/business/property/annual\\?taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        await(
          underTest.createOrUpdateAnnualSubmission(
            taxYear,
            incomeSourceId,
            taxableEntityId,
            aPropertyAnnualSubmission
          )(hc)
        ) shouldBe Right((): Unit)
      }

      "create submissions data for TaxYear(2024) onwards" in {
        val taxYear = TaxYear(2024)

        val httpResponse = HttpResponse(NO_CONTENT, Json.toJson(aPropertyAnnualSubmission).toString())

        stubPutHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        await(
          underTest.createOrUpdateAnnualSubmission(
            taxYear,
            incomeSourceId,
            nino,
            aPropertyAnnualSubmission
          )(hc)
        ) shouldBe Right((): Unit)
      }

      "return Conflict from Upstream" in {
        val httpResponse = HttpResponse(CONFLICT, Json.toJson(SingleErrorBody("some-code", "Conflict")).toString())
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        await(
          underTest.createOrUpdateAnnualSubmission(
            taxYear,
            incomeSourceId,
            nino,
            aPropertyAnnualSubmission
          )(hc)
        ) shouldBe Left(ApiError(500, SingleErrorBody("some-code", "Conflict")))
      }

      "return not found from Upstream" in {
        val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(SingleErrorBody("some-code", "NotFound")).toString())
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        await(
          underTest.createOrUpdateAnnualSubmission(
            taxYear,
            incomeSourceId,
            nino,
            aPropertyAnnualSubmission
          )(hc)
        ) shouldBe Left(ApiError(NOT_FOUND, SingleErrorBody("some-code", "NotFound")))
      }

      "return Service Unavailable Error from Upstream" in {
        val httpResponse =
          HttpResponse(SERVICE_UNAVAILABLE, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
          Json.toJson(aPropertyAnnualSubmission).toString(),
          httpResponse
        )

        await(
          underTest.createOrUpdateAnnualSubmission(
            taxYear,
            incomeSourceId,
            nino,
            aPropertyAnnualSubmission
          )(hc)
        ) shouldBe
          Left(ApiError(SERVICE_UNAVAILABLE, SingleErrorBody("some-code", "some-reason")))
      }
    }

  }

  "Given a need to create Periodic Submission Data" when {
    val fromDate = LocalDate.now()
    val toDate = LocalDate.now()

    val aPeriodicSubmissionModel = PeriodicSubmissionId("1")
    val requestBody: JsValue = Json.parse(s"""
                                             |{
                                             |   "fromDate": "$fromDate",
                                             |   "toDate": "$toDate",
                                             |   "ukOtherProperty": {
                                             |      "income": {
                                             |      "premiumsOfLeaseGrant": 200.00,
                                             |      "reversePremiums": 200.00,
                                             |      "periodAmount": 200.00,
                                             |      "taxDeducted": 200.00,
                                             |      "otherIncome": 200.00
                                             |      }
                                             |   }
                                             |}
                                             |""".stripMargin)

    "create periodic submission" should {
      "create submissions data for the APIs used before TaxYear(2024)" in {
        val httpResponse = HttpResponse(CREATED, Json.toJson(aPeriodicSubmissionModel).toString())
        val taxYear = TaxYear(2021)

        stubPostHttpClientCall(
          s"/income-tax/business/property/periodic\\?taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest.createPeriodicSubmission(
            taxYear,
            taxableEntityId,
            incomeSourceId,
            validCreateUKPropertyPeriodicSubmissionRequest
          )(hc)
        ) shouldBe Right(Some(aPeriodicSubmissionModel))
      }

      "create submissions data for TaxYear(2024) onwards" in {
        val httpResponse = HttpResponse(CREATED, Json.toJson(aPeriodicSubmissionModel).toString())
        val taxYear = TaxYear(2024)

        stubPostHttpClientCall(
          s"/income-tax/business/property/periodic/23-24\\?taxableEntityId=$nino&incomeSourceId=$incomeSourceId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest
            .createPeriodicSubmission(taxYear, nino, incomeSourceId, validCreateUKPropertyPeriodicSubmissionRequest)(hc)
        ) shouldBe Right(Some(aPeriodicSubmissionModel))
      }

      "return Conflict from Upstream" in {
        val httpResponse = HttpResponse(CONFLICT, Json.toJson(SingleErrorBody("some-code", "Conflict")).toString())
        val taxYear = TaxYear(2024)

        stubPostHttpClientCall(
          s"/income-tax/business/property/periodic/23-24\\?taxableEntityId=$nino&incomeSourceId=$incomeSourceId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest
            .createPeriodicSubmission(taxYear, nino, incomeSourceId, validCreateUKPropertyPeriodicSubmissionRequest)(hc)
        ) shouldBe Left(ApiError(CONFLICT, SingleErrorBody("some-code", "Conflict")))
      }
      "return not found from Upstream" in {
        val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(SingleErrorBody("some-code", "NotFound")).toString())
        val taxYear = TaxYear(2024)

        stubPostHttpClientCall(
          s"/income-tax/business/property/periodic/23-24\\?taxableEntityId=$nino&incomeSourceId=$incomeSourceId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest
            .createPeriodicSubmission(taxYear, nino, incomeSourceId, validCreateUKPropertyPeriodicSubmissionRequest)(hc)
        ) shouldBe Left(ApiError(NOT_FOUND, SingleErrorBody("some-code", "NotFound")))
      }

      "return Service Unavailable Error from Upstream" in {
        val httpResponse =
          HttpResponse(SERVICE_UNAVAILABLE, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())
        val taxYear = TaxYear(2024)

        stubPostHttpClientCall(
          s"/income-tax/business/property/periodic/23-24\\?taxableEntityId=$nino&incomeSourceId=$incomeSourceId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest
            .createPeriodicSubmission(taxYear, nino, incomeSourceId, validCreateUKPropertyPeriodicSubmissionRequest)(hc)
        ) shouldBe
          Left(ApiError(SERVICE_UNAVAILABLE, SingleErrorBody("some-code", "some-reason")))
      }
    }
  }

  "Given a need to update Periodic Submission Data" when {

    val requestBody: JsValue = Json.parse(s"""
                                             |{
                                             |   "ukOtherProperty": {
                                             |      "income": {
                                             |      "premiumsOfLeaseGrant": 200.00,
                                             |      "reversePremiums": 200.00,
                                             |      "periodAmount": 200.00,
                                             |      "taxDeducted": 200.00,
                                             |      "otherIncome": 200.00
                                             |      }
                                             |   }
                                             |}
                                             |""".stripMargin)

    "update periodic submission" should {

      "update submissions data for the APIs used before TaxYear(2024)" in {
        val httpResponse = HttpResponse(NO_CONTENT, "")
        val taxYear = TaxYear(2021)

        stubPutHttpClientCall(
          s"/income-tax/business/property/periodic\\?taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId&submissionId=$submissionId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest.updatePeriodicSubmission(
            taxableEntityId,
            incomeSourceId,
            taxYear,
            submissionId,
            validUpdateUKPropertyPeriodicSubmissionRequest
          )(hc)
        ) shouldBe Right(None)
      }

      "update submissions data for TaxYear(2024) onwards" in {
        val httpResponse = HttpResponse(NO_CONTENT, "")
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/periodic/23-24\\?taxableEntityId=$nino&incomeSourceId=$incomeSourceId&submissionId=$submissionId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest.updatePeriodicSubmission(
            nino,
            incomeSourceId,
            taxYear,
            submissionId,
            validUpdateUKPropertyPeriodicSubmissionRequest
          )(hc)
        ) shouldBe Right(None)
      }

      "return not found from Upstream" in {
        val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(SingleErrorBody("some-code", "NotFound")).toString())
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/periodic/23-24\\?taxableEntityId=$nino&incomeSourceId=$incomeSourceId&submissionId=$submissionId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest.updatePeriodicSubmission(
            nino,
            incomeSourceId,
            taxYear,
            submissionId,
            validUpdateUKPropertyPeriodicSubmissionRequest
          )(hc)
        ) shouldBe Left(ApiError(NOT_FOUND, SingleErrorBody("some-code", "NotFound")))
      }

      "return unprocessable-entity from Upstream" in {
        val httpResponse = HttpResponse(
          UNPROCESSABLE_ENTITY,
          Json.toJson(SingleErrorBody("some-code", "unprocessable-entity")).toString()
        )
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/periodic/23-24\\?taxableEntityId=$nino&incomeSourceId=$incomeSourceId&submissionId=$submissionId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest.updatePeriodicSubmission(
            nino,
            incomeSourceId,
            taxYear,
            submissionId,
            validUpdateUKPropertyPeriodicSubmissionRequest
          )(hc)
        ) shouldBe Left(ApiError(UNPROCESSABLE_ENTITY, SingleErrorBody("some-code", "unprocessable-entity")))
      }

      "return Service Unavailable Error from Upstream" in {
        val httpResponse =
          HttpResponse(SERVICE_UNAVAILABLE, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())
        val taxYear = TaxYear(2024)

        stubPutHttpClientCall(
          s"/income-tax/business/property/periodic/23-24\\?taxableEntityId=$nino&incomeSourceId=$incomeSourceId&submissionId=$submissionId",
          requestBody.toString(),
          httpResponse
        )

        await(
          underTest.updatePeriodicSubmission(
            nino,
            incomeSourceId,
            taxYear,
            submissionId,
            validUpdateUKPropertyPeriodicSubmissionRequest
          )(hc)
        ) shouldBe
          Left(ApiError(SERVICE_UNAVAILABLE, SingleErrorBody("some-code", "some-reason")))
      }
    }
  }

  "Create Annual Foreign Property Annual Submission " should {

    val annualForeignPropertySubmission: AnnualForeignPropertySubmission = AnnualForeignPropertySubmission(
      foreignProperty = Some(
        Seq(
          AnnualForeignProperty(
            countryCode = "ESP",
            adjustments = None,
            allowances = Some(
              ForeignPropertyAllowances(
                zeroEmissionsCarAllowance = Some(1),
                zeroEmissionsGoodsVehicleAllowance = Some(2),
                costOfReplacingDomesticItems = Some(3),
                otherCapitalAllowance = Some(4),
                annualInvestmentAllowance = None,
                propertyAllowance = None,
                electricChargePointAllowance = None,
                structuredBuildingAllowance = None
              )
            )
          )
        )
      )
    )

    "for Year 2020-21 create annual foreign property submissions data for the APIs used before TaxYear(2024)" in {
      val taxYear = TaxYear(2021)
      val httpResponse = HttpResponse(NO_CONTENT, Json.toJson(annualForeignPropertySubmission).toString())
      stubPutHttpClientCall(
        s"/income-tax/business/property/annual\\?taxableEntityId=$taxableEntityId&taxYear=2020-21&incomeSourceId=$incomeSourceId",
        Json.toJson(annualForeignPropertySubmission).toString(),
        httpResponse
      )

      val propertyAnnualSubmission: PropertyAnnualSubmission =
        Json.toJson(annualForeignPropertySubmission).as[PropertyAnnualSubmission]
      await(
        underTest.createOrUpdateAnnualSubmission(
          taxYear,
          incomeSourceId,
          taxableEntityId,
          propertyAnnualSubmission
        )(hc)
      ) shouldBe Right((): Unit)
    }

    "for TaxYear(2024) onwards create annual foreign property submissions data" in {
      val taxYear = TaxYear(2024)

      val httpResponse = HttpResponse(NO_CONTENT, Json.toJson(annualForeignPropertySubmission).toString())

      stubPutHttpClientCall(
        s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
        Json.toJson(annualForeignPropertySubmission).toString(),
        httpResponse
      )

      await(
        underTest
          .createOrUpdateAnnualForeignPropertySubmission(
            taxYear,
            incomeSourceId,
            nino,
            annualForeignPropertySubmission
          )(hc)
      ) shouldBe Right((): Unit)
    }

    "for TaxYear(2024) onwards create annual foreign property submissions adjustments data" in {
      val taxYear = TaxYear(2024)
      val httpResponse = HttpResponse(NO_CONTENT, Json.toJson(annualForeignPropertySubmissionAdjustments).toString())

      stubPutHttpClientCall(
        s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
        Json.toJson(annualForeignPropertySubmissionAdjustments).toString(),
        httpResponse
      )

      await(
        underTest
          .createOrUpdateAnnualForeignPropertySubmissionAdjustments(
            taxYear,
            incomeSourceId,
            nino,
            annualForeignPropertySubmissionAdjustments
          )(hc)
      ) shouldBe Right((): Unit)
    }
    "for TaxYear(2024) onwards create annual foreign property submissions allowances data" in {
      val taxYear = TaxYear(2024)
      val httpResponse = HttpResponse(NO_CONTENT, Json.toJson(annualForeignPropertySubmissionAllowances).toString())

      stubPutHttpClientCall(
        s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
        Json.toJson(annualForeignPropertySubmissionAllowances).toString(),
        httpResponse
      )

      await(
        underTest
          .createOrUpdateAnnualForeignPropertySubmissionAllowances(
            taxYear,
            incomeSourceId,
            nino,
            annualForeignPropertySubmissionAllowances
          )(hc)
      ) shouldBe Right((): Unit)
    }

    "return Conflict from Upstream for annual foreign property" in {
      val httpResponse = HttpResponse(CONFLICT, Json.toJson(SingleErrorBody("some-code", "Conflict")).toString())
      val taxYear = TaxYear(2024)

      stubPutHttpClientCall(
        s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
        Json.toJson(annualForeignPropertySubmission).toString(),
        httpResponse
      )

      await(
        underTest
          .createOrUpdateAnnualForeignPropertySubmission(
            taxYear,
            incomeSourceId,
            nino,
            annualForeignPropertySubmission
          )(hc)
      ) shouldBe Left(ApiError(500, SingleErrorBody("some-code", "Conflict")))
    }
    "return Conflict from Upstream for annual foreign property submission adjustments" in {
      val httpResponse = HttpResponse(CONFLICT, Json.toJson(SingleErrorBody("some-code", "Conflict")).toString())
      val taxYear = TaxYear(2024)

      stubPutHttpClientCall(
        s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
        Json.toJson(annualForeignPropertySubmissionAdjustments).toString(),
        httpResponse
      )

      await(
        underTest
          .createOrUpdateAnnualForeignPropertySubmissionAdjustments(
            taxYear,
            incomeSourceId,
            nino,
            annualForeignPropertySubmissionAdjustments
          )(hc)
      ) shouldBe Left(ApiError(500, SingleErrorBody("some-code", "Conflict")))
    }
    "return Conflict from Upstream for annual foreign property submission allowances" in {
      val httpResponse = HttpResponse(CONFLICT, Json.toJson(SingleErrorBody("some-code", "Conflict")).toString())
      val taxYear = TaxYear(2024)

      stubPutHttpClientCall(
        s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
        Json.toJson(annualForeignPropertySubmissionAllowances).toString(),
        httpResponse
      )

      await(
        underTest
          .createOrUpdateAnnualForeignPropertySubmissionAllowances(
            taxYear,
            incomeSourceId,
            nino,
            annualForeignPropertySubmissionAllowances
          )(hc)
      ) shouldBe Left(ApiError(500, SingleErrorBody("some-code", "Conflict")))
    }

    "return not found from Upstream for annual foreign property" in {
      val httpResponse = HttpResponse(NOT_FOUND, Json.toJson(SingleErrorBody("some-code", "NotFound")).toString())
      val taxYear = TaxYear(2024)

      stubPutHttpClientCall(
        s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
        Json.toJson(annualForeignPropertySubmission).toString(),
        httpResponse
      )

      await(
        underTest
          .createOrUpdateAnnualForeignPropertySubmission(
            taxYear,
            incomeSourceId,
            nino,
            annualForeignPropertySubmission
          )(hc)
      ) shouldBe Left(ApiError(NOT_FOUND, SingleErrorBody("some-code", "NotFound")))
    }

    "return Service Unavailable Error from Upstream for annual foreign property" in {
      val httpResponse =
        HttpResponse(SERVICE_UNAVAILABLE, Json.toJson(SingleErrorBody("some-code", "some-reason")).toString())
      val taxYear = TaxYear(2024)

      stubPutHttpClientCall(
        s"/income-tax/business/property/annual/23-24/$nino/$incomeSourceId",
        Json.toJson(annualForeignPropertySubmission).toString(),
        httpResponse
      )

      await(
        underTest
          .createOrUpdateAnnualForeignPropertySubmission(
            taxYear,
            incomeSourceId,
            nino,
            annualForeignPropertySubmission
          )(hc)
      ) shouldBe
        Left(ApiError(SERVICE_UNAVAILABLE, SingleErrorBody("some-code", "some-reason")))
    }
  }

}
