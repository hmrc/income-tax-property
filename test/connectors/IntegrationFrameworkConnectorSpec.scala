/*
 * Copyright 2024 HM Revenue & Customs
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

import config.AppConfig
import connectors.response.{PostPeriodicSubmissionResponse, PutAnnualSubmissionResponse, PutPeriodicSubmissionResponse}
import models.common.{IncomeSourceId, Nino, TaxYear}
import models.errors.ApiError
import models.responses._
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.{Json, Writes}
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpReads, HttpResponse}
import utils.UnitTest

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class IntegrationFrameworkConnectorSpec extends UnitTest with MockFactory {
  val mockHttpClient: HttpClient = mock[HttpClient]
  val appConf: AppConfig = mock[AppConfig]
  val validPropertyPeriodicSubmissionRequest = PropertyPeriodicSubmissionRequest(Some(ForeignFhlEea(ForeignFhlIncome(200.00), ForeignFhlExpenses(None, None, None, None, None, None, None, Some(1000.99)))), None, None, None)

  val integrationFrameworkConnector = new IntegrationFrameworkConnector(mockHttpClient, appConf)
  "IntegrationFrameworkController" should {
    "create periodic submission" in {

      val validRequestBody: String =
        """
          |{
          |   "foreignFhlEea": {
          |      "income": {
          |         "rentAmount": 200.00
          |      },
          |      "expenses": {
          |         "consolidatedExpenseAmount": 1000.99
          |       }
          |   }
          |}
          |""".stripMargin

      (appConf.baseUrl(_: String)).expects("integration-framework").returning("http://something:1234/integration-framework/some-endpoint/")
      (appConf.authorisationTokenFor(_: String)).expects(*).returning("1234")
      (appConf.ifEnvironment _).expects().returning("abcd")
      (
        mockHttpClient
          .POST(_: String, _: PropertyPeriodicSubmissionRequest, _: Seq[(String, String)])(
            _: Writes[PropertyPeriodicSubmissionRequest],
            _: HttpReads[PostPeriodicSubmissionResponse],
            _: HeaderCarrier,
            _: ExecutionContext
          )
        ).expects(
        *,
        *,
        *,
        *,
        *,
        *,
        *
      )
        .returning(Future.successful(PostPeriodicSubmissionResponse(
          HttpResponse(204, validRequestBody), Right(Some(PeriodicSubmissionId("124"))))))
        .once()

      implicit val hc = HeaderCarrier()
      val returnValue: Either[ApiError, Option[PeriodicSubmissionId]] = await(
        integrationFrameworkConnector
          .createPeriodicSubmission(
            2000,
            "",
            "",
            validPropertyPeriodicSubmissionRequest
          ))
      returnValue shouldBe Right(Some(PeriodicSubmissionId("124")))
    }
    "update periodic submission" in {

      val validRequestBody: String =
        """
          |{
          |   "foreignFhlEea": {
          |      "income": {
          |         "rentAmount": 200.00
          |      },
          |      "expenses": {
          |         "consolidatedExpenseAmount": 1000.99
          |       }
          |   }
          |}
          |""".stripMargin

      (appConf.authorisationTokenFor(_: String)).expects(*).returning("1234")
      (appConf.ifEnvironment _).expects().returning("abcd")
      (
        mockHttpClient
          .PUT(_: String, _: PropertyPeriodicSubmissionRequest, _: Seq[(String, String)])(
            _: Writes[PropertyPeriodicSubmissionRequest],
            _: HttpReads[PutPeriodicSubmissionResponse],
            _: HeaderCarrier,
            _: ExecutionContext
          )
        ).expects(
        *,
        *,
        *,
        *,
        *,
        *,
        *
      )
        .returning(Future.successful(PutPeriodicSubmissionResponse(
          HttpResponse(204, validRequestBody), Right(Some("124")))))
        .once()

      implicit val hc = HeaderCarrier()
      val returnValue: Either[ApiError, Option[String]] = await(
        integrationFrameworkConnector
          .updatePeriodicSubmission(
            "",
            "",
            2000,
            "124",
            validPropertyPeriodicSubmissionRequest
          ))
      returnValue shouldBe Right(Some("124"))
    }
    "create or update annual submission" in {

      val validRequestBody = PropertyAnnualSubmission(
        submittedOn = Some(LocalDateTime.now),
        Some(AnnualForeignFhlEea(
          ForeignFhlAdjustments(1, 2, periodOfGraceAdjustment = false),
          ForeignFhlAllowances(Some(1), Some(2), Some(3), Some(4), Some(5))
        )), None, None, None)

      (appConf.authorisationTokenFor(_: String)).expects(*).returning("1234")
      (appConf.ifEnvironment _).expects().returning("abcd")
      (
        mockHttpClient.PUT(_: String, _: PropertyAnnualSubmission, _: Seq[(String, String)])(
          _: Writes[PropertyAnnualSubmission],
          _: HttpReads[PutAnnualSubmissionResponse],
          _: HeaderCarrier,
          _: ExecutionContext
        )).expects(
        *,
        *,
        *,
        *,
        *,
        *,
        *
      )
        .returning(Future.successful(PutAnnualSubmissionResponse(
          HttpResponse(204, Json.toJson(validRequestBody).toString()), Right(())
        )
        )
        )
        .once()

      implicit val hc = HeaderCarrier()
      val returnValue: Either[ApiError, Unit] = await(
        integrationFrameworkConnector
          .createOrUpdateAnnualSubmission(
            TaxYear(2000),
            IncomeSourceId(""),
            Nino(""),
            validRequestBody
          ))
      returnValue shouldBe Right(())
    }
  }
}
