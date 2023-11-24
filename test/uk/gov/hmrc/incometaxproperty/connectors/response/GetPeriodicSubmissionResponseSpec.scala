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

package uk.gov.hmrc.incometaxproperty.connectors.response
import play.api.http.Status._
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.incometaxproperty.connectors.response.GetPeriodicSubmissionResponse.getPeriodicSubmissionDataReads
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiError, SingleErrorBody}
import uk.gov.hmrc.incometaxproperty.models.responses.{PeriodicSubmissionIdModel, PeriodicSubmissionModel}
import uk.gov.hmrc.incometaxproperty.utils.UnitTest

class GetPeriodicSubmissionResponseSpec extends UnitTest {

  private val anyHeaders: Map[String, Seq[String]] = Map.empty
  private val anyMethod: String = "GET"
  private val anyUrl = "/income-tax/business/property/"

  private val underTest = getPeriodicSubmissionDataReads

  "getPeriodicSubmissionDataReads" should {
    "convert JsValue to GetPeriodicSubmissionResponse" when {
      "status is OK and valid jsValue" in {
        val periodicSubmissionModel = PeriodicSubmissionModel(
          List(
            PeriodicSubmissionIdModel("1", "2022-12-10", "2021-11-11"),
            PeriodicSubmissionIdModel("2", "2023-11-03", "2023-05-12")
          )
        )

        val httpResponse: HttpResponse = HttpResponse.apply(OK, periodicSubmissionModel.toJson.toString, anyHeaders)

        underTest.read(anyMethod, anyUrl, httpResponse) shouldBe GetPeriodicSubmissionResponse(
          httpResponse,
          Right(periodicSubmissionModel)
        )
      }

      "status is OK and invalid jsValue" in {
        val jsValue: JsValue = Json.parse(
          """
            |{
            |   "PeriodicSubmission": {"submissionId": []}
            |}
            |""".stripMargin)

        val httpResponse: HttpResponse = HttpResponse.apply(OK, jsValue, anyHeaders)

        underTest.read(anyMethod, anyUrl, httpResponse) shouldBe GetPeriodicSubmissionResponse(
          httpResponse,
          Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody.parsingError))
        )
      }

      "status is NOT_FOUND and any jsValue" in {
        val jsValue: JsValue = Json.parse(
          """
            |{}
            |""".stripMargin)

        val httpResponse: HttpResponse = HttpResponse.apply(NOT_FOUND, jsValue, anyHeaders)

        underTest.read(anyMethod, anyUrl, httpResponse) shouldBe GetPeriodicSubmissionResponse(
          httpResponse,
          Right(PeriodicSubmissionModel(List.empty))
        )
      }

      "status is INTERNAL_SERVER_ERROR and jsValue for error" in {
        val jsValue: JsValue = Json.toJson(SingleErrorBody("some-code", "some-reason"))

        val httpResponse: HttpResponse = HttpResponse.apply(INTERNAL_SERVER_ERROR, jsValue, anyHeaders)

        underTest.read(anyMethod, anyUrl, httpResponse) shouldBe GetPeriodicSubmissionResponse(
          httpResponse,
          Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("some-code", "some-reason")))
        )
      }

      "status is SERVICE_UNAVAILABLE and jsValue for error" in {
        val jsValue: JsValue = Json.toJson(SingleErrorBody("some-code", "some-reason"))

        val httpResponse: HttpResponse = HttpResponse.apply(SERVICE_UNAVAILABLE, jsValue, anyHeaders)

        underTest.read(anyMethod, anyUrl, httpResponse) shouldBe GetPeriodicSubmissionResponse(
          httpResponse,
          Left(ApiError(SERVICE_UNAVAILABLE, SingleErrorBody("some-code", "some-reason")))
        )
      }

      "status is BAD_REQUEST and jsValue for error" in {
        val jsValue: JsValue = Json.toJson(SingleErrorBody("some-code", "some-reason"))

        val httpResponse: HttpResponse = HttpResponse.apply(BAD_REQUEST, jsValue, anyHeaders)

        underTest.read(anyMethod, anyUrl, httpResponse) shouldBe GetPeriodicSubmissionResponse(
          httpResponse,
          Left(ApiError(BAD_REQUEST, SingleErrorBody("some-code", "some-reason")))
        )
      }

      "status is OTHER and jsValue for error" in {
        val jsValue: JsValue = Json.toJson(SingleErrorBody("some-code", "some-reason"))

        val httpResponse: HttpResponse = HttpResponse.apply(FAILED_DEPENDENCY, jsValue, anyHeaders)

        underTest.read(anyMethod, anyUrl, httpResponse) shouldBe GetPeriodicSubmissionResponse(
          httpResponse,
          Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("some-code", "some-reason")))
        )
      }
    }
  }
}
