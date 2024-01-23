
package uk.gov.hmrc.incometaxproperty.connectors.response

import play.api.http.Status.{BAD_REQUEST, FAILED_DEPENDENCY, INTERNAL_SERVER_ERROR, NOT_FOUND, NO_CONTENT, OK, SERVICE_UNAVAILABLE}
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.incometaxproperty.connectors.response.PutAnnualSubmissionResponse.putAnnualSubmission
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiError, SingleErrorBody}
import uk.gov.hmrc.incometaxproperty.models.responses.PropertyAnnualSubmission
import uk.gov.hmrc.incometaxproperty.utils.UnitTest

import java.time.LocalDateTime

class PutAnnualSubmissionResponseSpec extends UnitTest{

    private val anyHeaders: Map[String, Seq[String]] = Map.empty
    private val anyMethod: String = "PUT"
    private val anyUrl = "/income-tax/business/property/"

    private val underTest = putAnnualSubmission

    "getPropertyAnnualSubmissionDataReads" should {

      "convert JsValue to GetPropertyAnnualSubmissionResponse" when {

        "status is NO_CONTENT and valid jsValue" in {
          val propertyAnnualSubmission = PropertyAnnualSubmission(
            submittedOn = LocalDateTime.now,
            None, None, None, None
          )

          val httpResponse: HttpResponse = HttpResponse.apply(NO_CONTENT, Json.toJson(propertyAnnualSubmission).toString, anyHeaders)

          underTest.read(anyMethod, anyUrl, httpResponse) shouldBe PutAnnualSubmissionResponse(
            httpResponse,
            Right()
          )
        }

        "status is OK and invalid jsValue" in {
          val jsValue: JsValue = Json.parse(
            """
              |{
              |   "PropertyAnnualSubmission": {"value": []}
              |}
              |""".stripMargin)

          val httpResponse: HttpResponse = HttpResponse.apply(OK, jsValue, anyHeaders)

          underTest.read(anyMethod, anyUrl, httpResponse) shouldBe PutAnnualSubmissionResponse(
            httpResponse,
            Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody.parsingError))
          )
        }

        "status is NOT_FOUND and any jsValue" in {
          val jsValue: JsValue = Json.toJson(SingleErrorBody("some-code", "some-reason"))

          val httpResponse: HttpResponse = HttpResponse.apply(NOT_FOUND, jsValue, anyHeaders)

          underTest.read(anyMethod, anyUrl, httpResponse) shouldBe PutAnnualSubmissionResponse(
            httpResponse,
            Left(ApiError(NOT_FOUND, SingleErrorBody("some-code", "some-reason")))
          )
        }

        "status is INTERNAL_SERVER_ERROR and jsValue for error" in {
          val jsValue: JsValue = Json.toJson(SingleErrorBody("some-code", "some-reason"))

          val httpResponse: HttpResponse = HttpResponse.apply(INTERNAL_SERVER_ERROR, jsValue, anyHeaders)

          underTest.read(anyMethod, anyUrl, httpResponse) shouldBe PutAnnualSubmissionResponse(
            httpResponse,
            Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("some-code", "some-reason")))
          )
        }

        "status is SERVICE_UNAVAILABLE and jsValue for error" in {
          val jsValue: JsValue = Json.toJson(SingleErrorBody("some-code", "some-reason"))

          val httpResponse: HttpResponse = HttpResponse.apply(SERVICE_UNAVAILABLE, jsValue, anyHeaders)

          underTest.read(anyMethod, anyUrl, httpResponse) shouldBe PutAnnualSubmissionResponse(
            httpResponse,
            Left(ApiError(SERVICE_UNAVAILABLE, SingleErrorBody("some-code", "some-reason")))
          )
        }

        "status is BAD_REQUEST and jsValue for error" in {
          val jsValue: JsValue = Json.toJson(SingleErrorBody("some-code", "some-reason"))

          val httpResponse: HttpResponse = HttpResponse.apply(BAD_REQUEST, jsValue, anyHeaders)

          underTest.read(anyMethod, anyUrl, httpResponse) shouldBe PutAnnualSubmissionResponse(
            httpResponse,
            Left(ApiError(BAD_REQUEST, SingleErrorBody("some-code", "some-reason")))
          )
        }

        "status is OTHER and jsValue for error" in {
          val jsValue: JsValue = Json.toJson(SingleErrorBody("some-code", "some-reason"))

          val httpResponse: HttpResponse = HttpResponse.apply(FAILED_DEPENDENCY, jsValue, anyHeaders)

          underTest.read(anyMethod, anyUrl, httpResponse) shouldBe PutAnnualSubmissionResponse(
            httpResponse,
            Left(ApiError(INTERNAL_SERVER_ERROR, SingleErrorBody("some-code", "some-reason")))
          )
        }
      }
    }
}
