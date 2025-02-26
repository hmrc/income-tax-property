/*
 * Copyright 2025 HM Revenue & Customs
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

package controllers

import models.{BusinessDetailsResponse, PropertyDetails}
import models.errors.{ApiServiceError, DataNotFoundError}
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.Helpers.{contentAsJson, status}
import utils.ControllerUnitTest
import uk.gov.hmrc.http.HeaderCarrier
import utils.mocks.{MockAuthorisedAction, MockBusinessDetailsService}
import utils.providers.FakeRequestProvider

import scala.concurrent.{ExecutionContext, Future}

class PrePopulationControllerSpec extends ControllerUnitTest
  with MockBusinessDetailsService
  with MockAuthorisedAction
  with FakeRequestProvider {

  trait Test {
    val taxYear: Int = 2024
    val nino: String = "AA111111A"

    implicit val hc: HeaderCarrier = HeaderCarrier()
    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

    val controller = new PrePopulationController(
      service = mockIntegrationFrameworkService,
      auth = mockAuthorisedAction,
      cc = cc
    )

    mockAuthorisation()
  }

  "get" when {
    "BusinessDetailsService returns an error" should {
      "return an error" in new Test {
        mockGetBusinessDetails(nino, Left(ApiServiceError(500)))

        val result: Future[Result] = controller.get(nino)(fakeGetRequest)
        status(result) shouldBe 500
      }
    }
    "BusinessDetailsService returns DataNotFoundError" should {
      "return false" in new Test {
        mockGetBusinessDetails(nino, Left(DataNotFoundError))

        val result: Future[Result] = controller.get(nino)(fakeGetRequest)
        status(result) shouldBe 200

        contentAsJson(result) shouldBe
          Json.parse(
            """
              |{
              |   "hasUkPropertyPrePop": false,
              |   "hasForeignPropertyPrePop": false
              |}
          """.stripMargin
          )
      }
    }

    "prePopulationService returns a pre pop response" should {
      "return it" in new Test {
        val someProperty: Seq[PropertyDetails] = List(
          PropertyDetails(Some("uk-property"), None, None, "XYIS00000451267"),
          PropertyDetails(Some("foreign-property"), None, None, "XYIS00000451268")
        )

        mockGetBusinessDetails(
          nino,
          Right(BusinessDetailsResponse(someProperty))
        )

        val result: Future[Result] = controller.get(nino)(fakeGetRequest)
        status(result) shouldBe 200
        contentAsJson(result) shouldBe
          Json.parse(
            """
              |{
              |   "hasUkPropertyPrePop": true,
              |   "hasForeignPropertyPrePop": true
              |}
          """.stripMargin
          )
      }
    }
  }

}
