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

package controllers

import play.api.http.Status.{BAD_REQUEST, NO_CONTENT}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Results.NoContent
import play.api.test.Helpers.status
import models.common.JourneyName.About
import models.request.PropertyAbout
import models.common.{BusinessId, JourneyContext, JourneyContextWithNino, Mtditid, Nino, TaxYear}
import utils.ControllerUnitTest
import utils.mocks.{MockAuthorisedAction, MockPropertyService}
import utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global

class JourneyAnswersControllerSpec extends ControllerUnitTest
  with MockPropertyService
  with MockAuthorisedAction
  with FakeRequestProvider {


  private val underTest = new JourneyAnswersController(
    mockPropertyService,
    mockAuthorisedAction,
    cc
  )

  val taxYear: TaxYear = TaxYear(2024)
  val businessId: BusinessId = BusinessId("someBusinessId")
  val nino: Nino = Nino("nino")
  val mtditid: Mtditid = Mtditid("1234567890")


  "create or update property about section" should {

    val validRequestBody: JsValue = Json.parse(
      """
        |{
        |   "totalIncome": "over",
        |   "ukProperty": ["property.rentals"]
        |}
        |""".stripMargin)
    val ctx: JourneyContext = JourneyContextWithNino(taxYear, businessId, mtditid, nino).toJourneyContext(About)


    "should return no_content for valid request body" in {

      mockAuthorisation()
      mockPersistAnswers(ctx, PropertyAbout("over", Seq("property.rentals"), Some(true)))
      val request = fakePostRequest.withJsonBody(validRequestBody)
      val result = await(underTest.savePropertyAbout(taxYear, businessId, nino)(request))
      result.header.status shouldBe NO_CONTENT
    }

    "should return bad request error when request body is empty" in {
      mockAuthorisation()
      val result = underTest.savePropertyAbout(taxYear, businessId, nino)(fakePostRequest)
      status(result) shouldBe BAD_REQUEST
    }
  }

}
