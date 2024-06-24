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

package controllers

import actions.AuthorisationRequest
import models.User
import models.common.{IncomeSourceId, JourneyName, Nino, TaxYear}
import models.request.{PremiumsGrantLease, PropertyRentalsIncome}
import org.apache.pekko.util.ByteString
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Logging
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR, OK}
import play.api.libs.json.{JsNull, JsUndefined, JsValue, Json}
import play.api.mvc.Results.Ok
import play.api.mvc.{AnyContent, Request}
import play.api.test.Helpers.status
import utils.ControllerUnitTest
import utils.mocks.MockAuthorisedAction
import utils.providers.FakeRequestProvider

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class RequestHandlerSpec
    extends ControllerUnitTest with MockAuthorisedAction with FakeRequestProvider with ScalaCheckPropertyChecks {

  val requestHandler = new RequestHandler with Logging {}
  val validRequestBody: JsValue = Json.parse("""
                                               |{
                                               |            "incomeFromPropertyRentals" : 15,
                                               |            "isNonUKLandlord" : false,
                                               |            "otherIncomeFromProperty" : 25,
                                               |            "deductingTax" : {
                                               |                "taxDeductedYesNo" : true,
                                               |                "taxDeductedAmount" : 20
                                               |            },
                                               |            "premiumsGrantLease" : {
                                               |                "premiumsGrantLeaseYesOrNo" : true,
                                               |                "premiumsGrantLease" : 5
                                               |            },
                                               |            "reversePremiumsReceived" : {
                                               |                "reversePremiumsReceived" : true,
                                               |                "amount" : 10
                                               |            }
                                               |        }
                                               |""".stripMargin)

  "RequestHandler" should {
    "handle errors and parsing correctly" in {
      val scenarios = Table[Request[AnyContent], Int, String](
        ("Request", "Expected Status", "Expected Message"),
        // (fakeRequest.withHeaders(("Content-Type", "application/json")).withRawBody(ByteString("test")), BAD_REQUEST, "Cannot parse JSON"),
        (fakeRequest.withJsonBody(validRequestBody), OK, "Success"),
        (fakeRequest.withJsonBody(Json.toJson(PremiumsGrantLease(true, Some(12.34)))), BAD_REQUEST, "Cannot read JSON")
      )

      forAll(scenarios) { (request: Request[AnyContent], expectedStatus: Int, expectedMessage: String) =>
        val result = requestHandler.withJourneyContextAndEntity[PropertyRentalsIncome](
          TaxYear(2023),
          IncomeSourceId(""),
          Nino(""),
          JourneyName.NoJourney,
          AuthorisationRequest[AnyContent](User("", None), request)
        ) { (_, _) =>
          Future.successful(Ok("Success"))
        }

        await(result.map(a => consumeBody(a))).contains(expectedMessage) shouldBe true
        status(result) shouldBe expectedStatus
      }
    }
  }
}
