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

package support.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import play.api.http.Status.OK
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel}

object AuthStub {

  def userLoggedInITPUser(testUserJson: String, retrievals: String = Retrievals): StubMapping =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        //.withRequestBody(equalToJson(retrievals))
        .willReturn(
          aResponse()
            .withStatus(250)
            .withBody(testUserJson)
        )
    )

  def userLoggedInIsNotITPUser(error: String): StubMapping =
    stubFor(
      post(urlEqualTo("/auth/authorise"))
        .withRequestBody(equalToJson(Retrievals))
        .willReturn(
          unauthorized.withHeader("WWW-Authenticate", s"""MDTP detail="$error"""")
        )
    )

  def userVerifiedWithInternalAuth(): StubMapping =
    stubFor(
      post(urlPathMatching("/internal-auth/auth"))
        .willReturn(
          aResponse().withStatus(200)
        )
    )

  private def successfulAuthResponse(affinityGroup: Option[AffinityGroup], confidenceLevel: Option[ConfidenceLevel], enrolments: JsObject*): JsObject = {
    affinityGroup.fold(Json.obj())(unwrappedAffinityGroup => Json.obj("affinityGroup" -> unwrappedAffinityGroup)) ++
      confidenceLevel.fold(Json.obj())(unwrappedConfidenceLevel => Json.obj("confidenceLevel" -> unwrappedConfidenceLevel)) ++
      Json.obj("allEnrolments" -> enrolments)
  }

  def authorised(response: JsObject = successfulAuthResponse(Some(Individual), Some(ConfidenceLevel.L250), mtditEnrolment, ninoEnrolment)): StubMapping = {
    stubFor(post(urlMatching("/auth/authorise"))
      .willReturn(
        aResponse()
          .withStatus(OK)
          .withBody(response.toString())
          .withHeader("Content-Type", "application/json; charset=utf-8")))
  }

  private val mtditEnrolment = Json.obj(
    "key" -> "HMRC-MTD-IT",
    "identifiers" -> Json.arr(
      Json.obj(
        "key" -> "MTDITID",
        "value" -> "555555555"
      )
    )
  )

  private val ninoEnrolment = Json.obj(
    "key" -> "HMRC-NI",
    "identifiers" -> Json.arr(
      Json.obj(
        "key" -> "NINO",
        "value" -> "AA123123A"
      )
    )
  )

  private val Retrievals: String =
    """
          |{
          |  "authorise": [
          |    {
          |      "$or": [
          |        {
          |          "affinityGroup": "Individual"
          |        },
          |        {
          |          "affinityGroup": "Organisation"
          |        }
          |      ]
          |    },
          |    {
          |      "authProviders": [
          |        "GovernmentGateway"
          |      ]
          |    },
          |    {
          |      "confidenceLevel": "L250"
          |    }
          |  ],
          |  "retrieve": [
          |    "nino",
          |    "internalId"
          |  ]
          |}""".stripMargin
}
