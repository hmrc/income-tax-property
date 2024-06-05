/*
 * Copyright 2023 HM Revenue & Customs
 *
 */

package support.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.stubbing.StubMapping

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
