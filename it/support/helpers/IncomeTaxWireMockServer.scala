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

package support.helpers

import com.github.tomakehurst.wiremock
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder

trait IncomeTaxWireMockServer {

  private val wiremockPort = 11111
  private val wiremockHost = "localhost"

  private lazy val wireMockServer = new wiremock.WireMockServer(wireMockConfig().port(wiremockPort))

  private lazy val connectedServices: Seq[String] = Seq("dividends")

  def servicesToUrlConfig: Seq[(String, String)] = connectedServices
    .map(service => s"microservice.services.$service.base-url" -> s"http://localhost:$wiremockPort")

  implicit lazy val application: Application = GuiceApplicationBuilder()
    .configure(("auditing.consumer.baseUri.port" -> wiremockPort) +: servicesToUrlConfig: _*)
    .build()

  def startWiremock(): Unit = {
    wireMockServer.start()
    WireMock.configureFor(wiremockHost, wiremockPort)
  }

  def stopWiremock(): Unit = wireMockServer.stop()
}
