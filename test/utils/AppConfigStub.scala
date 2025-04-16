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

package utils

import config.{AppConfig, AppConfigImpl}
import org.scalamock.scalatest.MockFactory
import play.api.Configuration

import scala.concurrent.duration.Duration

class AppConfigStub extends MockFactory {

  def config(environment: String = "test"): AppConfig = new AppConfigImpl(mock[Configuration]) {
    private val wireMockPort = 11111

    private lazy val authorisationToken: String = "secret"

    override lazy val timeToLive: Int = Duration("28days").toDays.toInt

    override lazy val appName = "income-tax-property"

    override lazy val ifBaseUrl: String = s"http://localhost:$wireMockPort"
    override lazy val ifEnvironment: String = environment

    override lazy val propertyFrontendUrl = s"http://localhost:TEST/update-and-submit-income-tax-return/property"

    override def authorisationTokenFor(apiVersion: String): String = authorisationToken + s".$apiVersion"
  }
}
