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
import org.scalatestplus.mockito.MockitoSugar.mock
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.duration.Duration

class AppConfigStub {

  def config(environment: String = "test", featureSwitchConfig: Option[FeatureSwitchConfig] = None): AppConfig = new AppConfigImpl(mock[ServicesConfig]) {
    private val wireMockPort = 11111

    private lazy val authorisationToken: String = "secret"

    override lazy val timeToLive: Int = Duration("28days").toDays.toInt

    override lazy val appName = "income-tax-property"

    override lazy val ifBaseUrl: String = s"http://localhost:$wireMockPort"
    override lazy val ifEnvironment: String = environment

    override lazy val propertyFrontendUrl = s"http://localhost:TEST/update-and-submit-income-tax-return/property"

    override lazy val hipBaseUrl: String = s"http://localhost:$wireMockPort"
    override lazy val hipEnvironment: String = environment

    override def authorisationTokenFor(apiVersion: String): String = authorisationToken + s".$apiVersion"
    override def hipAuthTokenFor(apiVersion: String): String = authorisationToken + s".$apiVersion"


    lazy val featureSwitches: FeatureSwitchConfig = featureSwitchConfig.getOrElse(FeatureSwitchConfig())

    override def enableHipApis: Boolean = featureSwitches.enableHipApis

  }
}
