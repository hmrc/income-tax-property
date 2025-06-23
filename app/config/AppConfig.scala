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

package config

import com.google.inject.ImplementedBy
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.Duration

@ImplementedBy(classOf[AppConfigImpl])
trait AppConfig {
  def appName: String

  def ifBaseUrl: String

  def timeToLive: Int

  def ifEnvironment: String

  def propertyFrontendUrl: String

  def authorisationTokenKey: String

  def authorisationTokenFor(apiVersion: String): String

  def hipAuthTokenKey: String

  def hipAuthTokenFor(apiVersion: String): String

  def enableHipApis: Boolean

  def hipBaseUrl: String

  def hipEnvironment: String
}

@Singleton
class AppConfigImpl @Inject()(config: ServicesConfig) extends AppConfig {

  override lazy val appName: String = config.getString("appName")
  override lazy val ifBaseUrl: String = config.baseUrl(serviceName = "integration-framework")
  override lazy val timeToLive: Int = Duration(config.getString("mongodb.timeToLive")).toDays.toInt
  override lazy val propertyFrontendUrl: String =
    s"${config.getString("microservice.services.income-tax-property-frontend.url")}/update-and-submit-income-tax-return/property"

  override def ifEnvironment: String = config.getString("microservice.services.integration-framework.environment")

  override lazy val authorisationTokenKey: String = "microservice.services.integration-framework.authorisation-token"

  override def authorisationTokenFor(apiVersion: String): String =
    config.getString(authorisationTokenKey + s".$apiVersion")

  override lazy val hipBaseUrl: String = config.baseUrl(serviceName = "hybrid-integration-platform")

  override def hipEnvironment: String = config.getString("microservice.services.hybrid-integration-platform.environment")

  override lazy val hipAuthTokenKey: String = "microservice.services.hybrid-integration-platform.authorisation-token"

  override def hipAuthTokenFor(apiVersion: String): String =
    config.getString(hipAuthTokenKey + s".$apiVersion")

  override def enableHipApis: Boolean = config.getBoolean("feature-switch.enableHipApis")
}
