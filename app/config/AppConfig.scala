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
import play.api.Configuration

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
  def hipMigration1502Enabled: Boolean
  def hipBaseUrl: String
  def hipEnvironment: String
  def baseUrl(serviceName: String): String
  protected def rootServices: String
  protected def defaultProtocol: String
  def getConfString(confKey: String, defString: => String): String
  def getConfInt(confKey: String, defInt: => Int): Int
  def throwConfigNotFoundError(key: String): RuntimeException
  def hipMigration1500Enabled: Boolean
  def hipMigration1501Enabled: Boolean
  def hipMigration5266Enabled: Boolean
}

@Singleton
class AppConfigImpl @Inject() (config: Configuration) extends AppConfig {

  override lazy val appName: String = config.get[String]("appName")
  override lazy val ifBaseUrl: String = baseUrl(serviceName = "integration-framework")
  override lazy val timeToLive: Int = Duration(config.get[String]("mongodb.timeToLive")).toDays.toInt
  override lazy val propertyFrontendUrl: String =
    s"${config.get[String]("microservice.services.income-tax-property-frontend.url")}/update-and-submit-income-tax-return/property"

  override def ifEnvironment: String = config.get[String]("microservice.services.integration-framework.environment")

  override lazy val authorisationTokenKey: String = "microservice.services.integration-framework.authorisation-token"

  override def authorisationTokenFor(apiVersion: String): String =
    config.get[String](authorisationTokenKey + s".$apiVersion")


  override lazy val hipBaseUrl: String = baseUrl(serviceName = "hip-integration-framework")

  override def hipEnvironment: String = config.get[String]("microservice.services.hip-integration-framework.environment")

  override lazy val hipAuthTokenKey: String = "microservice.services.hip-integration-framework.authorisation-token"

  override def hipAuthTokenFor(apiVersion: String): String =
    config.get[String](hipAuthTokenKey + s".$apiVersion")

  override def baseUrl(serviceName: String): String = {
    val protocol = getConfString(s"$serviceName.protocol", defaultProtocol)
    val host = getConfString(s"$serviceName.host", throwConfigNotFoundError(s"$serviceName.host"))
    val port = getConfInt(s"$serviceName.port", throwConfigNotFoundError(s"$serviceName.port"))
    s"$protocol://$host:$port"
  }

  override protected lazy val rootServices = "microservice.services"

  override protected lazy val defaultProtocol: String =
    config
      .getOptional[String](s"$rootServices.protocol")
      .getOrElse("http")

  override def getConfString(confKey: String, defString: => String): String =
    config
      .getOptional[String](s"$rootServices.$confKey")
      .getOrElse(defString)

  override def getConfInt(confKey: String, defInt: => Int): Int =
    config
      .getOptional[Int](s"$rootServices.$confKey")
      .getOrElse(defInt)

  override def throwConfigNotFoundError(key: String) =
    throw new RuntimeException(s"Could not find config key '$key'")

  override def hipMigration1502Enabled: Boolean = config.get[Boolean]("feature-switch.hip-migration.api-1502-enabled")

  override lazy val hipMigration1500Enabled: Boolean = config.get[Boolean]("feature-switch.hip-migration.api-1500-enabled")

  override lazy val hipMigration1501Enabled: Boolean = config.get[Boolean]("feature-switch.hip-migration.api-1501-enabled")

  override lazy val hipMigration5266Enabled: Boolean = config.get[Boolean]("feature-switch.hip-migration.api-5266-enabled")

}
