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

package uk.gov.hmrc.incometaxproperty.connectors

import play.api.Logging
import play.api.libs.json.{JsValue, StaticBinding}
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpReads, StringContextOps}
import uk.gov.hmrc.incometaxproperty.config.AppConfig
import uk.gov.hmrc.incometaxproperty.connectors.response.GetBusinessDetailsResponse.getBusinessDetailsResponseReads
import uk.gov.hmrc.incometaxproperty.connectors.response._
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiError, SingleErrorBody}
import uk.gov.hmrc.incometaxproperty.models.responses._

import java.net.URL
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class IntegrationFrameworkConnector @Inject()(httpClient: HttpClient, appConf: AppConfig)
                                             (implicit ec: ExecutionContext) extends IFConnector with Logging {


  override protected[connectors] val appConfig: AppConfig = appConf

  def getBusinessDetails(nino: String)
                        (implicit hc: HeaderCarrier): Future[Either[ApiError, Option[IncomeSourceDetailsModel]]] = {
    val url = new URL(s"${appConfig.ifBaseUrl}/registration/business-details/nino/$nino")
    val apiVersion = "1171"

    callGetBusinessDetails(url)(ifHeaderCarrier(url, apiVersion)).map(_.result)
  }

  private def callGetBusinessDetails(url: URL)(implicit hc: HeaderCarrier): Future[GetBusinessDetailsResponse] = {
    httpClient.GET[GetBusinessDetailsResponse](url)
  }

  def getAllPeriodicSubmission(taxYear: Int,
                               taxableEntityId: String,
                               incomeSourceId: String)
                              (implicit hc: HeaderCarrier): Future[Either[ApiError, List[PeriodicSubmissionIdModel]]] = {

    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (new URL(s"${appConfig.ifBaseUrl}/income-tax/business/property/${toTaxYearParamAfter2324(taxYear)}/$taxableEntityId/$incomeSourceId/period"), "1954")
    } else {
      (new URL(
        s"${appConfig.ifBaseUrl}/income-tax/business/property/$taxableEntityId/$incomeSourceId/period?taxYear=${toTaxYearParamBefore2324(taxYear)}"),
        "1649")
    }

    val apiResponse = httpClient.GET[GetPeriodicSubmissionIdResponse](url)(
      implicitly[HttpReads[GetPeriodicSubmissionIdResponse]],
      ifHeaderCarrier(url, apiVersion),
      ec)
    apiResponse.map { response =>
      if (response.result.isLeft) {
        Left(ApiError(response.httpResponse.status, SingleErrorBody(response.getClass.getSimpleName, response.httpResponse.body)))
      } else {
        response.result
      }
    }
  }

  def getPropertyPeriodicSubmission(taxYear: Int, nino: String, incomeSourceId: String, submissionId: String)
                                   (implicit hc: HeaderCarrier): Future[Either[ApiError, Option[PropertyPeriodicSubmission]]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (new URL(s"${appConfig.ifBaseUrl}/income-tax/business/property/${toTaxYearParamAfter2324(taxYear)}/$nino/$incomeSourceId/periodic/$submissionId"), "1862")
    } else {
      (new URL(s"${appConfig.ifBaseUrl}/income-tax/business/property/periodic?" +
        s"taxableEntityId=$nino&taxYear=${toTaxYearParamBefore2324(taxYear)}&incomeSourceId=$incomeSourceId&submissionId=$submissionId"), "1595")
    }

    httpClient.GET[GetPropertyPeriodicSubmissionResponse](url)(
      implicitly[HttpReads[GetPropertyPeriodicSubmissionResponse]],
      ifHeaderCarrier(url, apiVersion),
      ec).map { response: GetPropertyPeriodicSubmissionResponse =>
      if (response.result.isLeft) {
        val correlationId = response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
        logger.error(s"Error getting a property periodic submission from the Integration Framework:" +
          s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}")
      }
      response.result
    }
  }

  def getPropertyAnnualSubmission(taxYear: Int, nino: String, incomeSourceId: String)
                                 (implicit hc: HeaderCarrier): Future[Either[ApiError, Option[PropertyAnnualSubmission]]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (new URL(s"${appConfig.ifBaseUrl}/income-tax/business/property/annual/${toTaxYearParamAfter2324(taxYear)}/$nino/$incomeSourceId"), "1805")
    } else {
      (new URL(s"${appConfig.ifBaseUrl}/income-tax/business/property/annual?" +
        s"taxableEntityId=$nino&taxYear=${toTaxYearParamBefore2324(taxYear)}&incomeSourceId=$incomeSourceId"), "1598")
    }

    httpClient.GET[GetPropertyAnnualSubmissionResponse](url)(
      implicitly[HttpReads[GetPropertyAnnualSubmissionResponse]],
      ifHeaderCarrier(url, apiVersion),
      ec).map { response: GetPropertyAnnualSubmissionResponse =>
      if (response.result.isLeft) {
        val correlationId = response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
        logger.error(s"Error getting a property annual submission from the Integration Framework:" +
          s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}")
      }
      response.result
    }
  }

  def createPeriodicSubmission(taxYear: Int, nino: String, incomeSourceId: String, body: JsValue)
                              (implicit hc: HeaderCarrier): Future[Either[ApiError, Option[PeriodicSubmissionId]]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (url"""${appConfig.ifBaseUrl}/income-tax/business/property/periodic/${toTaxYearParamAfter2324(taxYear)}?taxableEntityId=$nino&incomeSourceId=$incomeSourceId""", "1861")
    } else {
      (url"""${appConfig.ifBaseUrl}/income-tax/business/property/periodic?taxableEntityId=$nino&taxYear=${toTaxYearParamBefore2324(taxYear)}&incomeSourceId=$incomeSourceId""", "1593")
    }

    httpClient.POSTString[PostPeriodicSubmissionResponse](url, StaticBinding.generateFromJsValue(body, escapeNonASCII = false))(
      implicitly[HttpReads[PostPeriodicSubmissionResponse]],
      ifHeaderCarrier(url, apiVersion).withExtraHeaders(headers = "Content-Type" -> "application/json"),
      ec).map { response: PostPeriodicSubmissionResponse =>
      if (response.result.isLeft) {
        val correlationId = response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
        logger.error(s"Error creating a property periodic submission from the Integration Framework:" +
          s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}")
      }
      response.result
    }
  }

  def updatePeriodicSubmission(nino: String, incomeSourceId: String, taxYear: Int, submissionId: String, body: JsValue)
                              (implicit hc: HeaderCarrier): Future[Either[ApiError, Option[String]]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (url"""${appConfig.ifBaseUrl}/income-tax/business/property/periodic/${toTaxYearParamAfter2324(taxYear)}?taxableEntityId=$nino&incomeSourceId=$incomeSourceId&submissionId=$submissionId""", "1958")
    } else {
      (url"""${appConfig.ifBaseUrl}/income-tax/business/property/periodic?taxableEntityId=$nino&taxYear=${toTaxYearParamBefore2324(taxYear)}&incomeSourceId=$incomeSourceId&submissionId=$submissionId""", "1594")
    }

    httpClient.PUTString[PutPeriodicSubmissionResponse](url, StaticBinding.generateFromJsValue(body, escapeNonASCII = false))(
      implicitly[HttpReads[PutPeriodicSubmissionResponse]],
      ifHeaderCarrier(url, apiVersion).withExtraHeaders(headers = "Content-Type" -> "application/json"),
      ec).map { response: PutPeriodicSubmissionResponse =>
      if (response.result.isLeft) {
        val correlationId = response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
        logger.error(s"Error updating a property periodic submission from the Integration Framework:" +
          s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}")
      }
      response.result
    }
  }

  def updateOrUpdateAnnualSubmission(taxYear: Int, nino: String, incomeSourceId: String, body: JsValue)
  (implicit hc: HeaderCarrier): Future[Either[ApiError, Option[PeriodicSubmissionId]]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (url"""${appConfig.ifBaseUrl}/income-tax/business/property/annual/${toTaxYearParamAfter2324(taxYear)}/$nino/$incomeSourceId""", "1861")
    } else {
      (url"""${appConfig.ifBaseUrl}/income-tax/business/property/annual?taxableEntityId=$nino&taxYear=${toTaxYearParamBefore2324(taxYear)}&incomeSourceId=$incomeSourceId""", "1593")
    }

    httpClient.PUTString[PostPeriodicSubmissionResponse](url, StaticBinding.generateFromJsValue(body, escapeNonASCII = false))(
      implicitly[HttpReads[PostPeriodicSubmissionResponse]],
      ifHeaderCarrier(url, apiVersion).withExtraHeaders(headers = "Content-Type" -> "application/json"),
      ec).map { response: PostPeriodicSubmissionResponse =>
      if (response.result.isLeft) {
        val correlationId = response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
        logger.error(s"Error creating a property periodic submission from the Integration Framework:" +
          s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}")
      }
      response.result
    }
  }


  private def after2324Api(taxYear: Int): Boolean = {
    taxYear >= 2024
  }

  private def toTaxYearParamBefore2324(taxYear: Int): String = {
    s"${taxYear - 1}-${taxYear.toString takeRight 2}"
  }

  private def toTaxYearParamAfter2324(taxYear: Int): String = {
    s"${(taxYear - 1).toString takeRight 2}-${taxYear.toString takeRight 2}"
  }

}
