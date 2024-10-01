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

package connectors

import config.AppConfig
import connectors.Connector.hcWithCorrelationId
import connectors.response._
import models.common.TaxYear.{asTyBefore24, asTys}
import models.common.{IncomeSourceId, Nino, TaxYear}
import models.errors.ApiError
import models.request.{CreatePropertyPeriodicSubmissionRequest, UpdatePropertyPeriodicSubmissionRequest}
import models.responses._
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json.Json
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HeaderNames, StringContextOps}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class IntegrationFrameworkConnector @Inject() (http: HttpClientV2, appConf: AppConfig)(implicit
  ec: ExecutionContext
) extends IFConnector {

  lazy val logger: Logger = LoggerFactory.getLogger("connector")

  override protected[connectors] val appConfig: AppConfig = appConf

  def getAllPeriodicSubmission(taxYear: Int, taxableEntityId: String, incomeSourceId: String)(implicit
    hc: HeaderCarrier
  ): Future[Either[ApiError, List[PeriodicSubmissionIdModel]]] = {

    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property/${toTaxYearParamAfter2324(taxYear)}/$taxableEntityId/$incomeSourceId/period",
        "1954"
      )
    } else {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property/$taxableEntityId/$incomeSourceId/period?taxYear=${toTaxYearParamBefore2324(taxYear)}",
        "1649"
      )
    }

    http
      .get(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.ifEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.authorisationTokenFor(apiVersion)}")
      .execute[GetPeriodicSubmissionIdResponse]
      .map(response => response.result)
  }

  def getPropertyPeriodicSubmission(taxYear: Int, nino: String, incomeSourceId: String, submissionId: String)(implicit
    hc: HeaderCarrier
  ): Future[Either[ApiError, Option[PropertyPeriodicSubmission]]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property/${toTaxYearParamAfter2324(taxYear)}/$nino/$incomeSourceId/periodic/$submissionId",
        "1862"
      )
    } else {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property/periodic?" +
          s"taxableEntityId=$nino&taxYear=${toTaxYearParamBefore2324(taxYear)}&incomeSourceId=$incomeSourceId&submissionId=$submissionId",
        "1595"
      )
    }

    http
      .get(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.ifEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.authorisationTokenFor(apiVersion)}")
      .execute[GetPropertyPeriodicSubmissionResponse]
      .map { response =>
        if (response.result.isLeft) {
          val correlationId =
            response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
          logger.error(
            s"Error getting a property periodic submission from the Integration Framework:" +
              s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}"
          )
        }
        response.result
      }
  }

  def getPropertyAnnualSubmission(taxYear: Int, nino: String, incomeSourceId: String)(implicit
    hc: HeaderCarrier
  ): Future[Either[ApiError, Option[PropertyAnnualSubmission]]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property/annual/${toTaxYearParamAfter2324(taxYear)}/$nino/$incomeSourceId",
        "1805"
      )
    } else {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property/annual?" +
          s"taxableEntityId=$nino&taxYear=${toTaxYearParamBefore2324(taxYear)}&incomeSourceId=$incomeSourceId",
        "1598"
      )
    }

    http
      .get(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.ifEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.authorisationTokenFor(apiVersion)}")
      .execute[GetPropertyAnnualSubmissionResponse]
      .map { response =>
        if (response.result.isLeft) {
          val correlationId =
            response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
          logger.error(
            s"Error getting a property annual submission from the Integration Framework:" +
              s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}"
          )
        }
        response.result
      }
  }

  def deletePropertyAnnualSubmission(incomeSourceId: String, taxableEntityId: String, taxYear: Int)(implicit
    hc: HeaderCarrier
  ): Future[Either[ApiError, Unit]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property/annual" +
          s"/${toTaxYearParamAfter2324(taxYear)}?taxableEntityId=$taxableEntityId&incomeSourceId=$incomeSourceId",
        "1863"
      )
    } else {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property/" +
          s"annual?taxableEntityId=$taxableEntityId&taxYear=${toTaxYearParamBefore2324(taxYear)}&incomeSourceId=$incomeSourceId",
        "1596"
      )
    }

    http
      .delete(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.ifEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.authorisationTokenFor(apiVersion)}")
      .execute[DeletePropertyAnnualSubmissionResponse]
      .map { response =>
        if (response.result.isLeft) {
          val correlationId =
            response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
          logger.error(
            s"Error deleting a property annual submission from the Integration Framework:" +
              s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}"
          )
        }
        response.result
      }
  }

  def createPeriodicSubmission(
    taxYear: Int,
    nino: String,
    incomeSourceId: String,
    body: CreatePropertyPeriodicSubmissionRequest
  )(implicit hc: HeaderCarrier): Future[Either[ApiError, Option[PeriodicSubmissionId]]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property/periodic" +
          s"/${toTaxYearParamAfter2324(taxYear)}?taxableEntityId=$nino&incomeSourceId=$incomeSourceId",
        "1861"
      )
    } else {
      (
        s"${appConfig.ifBaseUrl}/income-tax/business/property" +
          s"/periodic?taxableEntityId=$nino&taxYear=${toTaxYearParamBefore2324(taxYear)}&incomeSourceId=$incomeSourceId",
        "1593"
      )
    }
    logger.debug(s"createPeriodicSubmission with url: $url, body: ${Json.toJson(body)}")

    http
      .post(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.ifEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.authorisationTokenFor(apiVersion)}")
      .withBody[CreatePropertyPeriodicSubmissionRequest](body)
      .execute[PostPeriodicSubmissionResponse]
      .map { response: PostPeriodicSubmissionResponse =>
        if (response.result.isLeft) {
          val correlationId =
            response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
          logger.error(
            s"Error creating a property periodic submission from the Integration Framework:" +
              s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}"
          )
        }
        response.result
      }
  }

  def updatePeriodicSubmission(
    nino: String,
    incomeSourceId: String,
    taxYear: Int,
    submissionId: String,
    propertyPeriodicSubmissionRequest: UpdatePropertyPeriodicSubmissionRequest
  )(implicit hc: HeaderCarrier): Future[Either[ApiError, Option[String]]] = {
    val (url, apiVersion) = if (after2324Api(taxYear)) {
      (
        s"""${appConfig.ifBaseUrl}/income-tax/business/property/periodic/${toTaxYearParamAfter2324(
            taxYear
          )}?taxableEntityId=$nino&incomeSourceId=$incomeSourceId&submissionId=$submissionId""",
        "1958"
      )
    } else {
      (
        s"""${appConfig.ifBaseUrl}/income-tax/business/property/periodic?taxableEntityId=$nino&taxYear=${toTaxYearParamBefore2324(
            taxYear
          )}&incomeSourceId=$incomeSourceId&submissionId=$submissionId""",
        "1594"
      )
    }
    logger.debug(
      s"updatePeriodicSubmission with url: $url, body: ${Json.toJson(propertyPeriodicSubmissionRequest)}"
    )

    http
      .put(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.ifEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.authorisationTokenFor(apiVersion)}")
      .withBody(propertyPeriodicSubmissionRequest)
      .execute[PutPeriodicSubmissionResponse]
      .map { response: PutPeriodicSubmissionResponse =>
        if (response.result.isLeft) {
          val correlationId =
            response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
          logger.error(
            s"Error updating a property periodic submission from the Integration Framework:" +
              s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}"
          )
        }
        response.result
      }

  }

  def createOrUpdateAnnualSubmission(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino,
    body: PropertyAnnualSubmission
  )(implicit hc: HeaderCarrier): Future[Either[ApiError, Unit]] = {
    val (url, apiVersion) = if (taxYear.isAfter24) {
      (
        s"""${appConfig.ifBaseUrl}/income-tax/business/property/annual/${asTys(taxYear)}/$nino/$incomeSourceId""",
        "1804"
      )
    } else {
      (
        s"""${appConfig.ifBaseUrl}/income-tax/business/property/annual?taxableEntityId=$nino&taxYear=${asTyBefore24(
            taxYear
          )}&incomeSourceId=$incomeSourceId""",
        "1597"
      )
    }
    logger.debug(s"createOrUpdateAnnualSubmission with url: $url, body: ${Json.toJson(body)}")

    // refactor: to fix bug
    val submissionRequest = body.copy(submittedOn = None)

    http
      .put(url"$url")(hcWithCorrelationId(hc))
      .setHeader("Environment" -> appConfig.ifEnvironment)
      .setHeader(HeaderNames.authorisation -> s"Bearer ${appConfig.authorisationTokenFor(apiVersion)}")
      .withBody(submissionRequest)
      .execute[PutAnnualSubmissionResponse]
      .map { response: PutAnnualSubmissionResponse =>
        if (response.result.isLeft) {
          val correlationId =
            response.httpResponse.header(key = "CorrelationId").map(id => s" CorrelationId: $id").getOrElse("")
          logger.error(
            s"Error creating a property annual submission from the Integration Framework:" +
              s" correlationId: $correlationId; status: ${response.httpResponse.status}; Body:${response.httpResponse.body}"
          )
        }
        response.result
      }
  }

  private def after2324Api(taxYear: Int): Boolean =
    taxYear >= 2024

  private def toTaxYearParamBefore2324(taxYear: Int): String =
    s"${taxYear - 1}-${taxYear.toString takeRight 2}"

  private def toTaxYearParamAfter2324(taxYear: Int): String =
    s"${(taxYear - 1).toString takeRight 2}-${taxYear.toString takeRight 2}"

}
