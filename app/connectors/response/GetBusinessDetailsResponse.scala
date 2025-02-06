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

package connectors.response

import connectors.Parser
import models.errors.ApiError
import models.responses.IncomeSourceDetailsModel
import play.api.Logging
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR, NOT_FOUND, OK, SERVICE_UNAVAILABLE}
import play.api.libs.json.Json
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class GetBusinessDetailsResponse(
  httpResponse: HttpResponse,
  result: Either[ApiError, Option[IncomeSourceDetailsModel]]
)

object GetBusinessDetailsResponse extends Logging {

  implicit val getBusinessDetailsResponseReads: HttpReads[GetBusinessDetailsResponse] =
    new HttpReads[GetBusinessDetailsResponse] with Parser {

      override protected[connectors] val parserName: String = this.getClass.getSimpleName

      override def read(method: String, url: String, response: HttpResponse): GetBusinessDetailsResponse =
        response.status match {
          case OK        => GetBusinessDetailsResponse(response, extractResult(response))
          case NOT_FOUND => GetBusinessDetailsResponse(response, Right(None))
          case INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE | BAD_REQUEST =>
            GetBusinessDetailsResponse(response, handleError(response, response.status))
          case _ => GetBusinessDetailsResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
        }

      private def extractResult(response: HttpResponse): Either[ApiError, Option[IncomeSourceDetailsModel]] = {
        //val json = response.json
        val json = Json.parse(
          """
            |{
            |    "processingDate": "2025-02-05T13:10:49Z",
            |    "taxPayerDisplayResponse":
            |    {
            |        "safeId": "XV0000100460886",
            |        "nino": "WP216633A",
            |        "mtdId": "XAIT00000176746",
            |        "propertyIncome": true,
            |        "propertyData":
            |        [
            |            {
              |              "incomeSourceType": "foreign-property",
            |                "incomeSourceId": "XJIS00001230596",
            |                "accountingPeriodStartDate": "2023-04-06",
            |                "accountingPeriodEndDate": "2024-04-05",
            |                "tradingStartDate": "2019-04-06",
            |                "cashOrAccruals": true,
            |                "incomeSourceStartDate": "2019-04-06",
            |                "firstAccountingPeriodStartDate": "2023-04-06",
            |                "firstAccountingPeriodEndDate": "2024-04-05"
            |            }
            |        ]
            |    }
            |}
            |""".stripMargin)
        logger.info(s"GetBusinessDetailsResponse response: $json")
        json
          .validate[IncomeSourceDetailsModel]
          .fold[Either[ApiError, Option[IncomeSourceDetailsModel]]](
            errors => {
              logger.error(s"Parsing failed with errors: ${errors.mkString(", ")}")
              badSuccessJsonResponse
            },
            parsedModel => {
              logger.info(s"Successfully parsed response: $json")
              Right(Some(parsedModel))
            }
          )
      }

    }
}
