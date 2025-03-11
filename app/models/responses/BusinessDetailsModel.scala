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

package models.responses

import play.api.libs.json._

import java.time.LocalDate

case class BusinessDetailsModel(incomeSourceId: String,
                                accountingPeriodStartDate: LocalDate,
                                accountingPeriodEndDate: LocalDate,
                                tradingName: Option[String],
                                businessAddressDetails: Option[BusinessAddressDetails],
                                businessContactDetails: Option[BusinessContactDetails],
                                tradingStartDate: Option[LocalDate],
                                latencyDetails: Option[LatencyDetails],
                                isCashOrAccruals: Option[Boolean],
                                isSeasonal: Option[Boolean],
                                cessationDate: Option[LocalDate],
                                isPaperless: Option[Boolean],
                                firstAccountingPeriodStartDate: Option[LocalDate],
                                firstAccountingPeriodEndDate: Option[LocalDate])

object BusinessDetailsModel {
  implicit val format: Format[BusinessDetailsModel] = Json.format[BusinessDetailsModel]
}

case class LatencyDetails(latencyEndDate: LocalDate, taxYear1: String, latencyIndicator1: String, taxYear2: String, latencyIndicator2: String)

object LatencyDetails {
  implicit val format: Format[LatencyDetails] = Json.format[LatencyDetails]
}

case class BusinessAddressDetails(addressLine1: Option[String],
                                  addressLine2: Option[String],
                                  addressLine3: Option[String],
                                  addressLine4: Option[String],
                                  postalCode: Option[String],
                                  countryCode: Option[String])

object BusinessAddressDetails {
  implicit val businessAddressDetailsFormat: OFormat[BusinessAddressDetails] = Json.format[BusinessAddressDetails]
}

case class BusinessContactDetails(telephone: Option[String],
                                  mobileNo: Option[String],
                                  faxNo: Option[String],
                                  email: Option[String])

object BusinessContactDetails {
  implicit val businessContactDetailsFormat: OFormat[BusinessContactDetails] = Json.format[BusinessContactDetails]
}





