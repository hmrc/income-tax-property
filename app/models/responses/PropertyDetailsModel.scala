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
import models.PropertyDetails

import java.time.LocalDate

case class PropertyDetailsModel(
  incomeSourceType: Option[String],
  incomeSourceId: String,
  accountingPeriodStartDate: LocalDate,
  accountingPeriodEndDate: LocalDate,
  tradingStartDate: Option[LocalDate],
  isCashOrAccruals: Option[Boolean],
  numPropRented: Option[Int],
  numPropRentedUK: Option[Int],
  numPropRentedEEA: Option[Int],
  numPropRentedNONEEA: Option[Int],
  email: Option[String],
  cessationDate: Option[LocalDate],
  isPaperless: Option[Boolean],
  incomeSourceStartDate: Option[LocalDate],
  firstAccountingPeriodStartDate: Option[LocalDate],
  firstAccountingPeriodEndDate: Option[LocalDate],
  latencyDetails: Option[LatencyDetails]
)

object PropertyDetailsModel {
  implicit val format: OFormat[PropertyDetailsModel] = Json.format[PropertyDetailsModel]

  def toResponseModel(propertyDetailsModel: PropertyDetailsModel): PropertyDetails =
    PropertyDetails(
      propertyDetailsModel.incomeSourceType,
      propertyDetailsModel.tradingStartDate,
      propertyDetailsModel.isCashOrAccruals,
      propertyDetailsModel.incomeSourceId
    )
}
