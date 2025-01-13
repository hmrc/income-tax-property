/*
 * Copyright 2024 HM Revenue & Customs
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

package models.request.foreign.sba

import play.api.libs.json.{Format, Json}

import java.time.LocalDate

case class ForeignPropertySbaWithCountryCode(
  countryCode: String,
  claimStructureBuildingAllowance: Boolean,
  allowances: Array[ForeignStructureBuildingAllowance]
)

object ForeignPropertySbaWithCountryCode {
  implicit val format: Format[ForeignPropertySbaWithCountryCode] = Json.format[ForeignPropertySbaWithCountryCode]
}

case class ForeignStructureBuildingAllowance(
  amount: BigDecimal,
  firstYear: Option[ForeignStructuredBuildingAllowanceDate],
  building: ForeignStructuredBuildingAllowanceBuilding
)

object ForeignStructureBuildingAllowance {
  implicit val format: Format[ForeignStructureBuildingAllowance] = Json.format[ForeignStructureBuildingAllowance]
}

case class ForeignStructuredBuildingAllowanceDate(qualifyingDate: LocalDate, qualifyingAmountExpenditure: BigDecimal)

object ForeignStructuredBuildingAllowanceDate {
  implicit val format: Format[ForeignStructuredBuildingAllowanceDate] =
    Json.format[ForeignStructuredBuildingAllowanceDate]
}

case class ForeignStructuredBuildingAllowanceBuilding(
  name: Option[String],
  number: Option[String],
  postCode: Option[String]
)

object ForeignStructuredBuildingAllowanceBuilding {
  implicit val format: Format[ForeignStructuredBuildingAllowanceBuilding] =
    Json.format[ForeignStructuredBuildingAllowanceBuilding]
}
