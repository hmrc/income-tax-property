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
  allowances: Option[Seq[ForeignStructureBuildingAllowance]]
)

object ForeignPropertySbaWithCountryCode {
  implicit val format: Format[ForeignPropertySbaWithCountryCode] = Json.format[ForeignPropertySbaWithCountryCode]
}

case class ForeignStructureBuildingAllowance(
  foreignStructureBuildingAllowanceClaim: BigDecimal,
  foreignStructureBuildingQualifyingDate: LocalDate,
  foreignStructureBuildingQualifyingAmount: BigDecimal,
  foreignStructureBuildingAddress: ForeignStructureBuildingAllowanceAddress
)

object ForeignStructureBuildingAllowance {
  implicit val format: Format[ForeignStructureBuildingAllowance] = Json.format[ForeignStructureBuildingAllowance]
}

case class ForeignStructureBuildingAllowanceAddress(
  name: String,
  number: String,
  postCode: String
)

object ForeignStructureBuildingAllowanceAddress {
  implicit val format: Format[ForeignStructureBuildingAllowanceAddress] =
    Json.format[ForeignStructureBuildingAllowanceAddress]
}
