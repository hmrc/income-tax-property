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

package models.request.foreign.allowances

import play.api.libs.json.{Format, Json, OFormat}

case class CapitalAllowancesForACar(isCapitalAllowancesForACar: Boolean, capitalAllowancesForACarAmount: Option[BigDecimal])

object CapitalAllowancesForACar {
  implicit val formats: OFormat[CapitalAllowancesForACar] = Json.format[CapitalAllowancesForACar]
}

case class ForeignPropertyAllowancesWithCountryCode(
  countryCode: String,
  zeroEmissionsCarAllowance: Option[BigDecimal],
  zeroEmissionsGoodsVehicleAllowance: Option[BigDecimal],
  costOfReplacingDomesticItems: Option[BigDecimal],
  otherCapitalAllowance: Option[BigDecimal],
  capitalAllowancesForACar: Option[CapitalAllowancesForACar]
)

object ForeignPropertyAllowancesWithCountryCode {
  implicit val format: Format[ForeignPropertyAllowancesWithCountryCode] = Json.format[ForeignPropertyAllowancesWithCountryCode]
}
