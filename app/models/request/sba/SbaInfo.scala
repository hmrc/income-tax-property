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

package models.request.sba

import models.request.common.Address
import play.api.libs.json.{Format, Json, OFormat}

import java.time.LocalDate

final case class Sba(
  structureBuildingQualifyingDate: LocalDate,
  structureBuildingQualifyingAmount: BigDecimal,
  structureBuildingAllowanceClaim: BigDecimal,
  structuredBuildingAllowanceAddress: Address
)

object Sba {
  implicit val format: Format[Sba] = Json.format[Sba]
}

final case class SbaInfo(
  claimStructureBuildingAllowance: Boolean,
  structureBuildingFormGroup: List[Sba]
)

object SbaInfo {
  implicit val format: OFormat[SbaInfo] = Json.format[SbaInfo]
}
