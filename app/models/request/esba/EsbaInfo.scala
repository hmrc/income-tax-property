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

package models.request.esba

import models.request.common.Address
import play.api.libs.json.Json

import java.time.LocalDate

final case class EsbaInUpstream(
                                esbaQualifyingDate: LocalDate,
                                esbaQualifyingAmount: BigDecimal,
                                esbaClaim: BigDecimal,
                                esbaAddress: Address
                              )

object EsbaInUpstream {
  implicit val format = Json.format[EsbaInUpstream]
}

final case class EsbaInfo(
                           claimEnhancedStructureBuildingAllowance: ClaimEnhancedStructureBuildingAllowance,
                           esbaClaims: EsbaClaims,
                           esbas: List[EsbaInUpstream]
                         )

object EsbaInfo {
  implicit val format = Json.format[EsbaInfo]
}