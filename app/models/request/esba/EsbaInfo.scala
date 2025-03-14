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

import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.responses.Esba
import play.api.libs.json.{Json, OFormat}

import java.time.LocalDate

final case class EsbaInUpstream(
                                 enhancedStructureBuildingAllowanceQualifyingDate: LocalDate,
                                 enhancedStructureBuildingAllowanceQualifyingAmount: BigDecimal,
                                 enhancedStructureBuildingAllowanceClaim: BigDecimal,
                                 enhancedStructureBuildingAllowanceAddress: Address
)

object EsbaInUpstream {
  implicit val format: OFormat[EsbaInUpstream] = Json.format[EsbaInUpstream]

  def fromEsbasToEsbasInUpstream(esbas: List[Esba]): Option[List[EsbaInUpstream]] =
    toNoneIfAnyFirstYearIsNone(esbas)

  private def toNoneIfAnyFirstYearIsNone(esbas: List[Esba]): Option[List[EsbaInUpstream]] = {
    val r: List[Option[EsbaInUpstream]] = esbas.map(e =>
      e.firstYear.map(firstYear =>
        EsbaInUpstream(
          firstYear.qualifyingDate,
          firstYear.qualifyingAmountExpenditure,
          e.amount,
          Address(
            BuildingName(e.building.name.getOrElse("")),
            BuildingNumber(e.building.number.getOrElse("")),
            Postcode(e.building.postCode)
          )
        )
      )
    )

    val esbasOnlyWithFirstYear: Option[List[EsbaInUpstream]] =
      r.foldLeft[Option[List[EsbaInUpstream]]](Some(List()))((acc, a) =>
        (acc, a) match {
          case (Some(list), Some(esba)) => Some(esba :: list)
          case _                        => None
        }
      )
    esbasOnlyWithFirstYear
  }
}

final case class EsbaInfo(
  claimEnhancedStructureBuildingAllowance: Boolean,
  enhancedStructureBuildingAllowanceClaims: Option[Boolean],
  enhancedStructureBuildingAllowances: List[EsbaInUpstream]
)

object EsbaInfo {
  implicit val format: OFormat[EsbaInfo] = Json.format[EsbaInfo]
}
