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

import models.responses.{Esba, StructuredBuildingAllowanceBuilding, StructuredBuildingAllowanceDate}
import play.api.libs.json.{Json, OFormat}

final case class EsbaInfoToSave(
  claimEnhancedStructureBuildingAllowance: Boolean,
  esbaClaims: Option[Boolean]
)

object EsbaInfoToSave {
  implicit val format: OFormat[EsbaInfoToSave] = Json.format[EsbaInfoToSave]
}

object EsbaInfoExtensions {
  implicit class EsbaExtensions(esbaInfo: EsbaInfo) {

    def toEsba: List[Esba] = esbaInfo.enhancedStructureBuildingAllowances.map(esbaInRequest =>
      Esba(
        esbaInRequest.enhancedStructureBuildingAllowanceClaim,
        Some(
          StructuredBuildingAllowanceDate(esbaInRequest.enhancedStructureBuildingAllowanceQualifyingDate, esbaInRequest.enhancedStructureBuildingAllowanceQualifyingAmount)
        ),
        StructuredBuildingAllowanceBuilding(
          Some(esbaInRequest.enhancedStructureBuildingAllowanceAddress.buildingName.value),
          Some(
            esbaInRequest.enhancedStructureBuildingAllowanceAddress.buildingNumber.value
          ),
          esbaInRequest.enhancedStructureBuildingAllowanceAddress.postCode.value
        )
      )
    )
  }
}
