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
import play.api.libs.json.Json

final case class EsbaInfoToSave(
                                 claimEnhancedStructureBuildingAllowance: ClaimEnhancedStructureBuildingAllowance,
                                 esbaClaims: EsbaClaims
                               )

object EsbaInfoToSave {
  implicit val format = Json.format[EsbaInfoToSave]
}

object EsbaInfoExtensions {
  implicit class EsbaExtensions(esbaInfo: EsbaInfo) {
    def toEsbaToSave: EsbaInfoToSave = EsbaInfoToSave(esbaInfo.claimEnhancedStructureBuildingAllowance, esbaInfo.esbaClaims)

    def toEsba: List[Esba] = esbaInfo.esbas.map(esbaInRequest => Esba(
      esbaInRequest.esbaClaim,  //Todo: IMPORTANT! Which one?
      Some(StructuredBuildingAllowanceDate(esbaInRequest.esbaQualifyingDate, esbaInRequest.esbaQualifyingAmount)), //Todo: IMPORTANT! Which one?
      StructuredBuildingAllowanceBuilding(
        Some(
          esbaInRequest.esbaAddress.buildingName.value),
        Some(
          esbaInRequest.esbaAddress.buildingNumber.value
        ),
        esbaInRequest.esbaAddress.postCode.value
      )
    )
    )
  }
}
