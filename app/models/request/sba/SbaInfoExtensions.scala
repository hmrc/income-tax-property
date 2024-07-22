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

import models.responses.{StructuredBuildingAllowance, StructuredBuildingAllowanceBuilding, StructuredBuildingAllowanceDate}
import play.api.libs.json.{Json, OFormat}

final case class SbaInfoToSave(
  claimStructureBuildingAllowance: ClaimStructureBuildingAllowance,
  structureBuildingFormGroup: List[Sba]
)

object SbaInfoToSave {
  implicit val format: OFormat[SbaInfoToSave] = Json.format[SbaInfoToSave]
}

object SbaInfoExtensions {
  implicit class SbaExtensions(sbaInfo: SbaInfo) {
    def toSbaToSave: SbaInfoToSave =
      SbaInfoToSave(sbaInfo.claimStructureBuildingAllowance, sbaInfo.sbas)

    def toSba: List[StructuredBuildingAllowance] = sbaInfo.sbas.map(structureBuildingFormGroup =>
      StructuredBuildingAllowance(
        structureBuildingFormGroup.structureBuildingAllowanceClaim,
        Some(
          StructuredBuildingAllowanceDate(
            structureBuildingFormGroup.structureBuildingQualifyingDate,
            structureBuildingFormGroup.structureBuildingQualifyingAmount
          )
        ),
        StructuredBuildingAllowanceBuilding(
          Some(structureBuildingFormGroup.structuredBuildingAllowanceAddress.buildingName.value),
          Some(
            structureBuildingFormGroup.structuredBuildingAllowanceAddress.buildingNumber.value
          ),
          structureBuildingFormGroup.structuredBuildingAllowanceAddress.postCode.value
        )
      )
    )
  }
}
