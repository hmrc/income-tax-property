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

package models.repository

import models.repository.Extractor._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba._
import utils.UnitTest

import java.time.LocalDate

class ExtractorSpec extends UnitTest {

  "EsbaExtractor" should {
    val esbaDate = LocalDate.parse("2024-01-01")
    val qualifyingAmountExpenditure = 35
    val amount = 25
    val address1 = "name1"
    val address2 = "name2"
    val postcode = "AB1 XY2"
    val claimEnhancedStructureBuildingAllowance = ClaimEnhancedStructureBuildingAllowance(true)
    val esbaClaims = EsbaClaims(false)

    "extract to save part into another model" in {
      EsbaInfo(
        claimEnhancedStructureBuildingAllowance,
        esbaClaims,
        List(
          EsbaInUpstream(
            esbaDate,
            qualifyingAmountExpenditure,
            amount,
            Address(
              BuildingName(address1),
              BuildingNumber(address2),
              Postcode(postcode)
            )
          )
        )
      ).extractToSavePart() shouldBe EsbaInfoToSave(claimEnhancedStructureBuildingAllowance, esbaClaims)
    }
  }
}
