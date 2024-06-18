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

package models

import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba.EsbaInUpstream
import models.responses.{Esba, StructuredBuildingAllowanceBuilding, StructuredBuildingAllowanceDate}
import utils.UnitTest

import java.time.LocalDate

class EsbaInUpstreamSpec extends UnitTest {
  val esbaQualifyingDate1 = LocalDate.now().minusMonths(6)
  val esbaQualifyingDate2 = LocalDate.now().minusMonths(9)

  val building1 = StructuredBuildingAllowanceBuilding(
    None,
    None,
    "postcode1"
  )
  val building2 = StructuredBuildingAllowanceBuilding(
    None,
    None,
    "postcode2"
  )
  val esbaQualifyingAmount1 = 98.76
  val esbaWithoutFirstYear = Esba(
    12.34,
    None,
    building1
  )

  val esbaQualifyingAmount2 = 56.78

  val esbaWithFirstYear1 = Esba(
    12.34,
    Some(StructuredBuildingAllowanceDate(esbaQualifyingDate1, esbaQualifyingAmount1)),
    building1
  )

  val esbaWithFirstYear2 = Esba(
    12.34,
    Some(StructuredBuildingAllowanceDate(esbaQualifyingDate2, esbaQualifyingAmount2)),
    building1
  )

  "EsbaInUpstream" should {
    "convert list with at least one esba without firstYear to None" in {
      val oneEsbaWithFirstYearAnotherWithoutFirstYear = List(
        esbaWithFirstYear1,
        esbaWithoutFirstYear
      )

      val result = EsbaInUpstream.fromEsbasToEsbasInUpstream(oneEsbaWithFirstYearAnotherWithoutFirstYear)
      result shouldBe None
    }

    "convert only to a some list if and only if all esbas have firstYear" in {
      val allEsbasWithFirstYear = List(
        esbaWithFirstYear1,
        esbaWithFirstYear2
      )

      val result = EsbaInUpstream.fromEsbasToEsbasInUpstream(allEsbasWithFirstYear)
      result shouldBe Some(
        List(
          EsbaInUpstream(
            esbaQualifyingDate2,
            esbaQualifyingAmount2,
            esbaWithFirstYear2.amount,
            Address(
              BuildingName(esbaWithFirstYear2.building.name.getOrElse("")),
              BuildingNumber(esbaWithFirstYear2.building.number.getOrElse("")),
              Postcode(esbaWithFirstYear2.building.postCode)
            )
          ),
          EsbaInUpstream(
            esbaQualifyingDate1,
            esbaQualifyingAmount1,
            esbaWithFirstYear1.amount,
            Address(
              BuildingName(esbaWithFirstYear1.building.name.getOrElse("")),
              BuildingNumber(esbaWithFirstYear1.building.number.getOrElse("")),
              Postcode(esbaWithFirstYear1.building.postCode)
            )
          )
        )
      )
    }
  }
}
