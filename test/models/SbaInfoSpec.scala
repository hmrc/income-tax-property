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
import models.request.sba.SbaInfoExtensions.SbaExtensions
import models.request.sba._
import models.responses._
import play.api.libs.json.{JsValue, Json}
import utils.UnitTest

import java.time.{LocalDate, LocalDateTime}

class SbaInfoSpec extends UnitTest {
  val sbaInfo: SbaInfo = SbaInfo(
    claimStructureBuildingAllowance = true,
    List(
      Sba(
        LocalDate.parse("2020-04-04"),
        12,
        43,
        Address(
          BuildingName("name12"),
          BuildingNumber("123"),
          Postcode("XX1 1XX")
        )
      ),
      Sba(
        LocalDate.parse("2023-01-22"),
        535,
        54,
        Address(
          BuildingName("235"),
          BuildingNumber("3"),
          Postcode("XX1 1XX")
        )
      ),
      Sba(
        LocalDate.parse("2024-02-12"),
        22,
        23,
        Address(
          BuildingName("12"),
          BuildingNumber("2"),
          Postcode("XX1 1XX")
        )
      )
    )
  )
  val validRequestBody: JsValue = Json.parse("""{
                                               | "claimStructureBuildingAllowance" : true,
                                               | "allowances": [
                                               |            {
                                               |                "structureBuildingQualifyingDate" : "2020-04-04",
                                               |                "structureBuildingQualifyingAmount" : 12,
                                               |                "structureBuildingAllowanceClaim" : 43,
                                               |                "structuredBuildingAllowanceAddress" : {
                                               |                    "buildingName" : "name12",
                                               |                    "buildingNumber" : "123",
                                               |                    "postCode" : "XX1 1XX"
                                               |                }
                                               |            },
                                               |            {
                                               |                "structureBuildingQualifyingDate" : "2023-01-22",
                                               |                "structureBuildingQualifyingAmount" : 535,
                                               |                "structureBuildingAllowanceClaim" : 54,
                                               |                "structuredBuildingAllowanceAddress" : {
                                               |                    "buildingName" : "235",
                                               |                    "buildingNumber" : "3",
                                               |                    "postCode" : "XX1 1XX"
                                               |                }
                                               |            },
                                               |            {
                                               |                "structureBuildingQualifyingDate" : "2024-02-12",
                                               |                "structureBuildingQualifyingAmount" : 22,
                                               |                "structureBuildingAllowanceClaim" : 23,
                                               |                "structuredBuildingAllowanceAddress" : {
                                               |                    "buildingName" : "12",
                                               |                    "buildingNumber" : "2",
                                               |                    "postCode" : "XX1 1XX"
                                               |                }
                                               |            }
                                               |        ]
                                               |}""".stripMargin)

  val submission: PropertyAnnualSubmission = PropertyAnnualSubmission(
    Some(LocalDateTime.now()),
    None,
    Some(
      AnnualUkOtherProperty(
        Some(
          UkOtherAdjustments(
            None,
            None,
            None,
            None,
            None,
            None
          )
        ),
        Some(
          UkOtherAllowances(
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(
              List(
                Esba(
                  43,
                  Some(StructuredBuildingAllowanceDate(LocalDate.parse("2020-04-04"), 12)),
                  StructuredBuildingAllowanceBuilding(Some("name12"), Some("123"), "XX1 1XX")
                ),
                Esba(
                  54,
                  Some(StructuredBuildingAllowanceDate(LocalDate.parse("2023-01-22"), 535)),
                  StructuredBuildingAllowanceBuilding(Some("235"), Some("3"), "XX1 1XX")
                ),
                Esba(
                  23,
                  Some(StructuredBuildingAllowanceDate(LocalDate.parse("2024-02-12"), 22)),
                  StructuredBuildingAllowanceBuilding(Some("12"), Some("2"), "XX1 1XX")
                )
              )
            ),
            None,
            None
          )
        )
      )
    )
  )
  "SbaInfo" should {
    "serialise from json" in {
      Json.toJson(sbaInfo) shouldBe validRequestBody
    }

  }

  "SbaInfoExtension" should {

    "convert from sbaInfo to structureBuildingFormGroup" in {
      val sbas = sbaInfo.allowances.map(sbaInRequest =>
        StructuredBuildingAllowance(
          sbaInRequest.structureBuildingAllowanceClaim,
          Some(
            StructuredBuildingAllowanceDate(
              sbaInRequest.structureBuildingQualifyingDate,
              sbaInRequest.structureBuildingQualifyingAmount
            )
          ),
          StructuredBuildingAllowanceBuilding(
            Some(sbaInRequest.structuredBuildingAllowanceAddress.buildingName.value),
            Some(
              sbaInRequest.structuredBuildingAllowanceAddress.buildingNumber.value
            ),
            sbaInRequest.structuredBuildingAllowanceAddress.postCode.value
          )
        )
      )

      sbaInfo.toSba shouldBe sbas
    }

    "convert to from SbaInfo to SbaInfoToSave" in {
      sbaInfo.toSbaToSave shouldBe SbaInfoToSave(
        Some(sbaInfo.claimStructureBuildingAllowance),
        sbaInfo.allowances
      )
    }
  }

}
