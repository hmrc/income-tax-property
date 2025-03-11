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

import models.repository.Extractor._
import models.request.common.{Address, BuildingName, BuildingNumber, Postcode}
import models.request.esba.EsbaInfoExtensions._
import models.request.esba._
import models.responses._
import play.api.libs.json.{JsValue, Json}
import utils.UnitTest

import java.time.{LocalDate, LocalDateTime}
class EsbaInfoSpec extends UnitTest {
  val esbaInfo = EsbaInfo(
    isClaimEnhancedStructureBuildingAllowance = true,
    isEnhancedStructureBuildingAllowanceClaims = Some(false),
    List(
      EsbaInUpstream(
        LocalDate.parse("2020-04-04"),
        12,
        43,
        Address(
          BuildingName("name12"),
          BuildingNumber("123"),
          Postcode("XX1 1XX")
        )
      ),
      EsbaInUpstream(
        LocalDate.parse("2023-01-22"),
        535,
        54,
        Address(
          BuildingName("235"),
          BuildingNumber("3"),
          Postcode("XX1 1XX")
        )
      ),
      EsbaInUpstream(
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
                                               | "isClaimEnhancedStructureBuildingAllowance" : true,
                                               | "enhancedStructureBuildingAllowances": [
                                               |            {
                                               |                "enhancedStructureBuildingAllowanceQualifyingDate" : "2020-04-04",
                                               |                "enhancedStructureBuildingAllowanceQualifyingAmount" : 12,
                                               |                "enhancedStructureBuildingAllowanceClaim" : 43,
                                               |                "enhancedStructureBuildingAllowanceAddress" : {
                                               |                    "buildingName" : "name12",
                                               |                    "buildingNumber" : "123",
                                               |                    "postCode" : "XX1 1XX"
                                               |                }
                                               |            },
                                               |            {
                                               |                "enhancedStructureBuildingAllowanceQualifyingDate" : "2023-01-22",
                                               |                "enhancedStructureBuildingAllowanceQualifyingAmount" : 535,
                                               |                "enhancedStructureBuildingAllowanceClaim" : 54,
                                               |                "enhancedStructureBuildingAllowanceAddress" : {
                                               |                    "buildingName" : "235",
                                               |                    "buildingNumber" : "3",
                                               |                    "postCode" : "XX1 1XX"
                                               |                }
                                               |            },
                                               |            {
                                               |                "enhancedStructureBuildingAllowanceQualifyingDate" : "2024-02-12",
                                               |                "enhancedStructureBuildingAllowanceQualifyingAmount" : 22,
                                               |                "enhancedStructureBuildingAllowanceClaim" : 23,
                                               |                "enhancedStructureBuildingAllowanceAddress" : {
                                               |                    "buildingName" : "12",
                                               |                    "buildingNumber" : "2",
                                               |                    "postCode" : "XX1 1XX"
                                               |                }
                                               |            }
                                               |        ],
                                               |        "isEnhancedStructureBuildingAllowanceClaims" : false
                                               |}""".stripMargin)

  val submission = PropertyAnnualSubmission(
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
  "EsbaInfo" should {
    "deserialise the json" in {

      validRequestBody.as[EsbaInfo] shouldBe esbaInfo

    }
    "serialise from json" in {
      Json.toJson(esbaInfo) shouldBe validRequestBody
    }

  }

  "EsbaInfoExtension" should {

    "convert from esbaInfo to esbas" in {
      val esbas = esbaInfo.enhancedStructureBuildingAllowances.map(esbaInRequest =>
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

      esbaInfo.toEsba shouldBe esbas
    }

    "convert to from EsbaInfo to EsbaInfoToSave" in {
      esbaInfo.extractToSavePart() shouldBe EsbaInfoToSave(
        esbaInfo.isClaimEnhancedStructureBuildingAllowance,
        esbaInfo.isEnhancedStructureBuildingAllowanceClaims
      )
    }
  }

}
