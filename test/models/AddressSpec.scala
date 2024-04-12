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
import play.api.libs.json.{JsValue, Json}
import utils.UnitTest

class AddressSpec extends UnitTest {
  private val address = Address(
    BuildingName("12"),
    BuildingNumber("2"),
    Postcode("XX1 1XX")
  )
  private val validRequestBody: JsValue = Json.parse(
    """{
      |                    "buildingName" : "12",
      |                    "buildingNumber" : "2",
      |                    "postCode" : "XX1 1XX"
      |                }""".stripMargin)

  "Address" should {
    "serialise to json successfully" in {
      Json.toJson(address) shouldBe validRequestBody
    }

    "deserialise json successfully" in {
      validRequestBody.as[Address] shouldBe address
    }

  }
}
