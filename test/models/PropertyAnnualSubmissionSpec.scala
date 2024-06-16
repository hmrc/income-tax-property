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

import models.request.{ClaimExpensesOrRRR, RaRAbout}
import models.request.common.Postcode
import models.responses.{AnnualUkOtherProperty, Esba, PropertyAnnualSubmission, StructuredBuildingAllowanceBuilding, UkOtherAdjustments, UkOtherAllowances, UkRentARoom}
import utils.UnitTest

import java.time.LocalDateTime

class PropertyAnnualSubmissionSpec extends UnitTest {
  val esbas = List(
    Esba(24, None, StructuredBuildingAllowanceBuilding(Some("test"), Some("123"), "XX1 XYZ")),
    Esba(24, None, StructuredBuildingAllowanceBuilding(Some("test"), Some("123"), "XX1 XYZ"))
  )

  val annualSubmissionWithoutEsbas = createAnnualSubmission(None)

  val annualSubmissionAfterAdditionOfEsbas = createAnnualSubmission(Some(esbas))

  def createAnnualSubmission(esbasMaybe: Option[List[Esba]]) =
    PropertyAnnualSubmission(
      None,
      None,
      None,
      None,
      Some(
        AnnualUkOtherProperty(
          Some(
            UkOtherAdjustments(
              Some(12.34),
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
              esbasMaybe,
              Some(34.56),
              None
            )
          )
        )
      )
    )

  "PropertyAnnualSubmission" should {
    "be generated from esba list" in {
      PropertyAnnualSubmission
        .fromEsbas(annualSubmissionWithoutEsbas, esbas)
        .copy(submittedOn = None) shouldBe annualSubmissionAfterAdditionOfEsbas
        .copy(submittedOn = None)
    }

    "be generated from uk rent a room about" in {
      val ukRaRAbout = RaRAbout(
        true,
        12.34,
        ClaimExpensesOrRRR(
          true,
          Some(56.78)
        )
      )
      PropertyAnnualSubmission
        .fromUkRentARoomAbout(ukRaRAbout, annualSubmissionAfterAdditionOfEsbas)
        .copy(submittedOn = None) shouldBe annualSubmissionAfterAdditionOfEsbas
        .copy(ukOtherProperty =
          annualSubmissionAfterAdditionOfEsbas.ukOtherProperty.map(
            _.copy(ukOtherPropertyAnnualAdjustments =
              annualSubmissionAfterAdditionOfEsbas.ukOtherProperty.flatMap(
                _.ukOtherPropertyAnnualAdjustments
                  .map(_.copy(ukOtherRentARoom = Some(UkRentARoom(ukRaRAbout.ukRentARoomJointlyLet))))
              )
            )
          )
        )
        .copy(submittedOn = None)
    }
  }
}
