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

import models.request._
import models.request.foreign.allowances.ForeignPropertyAllowances
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import utils.CaseClassLevelDifferenceUtil.diff
import utils.UnitTest

import java.time.LocalDate

class PropertyAnnualSubmissionSpec extends UnitTest {
  val esbas = List(
    Esba(24, None, StructuredBuildingAllowanceBuilding(Some("test"), Some("123"), "XX1 XYZ")),
    Esba(24, None, StructuredBuildingAllowanceBuilding(Some("test"), Some("123"), "XX1 XYZ"))
  )
  val sbas = List(
    StructuredBuildingAllowance(24, None, StructuredBuildingAllowanceBuilding(Some("test1"), Some("123"), "XX1 XYZ")),
    StructuredBuildingAllowance(25, None, StructuredBuildingAllowanceBuilding(Some("test2"), Some("567"), "XX2 XYZ"))
  )

  val annualSubmissionWithoutEsbas = createAnnualSubmission(None)

  val annualSubmissionAfterAdditionOfEsbas = createAnnualSubmission(Some(esbas))

  val annualSubmissionWithAllFieldsFilled = PropertyAnnualSubmission(
    submittedOn = None,
    foreignProperty = Some(
      Seq(
        AnnualForeignProperty(
          "FR",
          Some(
            ForeignPropertyAdjustments(
              Some(56.78),
              Some(90.12)
            )
          ),
          Some(
            ForeignPropertyAllowances(
              "AUS",
              Some(34.56),
              Some(78.90),
              Some(12.34),
              Some(56.78),
              Some(34.56),
              Some(78.90),
              None
            )
          )
        ),
        AnnualForeignProperty(
          "GR",
          Some(
            ForeignPropertyAdjustments(
              Some(156.78),
              Some(190.12)
            )
          ),
          Some(
            ForeignPropertyAllowances(
              "AUS",
              Some(134.56),
              Some(178.90),
              Some(112.34),
              Some(156.78),
              Some(134.56),
              Some(178.90),
              None
            )
          )
        )
      )
    ),
    ukOtherProperty = Some(
      AnnualUkOtherProperty(
        Some(
          UkOtherAdjustments(
            Some(78.90),
            Some(12.34),
            Some(56.78),
            Some(90.12),
            Some(true),
            Some(UkRentARoom(true))
          )
        ),
        Some(
          UkOtherAllowances(
            Some(12.34),
            Some(56.78),
            Some(90.12),
            Some(34.56),
            Some(78.90),
            Some(
              Seq(
                StructuredBuildingAllowance(
                  56.78,
                  Option(StructuredBuildingAllowanceDate(LocalDate.now(), 90.12)),
                  StructuredBuildingAllowanceBuilding(
                    Some("name"),
                    Some("12"),
                    "AB1 2CD"
                  )
                ),
                StructuredBuildingAllowance(
                  34.56,
                  Option(StructuredBuildingAllowanceDate(LocalDate.now(), 78.90)),
                  StructuredBuildingAllowanceBuilding(
                    Some("name"),
                    Some("12"),
                    "AB1 2CD"
                  )
                )
              )
            ),
            Some(
              Seq(
                Esba(
                  56.78,
                  Option(StructuredBuildingAllowanceDate(LocalDate.now(), 90.12)),
                  StructuredBuildingAllowanceBuilding(
                    Some("name"),
                    Some("12"),
                    "AB1 2CD"
                  )
                ),
                Esba(
                  34.56,
                  Option(StructuredBuildingAllowanceDate(LocalDate.now(), 78.90)),
                  StructuredBuildingAllowanceBuilding(
                    Some("name"),
                    Some("12"),
                    "AB1 2CD"
                  )
                )
              )
            ),
            Some(78.90),
            Some(12.34)
          )
        )
      )
    )
  )
  def createAnnualSubmission(esbasMaybe: Option[List[Esba]]) =
    PropertyAnnualSubmission(
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
        ClaimExpensesOrRelief(
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
                  .map(_.copy(ukOtherRentARoom = Some(UkRentARoom(ukRaRAbout.jointlyLetYesOrNo))))
              )
            )
          )
        )
        .copy(submittedOn = None)
    }

    "be generated from rentals adjustments and not override existing other fields" in {
      val propertyRentalAdjustments = PropertyRentalAdjustments(
        13.57,
        BalancingCharge(true, Some(80.24)),
        Some(68.02),
        RenovationAllowanceBalancingCharge(
          true,
          Some(35.79)
        ),
        13.57,
        Some(91.35)
      )

      val firstLevelDiff = diff(
        PropertyAnnualSubmission
          .fromPropertyRentalAdjustments(
            propertyRentalAdjustments,
            annualSubmissionWithAllFieldsFilled
          ),
        annualSubmissionWithAllFieldsFilled
      )

      val secondLevelDiff = diff(
        PropertyAnnualSubmission
          .fromPropertyRentalAdjustments(
            propertyRentalAdjustments,
            annualSubmissionWithAllFieldsFilled
          )
          .ukOtherProperty
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty.get
      )

      val thirdLevelDiff = diff(
        PropertyAnnualSubmission
          .fromPropertyRentalAdjustments(
            propertyRentalAdjustments,
            annualSubmissionWithAllFieldsFilled
          )
          .ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAdjustments)
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAdjustments)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("ukOtherPropertyAnnualAdjustments")
      thirdLevelDiff should be(
        List("balancingCharge", "privateUseAdjustment", "businessPremisesRenovationAllowanceBalancingCharges")
      )
    }
    "be generated from rar adjustments and not override existing other fields" in {
      val raRAdjustments = RaRAdjustments(
        Option(BalancingCharge(true, Some(80.24))),
        Some(12.65)
      )

      val firstLevelDiff = diff(
        PropertyAnnualSubmission
          .fromRaRAdjustments(
            annualSubmissionWithAllFieldsFilled,
            raRAdjustments
          ),
        annualSubmissionWithAllFieldsFilled
      )

      val secondLevelDiff = diff(
        PropertyAnnualSubmission
          .fromRaRAdjustments(
            annualSubmissionWithAllFieldsFilled,
            raRAdjustments
          )
          .ukOtherProperty
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty.get
      )

      val thirdLevelDiff = diff(
        PropertyAnnualSubmission
          .fromRaRAdjustments(
            annualSubmissionWithAllFieldsFilled,
            raRAdjustments
          )
          .ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAdjustments)
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAdjustments)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("ukOtherPropertyAnnualAdjustments")
      thirdLevelDiff should be(
        List("balancingCharge")
      )
    }
    "be generated from rental allowances and not override existing other fields" in {
      val rentalAllowances = RentalAllowances(
        annualInvestmentAllowance = Some(11.22),
        zeroEmissionCarAllowance = Some(33.44),
        zeroEmissionGoodsVehicleAllowance = Some(44.55),
        replacementOfDomesticGoodsAllowance = Some(55.66),
        otherCapitalAllowance = Some(66.77),
        businessPremisesRenovationAllowance = Some(77.88)
      )

      val pas = PropertyAnnualSubmission
        .fromRentalAllowances(
          annualSubmissionWithAllFieldsFilled,
          rentalAllowances
        )
      val firstLevelDiff = diff(
        pas,
        annualSubmissionWithAllFieldsFilled
      )

      val secondLevelDiff = diff(
        pas.ukOtherProperty.get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty.get
      )

      val thirdLevelDiff = diff(
        pas.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("ukOtherPropertyAnnualAllowances")
      thirdLevelDiff should be(
        List(
          "annualInvestmentAllowance",
          "zeroEmissionGoodsVehicleAllowance",
          "businessPremisesRenovationAllowance",
          "otherCapitalAllowance",
          "costOfReplacingDomesticGoods",
          "zeroEmissionsCarAllowance"
        )
      )
    }
    "be generated from rar allowances and not override existing other fields" in {
      val raRAllowances = RentARoomAllowances(
        capitalAllowancesForACar = None,
        zeroEmissionCarAllowance = Some(33.44),
        zeroEmissionGoodsVehicleAllowance = Some(44.55),
        replacementOfDomesticGoodsAllowance = Some(55.66),
        otherCapitalAllowance = Some(66.77)
      )

      val pas = PropertyAnnualSubmission
        .fromRaRAllowances(
          annualSubmissionWithAllFieldsFilled,
          raRAllowances
        )
      val firstLevelDiff = diff(
        pas,
        annualSubmissionWithAllFieldsFilled
      )

      val secondLevelDiff = diff(
        pas.ukOtherProperty.get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty.get
      )

      val thirdLevelDiff = diff(
        pas.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("ukOtherPropertyAnnualAllowances")
      thirdLevelDiff should be(
        List(
          "zeroEmissionGoodsVehicleAllowance",
          "otherCapitalAllowance",
          "costOfReplacingDomesticGoods",
          "zeroEmissionsCarAllowance"
        )
      )
    }
    "be generated from rar allowances(with CapitalAllowancesForACar) and not override existing other fields" in {

      val raRAllowancesCapitalAllowancesForACar =
        RentARoomAllowances(Some(CapitalAllowancesForACar(true, Some(55.66))), None, None, None, None)

      val pas = PropertyAnnualSubmission
        .fromRaRAllowances(
          annualSubmissionWithAllFieldsFilled,
          raRAllowancesCapitalAllowancesForACar
        )
      val firstLevelDiff = diff(
        pas,
        annualSubmissionWithAllFieldsFilled
      )

      val secondLevelDiff = diff(
        pas.ukOtherProperty.get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty.get
      )

      val thirdLevelDiff = diff(
        pas.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("ukOtherPropertyAnnualAllowances")
      thirdLevelDiff should be(
        List(
          "zeroEmissionGoodsVehicleAllowance",
          "otherCapitalAllowance",
          "costOfReplacingDomesticGoods",
          "zeroEmissionsCarAllowance"
        )
      )
    }
    "be generated from rental sbas and not override existing other fields" in {

      val firstLevelDiff = diff(
        PropertyAnnualSubmission
          .fromSbas(
            annualSubmissionWithAllFieldsFilled,
            sbas
          ),
        annualSubmissionWithAllFieldsFilled
      )

      val secondLevelDiff = diff(
        PropertyAnnualSubmission
          .fromSbas(
            annualSubmissionWithAllFieldsFilled,
            sbas
          )
          .ukOtherProperty
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty.get
      )

      val thirdLevelDiff = diff(
        PropertyAnnualSubmission
          .fromSbas(
            annualSubmissionWithAllFieldsFilled,
            sbas
          )
          .ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("ukOtherPropertyAnnualAllowances")
      thirdLevelDiff should be(
        List("structuredBuildingAllowance")
      )
    }
    "be generated from rental esbas and not override existing other fields" in {
      val propertyAnnualSubmissionWithNewEsbas = PropertyAnnualSubmission
        .fromEsbas(
          annualSubmissionWithAllFieldsFilled,
          esbas
        )
      val firstLevelDiff = diff(
        propertyAnnualSubmissionWithNewEsbas,
        annualSubmissionWithAllFieldsFilled
      )

      val secondLevelDiff = diff(
        propertyAnnualSubmissionWithNewEsbas.ukOtherProperty.get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty.get
      )

      val thirdLevelDiff = diff(
        propertyAnnualSubmissionWithNewEsbas.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAllowances)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("ukOtherPropertyAnnualAllowances")
      thirdLevelDiff should be(
        List("enhancedStructuredBuildingAllowance")
      )
    }
    "be generated from rar about and not override existing other fields" in {
      val propertyAnnualSubmissionWithNewRaRAbout = PropertyAnnualSubmission
        .fromUkRentARoomAbout(
          RaRAbout(
            false,
            12.34,
            ClaimExpensesOrRelief(true, Some(56.78))
          ),
          annualSubmissionWithAllFieldsFilled
        )
      val firstLevelDiff = diff(
        propertyAnnualSubmissionWithNewRaRAbout,
        annualSubmissionWithAllFieldsFilled
      )

      val secondLevelDiff = diff(
        propertyAnnualSubmissionWithNewRaRAbout.ukOtherProperty.get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty.get
      )

      val thirdLevelDiff = diff(
        propertyAnnualSubmissionWithNewRaRAbout.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAdjustments)
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAdjustments)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("ukOtherPropertyAnnualAdjustments")
      thirdLevelDiff should be(
        List("ukOtherRentARoom")
      )
    }
    "be generated from rentals and rar about and not override existing other fields" in {
      val propertyAnnualSubmissionWithNewRaRAbout = PropertyAnnualSubmission
        .fromRentalsAndRentARoomAbout(
          RentalsAndRaRAbout(
            false,
            12.34,
            true,
            22.33,
            ClaimExpensesOrRelief(true, Some(56.78))
          ),
          annualSubmissionWithAllFieldsFilled
        )
      val firstLevelDiff = diff(
        propertyAnnualSubmissionWithNewRaRAbout,
        annualSubmissionWithAllFieldsFilled
      )

      val secondLevelDiff = diff(
        propertyAnnualSubmissionWithNewRaRAbout.ukOtherProperty.get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty.get
      )

      val thirdLevelDiff = diff(
        propertyAnnualSubmissionWithNewRaRAbout.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAdjustments)
          .get,
        annualSubmissionWithAllFieldsFilled.ukOtherProperty
          .flatMap(_.ukOtherPropertyAnnualAdjustments)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("ukOtherPropertyAnnualAdjustments")
      thirdLevelDiff should be(
        List("ukOtherRentARoom")
      )
    }

  }

}
