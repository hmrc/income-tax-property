/*
 * Copyright 2025 HM Revenue & Customs
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

package services
import models.ForeignAdjustmentsStoreAnswers
import models.common.JourneyStatus.InProgress
import models.common.{IncomeSourceId, JourneyName, Mtditid, TaxYear}
import models.domain.JourneyAnswers
import models.request.foreign.adjustments.{ForeignUnusedResidentialFinanceCost, ForeignWhenYouReportedTheLoss, UnusedLossesPreviousYears}
import models.request.foreign.allowances.ForeignAllowancesAnswers
import models.request.foreign.{AnnualForeignProperty, ForeignAdjustmentsAnswers, ForeignPropertyAdjustments, ForeignPropertyAllowances}
import models.request.{BalancingCharge, DeductingTax, PropertyRentalsIncome}
import models.responses._
import org.mockito.Mockito.when
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsObject, Json}
import utils.UnitTest

import java.time.{Instant, LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class MergeServiceSpec extends UnitTest with Matchers with MockitoSugar with ScalaCheckPropertyChecks {

  val mockJourneyAnswers: JourneyAnswers = mock[JourneyAnswers]
  val otherUkProperty: UkOtherProperty = UkOtherProperty(
    Some(UkOtherPropertyIncome(None, None, Some(BigDecimal(567)), Some(BigDecimal(2340)), Some(BigDecimal(999)), None)),
    None
  )

  val propertyPeriodicSubmission: PropertyPeriodicSubmission = PropertyPeriodicSubmission(
    Some(PeriodicSubmissionId("periodicSubmissionId")),
    submittedOn = Some(LocalDateTime.now),
    fromDate = LocalDate.now(),
    toDate = LocalDate.now(),
    foreignProperty = None,
    ukOtherProperty = Some(otherUkProperty),
    foreignIncome = None
  )

  val mergeService = new MergeService()

  "mergeRentalsIncome" should {
    "merge correctly with valid inputs" in {

      val mockData: JsObject = Json.obj(
        "isNonUKLandlord" -> true
      )
      when(mockJourneyAnswers.data).thenReturn(mockData)

      val result = mergeService.mergeRentalsIncome(Some(propertyPeriodicSubmission), Some(mockJourneyAnswers))

      result shouldBe Some(
        PropertyRentalsIncome(
          isNonUKLandlord = true,
          567,
          999,
          Some(DeductingTax(isTaxDeducted = true, Some(BigDecimal(2340)))),
          None,
          None,
          None,
          None,
          None
        )
      )
    }

    "return None when no data is available" in {
      val result = mergeService.mergeRentalsIncome(None, None)

      result shouldBe None
    }
  }
  "mergeForeignPropertyAllowances" should {

    "return a map of foreign property allowances" in {
      val foreignPropertyAllowances = ForeignPropertyAllowances(
        annualInvestmentAllowance = Some(15.15),
        costOfReplacingDomesticItems = Some(25.25),
        zeroEmissionsGoodsVehicleAllowance = Some(35.35),
        otherCapitalAllowance = Some(45.45),
        electricChargePointAllowance = Some(55.55),
        structuredBuildingAllowance = Some(
          Seq(
            StructuredBuildingAllowance(
              amount = 65.65,
              Some(
                StructuredBuildingAllowanceDate(qualifyingDate = LocalDate.now(), qualifyingAmountExpenditure = 50.00)
              ),
              building =
                StructuredBuildingAllowanceBuilding(name = Some("name"), number = Some("number"), postCode = "AB1 2XY")
            )
          )
        ),
        zeroEmissionsCarAllowance = Some(75.75),
        propertyAllowance = Some(85.85)
      )

      val aPropertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(
        submittedOn = Some(LocalDateTime.now()),
        ukOtherProperty = None,
        foreignProperty = Some(
          Seq(
            AnnualForeignProperty(
              countryCode = "ESP",
              adjustments = None,
              allowances = Some(
                ForeignPropertyAllowances(
                  annualInvestmentAllowance = Some(15.15),
                  costOfReplacingDomesticItems = Some(25.25),
                  zeroEmissionsGoodsVehicleAllowance = Some(35.35),
                  otherCapitalAllowance = Some(45.45),
                  electricChargePointAllowance = Some(55.55),
                  structuredBuildingAllowance = Some(
                    Seq(
                      StructuredBuildingAllowance(
                        amount = 65.65,
                        Some(
                          StructuredBuildingAllowanceDate(
                            qualifyingDate = LocalDate.now(),
                            qualifyingAmountExpenditure = 50.00
                          )
                        ),
                        building = StructuredBuildingAllowanceBuilding(
                          name = Some("name"),
                          number = Some("number"),
                          postCode = "AB1 2XY"
                        )
                      )
                    )
                  ),
                  zeroEmissionsCarAllowance = Some(75.75),
                  propertyAllowance = Some(85.85)
                )
              )
            )
          )
        )
      )

      val service = new MergeService()
      val result = service.mergeForeignPropertyAllowances(aPropertyAnnualSubmission, None)
      val expected = ForeignAllowancesAnswers(
        zeroEmissionsCarAllowance = foreignPropertyAllowances.zeroEmissionsCarAllowance,
        zeroEmissionsGoodsVehicleAllowance = foreignPropertyAllowances.zeroEmissionsGoodsVehicleAllowance,
        costOfReplacingDomesticItems = foreignPropertyAllowances.costOfReplacingDomesticItems,
        otherCapitalAllowance = foreignPropertyAllowances.otherCapitalAllowance,
        annualInvestmentAllowance = foreignPropertyAllowances.annualInvestmentAllowance,
        propertyAllowance = foreignPropertyAllowances.propertyAllowance,
        electricChargePointAllowance = foreignPropertyAllowances.electricChargePointAllowance,
        structuredBuildingAllowance = foreignPropertyAllowances.structuredBuildingAllowance,
        capitalAllowancesForACar = None
      )
      result shouldBe Some(
        Map(
          "ESP" -> expected
        )
      )
    }

    "return an empty map when foreign properties have no allowances" in {
      val foreignProperties = Seq(
        AnnualForeignProperty(
          countryCode = "ESP",
          adjustments = None,
          allowances = None
        )
      )

      val propertyAnnualSubmission = PropertyAnnualSubmission(
        submittedOn = None,
        foreignProperty = Some(foreignProperties),
        ukOtherProperty = None
      )

      val service = new MergeService()
      val result = service.mergeForeignPropertyAllowances(propertyAnnualSubmission, None)

      result shouldBe None
    }

  }

  "mergeForeignPropertyAdjustments" should {
    "return a map of foreign property adjustments" in {
      val privateUseAdjustment = BigDecimal(15.25)
      val balancingCharge = BigDecimal(25.35)
      val propertyAllowanceClaim = None
      val foreignPropertyAdjustments = ForeignPropertyAdjustments(
        privateUseAdjustment = Some(privateUseAdjustment), balancingCharge = Some(balancingCharge)
      )
      val foreignPropertyAllowances = ForeignPropertyAllowances(
        annualInvestmentAllowance = Some(15.15),
        costOfReplacingDomesticItems = Some(25.25),
        zeroEmissionsGoodsVehicleAllowance = Some(35.35),
        otherCapitalAllowance = Some(45.45),
        electricChargePointAllowance = Some(55.55),
        structuredBuildingAllowance = Some(
          Seq(
            StructuredBuildingAllowance(
              amount = 65.65,
              Some(
                StructuredBuildingAllowanceDate(
                  qualifyingDate = LocalDate.now(),
                  qualifyingAmountExpenditure = 50.00
                )
              ),
              building = StructuredBuildingAllowanceBuilding(
                name = Some("name"),
                number = Some("number"),
                postCode = "AB1 2XY"
              )
            )
          )
        ),
        zeroEmissionsCarAllowance = Some(75.75),
        propertyAllowance = propertyAllowanceClaim
      )
      val countryCode = "ESP"

      val aPropertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(
        submittedOn = Some(LocalDateTime.now()),
        ukOtherProperty = None,
        foreignProperty = Some(
          Seq(
            AnnualForeignProperty(
              countryCode = countryCode,
              adjustments = Some(foreignPropertyAdjustments),
              allowances = Some(foreignPropertyAllowances)
            )
          )
        )
      )

      val residentialFinancialCost = BigDecimal(66.66)
      val broughtFwdResidentialFinancialCost = BigDecimal(77.77)
      val foreignPropertyExpenses: ForeignPropertyExpenses = ForeignPropertyExpenses(
        premisesRunningCosts = Some(00.01),
        repairsAndMaintenance = Some(11.11),
        financialCosts = Some(22.22),
        professionalFees = Some(33.33),
        travelCosts = Some(44.44),
        costOfServices = Some(55.55),
        residentialFinancialCost = Some(residentialFinancialCost),
        broughtFwdResidentialFinancialCost = Some(broughtFwdResidentialFinancialCost),
        other = Some(88.88),
        consolidatedExpense = Some(99.99),
        None
      )

      val aPropertyPeriodicSubmission: PropertyPeriodicSubmission = PropertyPeriodicSubmission(
        submissionId = None,
        submittedOn = None,
        fromDate = LocalDate.now,
        toDate = LocalDate.now,
        foreignProperty = Some(Seq(ForeignProperty(countryCode, None, Some(foreignPropertyExpenses)))),
        ukOtherProperty = None,
        foreignIncome = None
      )

      val isBalancingCharge = true
      val isForeignUnusedResidentialFinanceCost = true
      val isUnusedLossesPreviousYears = true
      val whenYouReportedTheLoss = ForeignWhenYouReportedTheLoss.y2019to2020
      val repositoryAnswers = Map(
        countryCode -> JourneyAnswers(
          mtditid = Mtditid("some-mtditid"),
          incomeSourceId = IncomeSourceId("some-income-source-id"),
          taxYear = TaxYear(2024),
          journey = JourneyName.ForeignPropertyAdjustments,
          countryCode = Some(countryCode),
          status = InProgress,
          data = Json.toJsObject(ForeignAdjustmentsStoreAnswers(
            isBalancingCharge = isBalancingCharge,
            isForeignUnusedResidentialFinanceCost = Some(isForeignUnusedResidentialFinanceCost),
            isUnusedLossesPreviousYears = isUnusedLossesPreviousYears,
            whenYouReportedTheLoss = Some(whenYouReportedTheLoss)
          )),
          createdAt = Instant.now,
          updatedAt = Instant.now
        )
      )

      val service = new MergeService()
      val result = service.mergeForeignPropertyAdjustments(aPropertyAnnualSubmission, Some(aPropertyPeriodicSubmission), Some(repositoryAnswers))
      val expected = ForeignAdjustmentsAnswers(
          privateUseAdjustment = Some(privateUseAdjustment),
          balancingCharge = Some(BalancingCharge(isBalancingCharge, Some(balancingCharge))),
          residentialFinanceCost = Some(residentialFinancialCost),
          unusedResidentialFinanceCost = Some(ForeignUnusedResidentialFinanceCost(isForeignUnusedResidentialFinanceCost, Some(broughtFwdResidentialFinancialCost))),
          propertyIncomeAllowanceClaim = propertyAllowanceClaim,
          unusedLossesPreviousYears = Some(UnusedLossesPreviousYears(isUnusedLossesPreviousYears, None)),
          whenYouReportedTheLoss = Some(whenYouReportedTheLoss)
        )
      result shouldBe Some(Map(countryCode -> expected))
    }
  }
}
