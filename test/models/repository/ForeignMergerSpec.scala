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

import models.repository.ForeignMerger._
import models.repository.Merger._
import models.request.foreign._
import models.request.foreign.adjustments.ForeignWhenYouReportedTheLoss.y2019to2020
import models.request.foreign.adjustments.{ForeignUnusedResidentialFinanceCost, UnusedLossesPreviousYears}
import models.request.foreign.allowances.{CapitalAllowancesForACar, ForeignAllowancesAnswers}
import models.request.foreign.expenses.ConsolidatedExpenses
import models.request.{BalancingCharge, ForeignSbaInfo}
import models.responses._
import models.{ForeignAdjustmentsStoreAnswers, ForeignAllowancesStoreAnswers, ForeignPropertyExpensesStoreAnswers}
import utils.UnitTest

import java.time.{LocalDate, LocalDateTime}

class ForeignMergerSpec extends UnitTest {

  val countryCode = "USA"
  val foreignTaxPaidOrDeducted: Option[BigDecimal] = Some(BigDecimal(56.78))
  val foreignTaxCreditRelief: Option[Boolean] = Some(true)
  val rentIncome: BigDecimal = BigDecimal(12.34)
  val premiumsOfLeaseGrant: Option[BigDecimal] = Some(BigDecimal(13.34))
  val otherPropertyIncome: Option[BigDecimal] = Some(BigDecimal(24.56))
  val consolidatedExpenses: Option[ConsolidatedExpenses] = Some(ConsolidatedExpenses(
    isConsolidatedOrIndividualExpenses = true, Some(BigDecimal(90.05))))
  val premisesRunningCosts: Option[BigDecimal] = Some(BigDecimal(65.43))
  val repairsAndMaintenance: Option[BigDecimal] = Some(BigDecimal(32.21))
  val financialCosts: Option[BigDecimal] = Some(BigDecimal(54.32))
  val costOfServices: Option[BigDecimal] = Some(BigDecimal(22.22))
  val professionalFees: Option[BigDecimal] = Some(BigDecimal(65.43))
  val other: Option[BigDecimal] = Some(BigDecimal(44.44))
  val privateUseAdjustment: Option[BigDecimal] = Some(BigDecimal(543.21))
  val balancingCharge: Option[BigDecimal] = Some(BigDecimal(123.45))
  val residentialFinanceCost: Option[BigDecimal] = Some(BigDecimal(11.11))
  val unusedResidentialFinanceCost: Option[BigDecimal] = Some(BigDecimal(23.22))
  val propertyAllowance: Option[BigDecimal] = Some(BigDecimal(85.85))
  val annualInvestmentAllowance: Option[BigDecimal] = Some(15.15)
  val costOfReplacingDomesticItems: Option[BigDecimal] = Some(25.25)
  val zeroEmissionsGoodsVehicleAllowance: Option[BigDecimal] = Some(35.35)
  val otherCapitalAllowance: Option[BigDecimal] = Some(45.45)
  val electricChargePointAllowance: Option[BigDecimal] = Some(55.55)
  val zeroEmissionsCarAllowance: Option[BigDecimal] = Some(75.75)
  val aPropertyPeriodicSubmission: PropertyPeriodicSubmission = PropertyPeriodicSubmission(
    submissionId = None,
    submittedOn = None,
    fromDate = LocalDate.now(),
    toDate = LocalDate.now(),
    foreignProperty = Some(Seq(
      ForeignProperty(
        countryCode = countryCode,
        income = Some(ForeignPropertyIncome(
          rentIncome = Some(ForeignPropertyRentIncome(rentAmount = rentIncome)),
          foreignTaxCreditRelief = foreignTaxCreditRelief,
          premiumsOfLeaseGrant = premiumsOfLeaseGrant,
          otherPropertyIncome = otherPropertyIncome,
          foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted,
          specialWithholdingTaxOrUkTaxPaid = Some(BigDecimal(89.10))
        )),
        expenses = Some(ForeignPropertyExpenses(
          premisesRunningCosts = premisesRunningCosts,
          repairsAndMaintenance = repairsAndMaintenance,
          financialCosts = financialCosts,
          professionalFees = professionalFees,
          travelCosts = Some(BigDecimal(22.22)),
          costOfServices = costOfServices,
          residentialFinancialCost = residentialFinanceCost,
          broughtFwdResidentialFinancialCost = unusedResidentialFinanceCost,
          other = other,
          consolidatedExpense = consolidatedExpenses.flatMap(_.consolidatedExpense),
          consolidatedExpenseAmount = None
        ))
      )
    )),
    ukOtherProperty = None,
    foreignIncome = None
  )

  val aPropertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(
    submittedOn = Some(LocalDateTime.now()),
    ukOtherProperty = None,
    foreignProperty = Some(
      Seq(
        AnnualForeignProperty(
          countryCode = countryCode,
          adjustments = Some(ForeignPropertyAdjustments(
            privateUseAdjustment = privateUseAdjustment,
            balancingCharge = balancingCharge
          )),
          allowances = Some(
            ForeignPropertyAllowances(
              annualInvestmentAllowance = annualInvestmentAllowance,
              costOfReplacingDomesticItems = costOfReplacingDomesticItems,
              zeroEmissionsGoodsVehicleAllowance = zeroEmissionsGoodsVehicleAllowance,
              otherCapitalAllowance = otherCapitalAllowance,
              electricChargePointAllowance = electricChargePointAllowance,
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
              propertyAllowance = propertyAllowance
            )
          )
        )
      )
    )
  )

  "ForeignMerger" should {

    "merge foreign tax from downstream response and from repo into response model" when {

      val fromDownstreamMaybe: Option[Map[String, ForeignPropertyIncome]] = for {
        foreignProperties <- aPropertyPeriodicSubmission.foreignProperty
        foreignProperty <- foreignProperties.headOption
        foreignPropertyIncome <- foreignProperty.income
      } yield Map(countryCode -> foreignPropertyIncome)

      "store answers are available in the repo" in {
        val isForeignIncomeTax = true
        val foreignPropertyTaxStoreAnswers: Option[Map[String, ForeignPropertyTaxStoreAnswers]] =
          Some(Map(countryCode -> ForeignPropertyTaxStoreAnswers(Some(isForeignIncomeTax))))

        foreignPropertyTaxStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignPropertyTax(
            foreignIncomeTax = Some(ForeignIncomeTax(
              isForeignIncomeTax = isForeignIncomeTax,
              foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted
            )),
            foreignTaxCreditRelief = foreignTaxCreditRelief
          ))
        )
      }

      "store answers are not available in the repo" in {
        val foreignPropertyTaxStoreAnswers: Option[Map[String, ForeignPropertyTaxStoreAnswers]] = None
        foreignPropertyTaxStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignPropertyTax(
            foreignIncomeTax = Some(ForeignIncomeTax(
              isForeignIncomeTax = foreignTaxPaidOrDeducted.isDefined,
              foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted
            )),
            foreignTaxCreditRelief = foreignTaxCreditRelief
          ))
        )
      }
    }

    "merge foreign income from downstream response and from repo into response model" when {

      val fromDownstreamMaybe: Option[Map[String, ForeignPropertyIncome]] = for {
        foreignProperties <- aPropertyPeriodicSubmission.foreignProperty
        foreignProperty <- foreignProperties.headOption
        foreignPropertyIncome <- foreignProperty.income
      } yield Map(foreignProperty.countryCode -> foreignPropertyIncome)

      "store answers are available in the repo" in {
        val premiumsGrantLeaseReceived = true
        val premiumsOfLeaseGrantAgreed = true
        val calculatedPremiumLeaseTaxable = true
        val twelveMonthPeriodsInLease = Some(BigDecimal(3))
        val receivedGrantLeaseAmount = Some(BigDecimal(22))
        val foreignIncomeStoreAnswers: Option[Map[String, ForeignIncomeStoreAnswers]] =
          Some(Map(countryCode -> ForeignIncomeStoreAnswers(
            premiumsGrantLeaseReceived = premiumsGrantLeaseReceived,
            premiumsOfLeaseGrantAgreed = premiumsOfLeaseGrantAgreed,
            calculatedPremiumLeaseTaxable = calculatedPremiumLeaseTaxable,
            twelveMonthPeriodsInLease = twelveMonthPeriodsInLease,
            receivedGrantLeaseAmount = receivedGrantLeaseAmount
          )))
        foreignIncomeStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignIncomeAnswers(
            rentIncome = Some(rentIncome),
            premiumsGrantLeaseReceived = premiumsGrantLeaseReceived,
            otherPropertyIncome = otherPropertyIncome,
            calculatedPremiumLeaseTaxable = Some(CalculatedPremiumLeaseTaxable(
              calculatedPremiumLeaseTaxable, None
            )),
            receivedGrantLeaseAmount = receivedGrantLeaseAmount,
            twelveMonthPeriodsInLease = twelveMonthPeriodsInLease,
            premiumsOfLeaseGrantAgreed = Some(PremiumsOfLeaseGrantAgreed(
              premiumsOfLeaseGrantAgreed = true, premiumsOfLeaseGrant = premiumsOfLeaseGrant
            ))
          ))
        )
      }

      "store answers are not available in the repo" in {
        val foreignIncomeStoreAnswers: Option[Map[String, ForeignIncomeStoreAnswers]] = None
        foreignIncomeStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignIncomeAnswers(
            rentIncome = Some(rentIncome),
            premiumsGrantLeaseReceived = true,
            otherPropertyIncome = otherPropertyIncome,
            calculatedPremiumLeaseTaxable = None,
            receivedGrantLeaseAmount = None,
            twelveMonthPeriodsInLease = None,
            premiumsOfLeaseGrantAgreed = Some(PremiumsOfLeaseGrantAgreed(
              premiumsOfLeaseGrantAgreed = true, premiumsOfLeaseGrant = premiumsOfLeaseGrant
            ))
          ))
        )
      }
    }

    "merge foreign expenses from downstream response and from repo into response model" when {
      val fromDownstreamMaybe: Option[Map[String, ForeignPropertyExpenses]] = for {
        foreignProperties <- aPropertyPeriodicSubmission.foreignProperty
        foreignProperty <- foreignProperties.headOption
        foreignPropertyExpenses <- foreignProperty.expenses
      } yield Map(foreignProperty.countryCode -> foreignPropertyExpenses)

      "store answers are available in the repo" in {
        val isConsolidatedExpenses = true
        val foreignExpensesStoreAnswers: Option[Map[String, ForeignPropertyExpensesStoreAnswers]] =
          Some(Map(countryCode -> ForeignPropertyExpensesStoreAnswers(
            isConsolidatedExpenses = isConsolidatedExpenses
          )))
        foreignExpensesStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignExpensesAnswers(
            consolidatedExpenses = consolidatedExpenses,
            premisesRunningCosts = premisesRunningCosts,
            repairsAndMaintenance = repairsAndMaintenance,
            financialCosts = financialCosts,
            professionalFees = professionalFees,
            costOfServices = costOfServices,
            other = other
          ))
        )
      }

      "store answers are not available in the repo" in {
        val foreignExpensesStoreAnswers: Option[Map[String, ForeignPropertyExpensesStoreAnswers]] = None
        foreignExpensesStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignExpensesAnswers(
            consolidatedExpenses = consolidatedExpenses,
            premisesRunningCosts = premisesRunningCosts,
            repairsAndMaintenance = repairsAndMaintenance,
            financialCosts = financialCosts,
            professionalFees = professionalFees,
            costOfServices = costOfServices,
            other = other
          ))
        )
      }
    }

    "merge foreign sba from downstream response and from repo into response model" when {
      val fromDownstreamMaybe: Option[Map[String, Option[Seq[StructuredBuildingAllowance]]]] = for {
        foreignProperties <- aPropertyAnnualSubmission.foreignProperty
        foreignProperty <- foreignProperties.headOption
        foreignPropertyAllowances <- foreignProperty.allowances
      } yield Map(foreignProperty.countryCode -> foreignPropertyAllowances.structuredBuildingAllowance)

      "store answers are available in the repo" in {

        val foreignPropertySbaStoreAnswers: Option[Map[String, ForeignPropertySbaStoreAnswers]] =
          Some(Map(countryCode -> ForeignPropertySbaStoreAnswers(claimStructureBuildingAllowance = true)))

        foreignPropertySbaStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(
            countryCode -> ForeignSbaInfo(
              claimStructureBuildingAllowance = true,
              Some(
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
              )
            )
          )
        )
      }

      "store answers are not available in the repo" in {
        val foreignPropertySbaStoreAnswers: Option[Map[String, ForeignPropertySbaStoreAnswers]] = None
        foreignPropertySbaStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(
            countryCode -> ForeignSbaInfo(
              claimStructureBuildingAllowance = true,
              Some(
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
              )
            )
          )
        )
      }
    }

    "merge foreign adjustments from downstream response and from repo into response model " when {

      val fromDownstreamMaybe: Option[Map[String, (ForeignPropertyAdjustments, Option[BigDecimal], ForeignPropertyExpenses)]] = {
        aPropertyAnnualSubmission.foreignProperty.map { annualForeignProperties =>
          annualForeignProperties.flatMap { annualForeignProperty: AnnualForeignProperty =>
            for {
              adjustments <- annualForeignProperty.adjustments
              periodicForeignProperties <- aPropertyPeriodicSubmission.foreignProperty
              periodicForeignProperty <- periodicForeignProperties.find(_.countryCode == annualForeignProperty.countryCode)
              expenses <- periodicForeignProperty.expenses
            } yield annualForeignProperty.countryCode -> (adjustments, annualForeignProperty.allowances.flatMap(_.propertyAllowance), expenses)
          }.toMap
        }
      }

      "store answers are available in the repo" in {
        val whenYouReportedTheLoss = Some(y2019to2020)
        val foreignAdjustmentsStoreAnswers: Option[Map[String, ForeignAdjustmentsStoreAnswers]] =
          Some(Map(countryCode -> ForeignAdjustmentsStoreAnswers(
            isBalancingCharge = true,
            isForeignUnusedResidentialFinanceCost = Some(true),
            isUnusedLossesPreviousYears = true,
            whenYouReportedTheLoss = whenYouReportedTheLoss,
          )))
        foreignAdjustmentsStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(
            countryCode -> ForeignAdjustmentsAnswers(
              privateUseAdjustment = privateUseAdjustment,
              balancingCharge = Some(BalancingCharge(isBalancingCharge = true, balancingCharge)),
              residentialFinanceCost = residentialFinanceCost,
              unusedResidentialFinanceCost = Some(ForeignUnusedResidentialFinanceCost(
                isForeignUnusedResidentialFinanceCost = true,
                foreignUnusedResidentialFinanceCostAmount = unusedResidentialFinanceCost
              )),
              unusedLossesPreviousYears = Some(UnusedLossesPreviousYears(isUnusedLossesPreviousYears = true, None)),
              whenYouReportedTheLoss = whenYouReportedTheLoss,
              propertyIncomeAllowanceClaim = propertyAllowance
            )
          )
        )
      }

      "store answers are not available in the repo" in {
        val foreignAdjustmentsStoreAnswers: Option[Map[String, ForeignAdjustmentsStoreAnswers]] = None
        foreignAdjustmentsStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(
            countryCode -> ForeignAdjustmentsAnswers(
              privateUseAdjustment = privateUseAdjustment,
              balancingCharge = Some(BalancingCharge(isBalancingCharge = true, balancingCharge)),
              residentialFinanceCost = residentialFinanceCost,
              unusedResidentialFinanceCost = Some(ForeignUnusedResidentialFinanceCost(
                isForeignUnusedResidentialFinanceCost = true,
                foreignUnusedResidentialFinanceCostAmount = unusedResidentialFinanceCost
              )),
              unusedLossesPreviousYears = None,
              whenYouReportedTheLoss = None,
              propertyIncomeAllowanceClaim = propertyAllowance
            )
          )
        )
      }
    }

    "merge foreign allowances from downstream response and from repo into response model" when {
      val fromDownstreamMaybe: Option[Map[String, ForeignPropertyAllowances]] = {
        aPropertyAnnualSubmission.foreignProperty.map { annualForeignProperties =>
          annualForeignProperties.flatMap { annualForeignProperty: AnnualForeignProperty =>
            for {
              allowances <- annualForeignProperty.allowances
            } yield annualForeignProperty.countryCode -> allowances
          }.toMap
        }
      }

      "store answers are available in the repo" in {
        val isCapitalAllowancesForACar = Some(true)
        val foreignAllowancesStoreAnswers: Option[Map[String, ForeignAllowancesStoreAnswers]] =
          Some(
            Map(
              countryCode -> ForeignAllowancesStoreAnswers(
                None,
                None,
                None,
                None,
                isCapitalAllowancesForACar = isCapitalAllowancesForACar
              )
            )
          )
        val mergedResult: Option[Map[String, ForeignAllowancesAnswers]] =
          foreignAllowancesStoreAnswers.merge(fromDownstreamMaybe)
        val Some(result): Option[ForeignAllowancesAnswers] = mergedResult.flatMap(_.get(countryCode))
        result.annualInvestmentAllowance shouldBe annualInvestmentAllowance
        result.costOfReplacingDomesticItems shouldBe costOfReplacingDomesticItems
        result.zeroEmissionsGoodsVehicleAllowance shouldBe zeroEmissionsGoodsVehicleAllowance
        result.otherCapitalAllowance shouldBe otherCapitalAllowance
        result.electricChargePointAllowance shouldBe electricChargePointAllowance
        result.zeroEmissionsCarAllowance shouldBe zeroEmissionsCarAllowance
        result.capitalAllowancesForACar shouldBe Some(CapitalAllowancesForACar(
          isCapitalAllowancesForACar = true,
          capitalAllowancesForACarAmount = otherCapitalAllowance))
      }

      "store answers are not available in the repo" in {
        val foreignAllowancesStoreAnswers : Option[Map[String, ForeignAllowancesStoreAnswers]] = None
        val mergedResult: Option[Map[String, ForeignAllowancesAnswers]] = foreignAllowancesStoreAnswers.merge(fromDownstreamMaybe)
        val Some(result): Option[ForeignAllowancesAnswers] = mergedResult.flatMap(_.get(countryCode))
        result.annualInvestmentAllowance shouldBe annualInvestmentAllowance
        result.costOfReplacingDomesticItems shouldBe costOfReplacingDomesticItems
        result.zeroEmissionsGoodsVehicleAllowance shouldBe zeroEmissionsGoodsVehicleAllowance
        result.otherCapitalAllowance shouldBe otherCapitalAllowance
        result.electricChargePointAllowance shouldBe electricChargePointAllowance
        result.zeroEmissionsCarAllowance shouldBe zeroEmissionsCarAllowance
        result.capitalAllowancesForACar shouldBe None
      }
    }
  }
}
