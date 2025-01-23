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

import models.{ForeignAdjustmentsStoreAnswers, ForeignPropertyExpensesStoreAnswers}
import models.repository.ForeignMerger._
import models.repository.Merger._
import models.request.{BalancingCharge, ForeignSbaInfo, ReversePremiumsReceived}
import models.request.foreign._
import models.request.foreign.adjustments.ForeignWhenYouReportedTheLoss.y2019to2020
import models.request.foreign.adjustments.{ForeignUnusedResidentialFinanceCost, UnusedLossesPreviousYears}
import models.responses._
import utils.UnitTest
import models.request.foreign.expenses.ConsolidatedExpenses

import java.time.{LocalDate, LocalDateTime}

class ForeignMergerSpec extends UnitTest {

  val countryCode = "USA"
  val foreignTaxPaidOrDeducted: Option[BigDecimal] = Some(BigDecimal(56.78))
  val foreignTaxCreditRelief: Option[Boolean] = Some(true)
  val rentIncome: BigDecimal = BigDecimal(12.34)
  val premiumsOfLeaseGrant: Option[BigDecimal] = Some(BigDecimal(13.34))
  val otherPropertyIncome: Option[BigDecimal] = Some(BigDecimal(24.56))
  val consolidatedExpenses: Option[ConsolidatedExpenses] = Some(ConsolidatedExpenses(
    consolidatedOrIndividualExpensesYesNo = true, Some(BigDecimal(90.05))))
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
    ukOtherProperty = None
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
        val foreignIncomeTaxYesOrNo = true
        val foreignPropertyTaxStoreAnswers: Option[Map[String, ForeignPropertyTaxStoreAnswers]] =
          Some(Map(countryCode -> ForeignPropertyTaxStoreAnswers(Some(foreignIncomeTaxYesOrNo))))

        foreignPropertyTaxStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignPropertyTax(
            foreignIncomeTax = Some(ForeignIncomeTax(
              foreignIncomeTaxYesNo = foreignIncomeTaxYesOrNo,
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
              foreignIncomeTaxYesNo = foreignTaxPaidOrDeducted.isDefined,
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
        val reversePremiumsReceived = true
        val calculatedPremiumLeaseTaxable = true
        val twelveMonthPeriodsInLease = Some(BigDecimal(3))
        val receivedGrantLeaseAmount = Some(BigDecimal(22))
        val foreignIncomeStoreAnswers: Option[Map[String, ForeignIncomeStoreAnswers]] =
          Some(Map(countryCode -> ForeignIncomeStoreAnswers(
            premiumsGrantLeaseReceived = premiumsGrantLeaseReceived,
            premiumsOfLeaseGrantAgreed = premiumsOfLeaseGrantAgreed,
            reversePremiumsReceived = reversePremiumsReceived,
            calculatedPremiumLeaseTaxable = calculatedPremiumLeaseTaxable,
            twelveMonthPeriodsInLease = twelveMonthPeriodsInLease,
            receivedGrantLeaseAmount = receivedGrantLeaseAmount
          )))
        foreignIncomeStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(countryCode -> ForeignIncomeAnswers(
            rentIncome = Some(rentIncome),
            premiumsGrantLeaseReceived = premiumsGrantLeaseReceived,
            reversePremiumsReceived = Some(ReversePremiumsReceived(
              reversePremiumsReceived = reversePremiumsReceived, reversePremiums = None
            )),
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
            reversePremiumsReceived = None,
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
        val consolidatedExpensesYesOrNo = true
        val foreignExpensesStoreAnswers: Option[Map[String, ForeignPropertyExpensesStoreAnswers]] =
          Some(Map(countryCode -> ForeignPropertyExpensesStoreAnswers(
            consolidatedExpensesYesOrNo = consolidatedExpensesYesOrNo
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
            balancingChargeYesNo = true,
            foreignUnusedResidentialFinanceCostYesNo = Some(true),
            unusedLossesPreviousYearsYesNo = true,
            whenYouReportedTheLoss = whenYouReportedTheLoss,
          )))
        foreignAdjustmentsStoreAnswers.merge(fromDownstreamMaybe) shouldBe Some(
          Map(
            countryCode -> ForeignAdjustmentsAnswers(
              privateUseAdjustment = privateUseAdjustment,
              balancingCharge = Some(BalancingCharge(balancingChargeYesNo = true, balancingCharge)),
              residentialFinanceCost = residentialFinanceCost,
              unusedResidentialFinanceCost = Some(ForeignUnusedResidentialFinanceCost(
                foreignUnusedResidentialFinanceCostYesNo = true,
                foreignUnusedResidentialFinanceCostAmount = unusedResidentialFinanceCost
              )),
              unusedLossesPreviousYears = Some(UnusedLossesPreviousYears(unusedLossesPreviousYearsYesNo = true, None)),
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
              balancingCharge = Some(BalancingCharge(balancingChargeYesNo = true, balancingCharge)),
              residentialFinanceCost = residentialFinanceCost,
              unusedResidentialFinanceCost = Some(ForeignUnusedResidentialFinanceCost(
                foreignUnusedResidentialFinanceCostYesNo = true,
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
  }
}
