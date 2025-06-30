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
import models.common.JourneyStatus.InProgress
import models.common.{IncomeSourceId, JourneyName, Mtditid, TaxYear}
import models.domain.{JourneyAnswers, JourneyWithStatus}
import models.request.foreign.adjustments.{ForeignUnusedResidentialFinanceCost, ForeignWhenYouReportedTheLoss, UnusedLossesPreviousYears}
import models.request.foreign.allowances.{ForeignAllowancesAnswers, CapitalAllowancesForACar => ForeignCapitalAllowancesForACar}
import models.request.foreign.expenses.{ConsolidatedExpenses => ForeignConsolidatedExpenses}
import models.request.foreign._
import models.request.foreignincome.{ForeignDividend, ForeignDividendsAnswers, ForeignIncomeSubmission, GrossAmountWithReference}
import models.request._
import models.responses._
import models._
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
    Some(UkOtherPropertyExpenses(Some(BigDecimal(2340)), Some(BigDecimal(2341)), Some(BigDecimal(2342)), None, None, None, None, None, None, None, Some(BigDecimal(99))))
  )
  val otherAnnualUkProperty: AnnualUkOtherProperty = AnnualUkOtherProperty(
    Some(UkOtherAdjustments(None, None, Some(BigDecimal(567)), Some(BigDecimal(2340)), Some(true), None, None, None)),
    Some(UkOtherAllowances(Some(BigDecimal(2340)), Some(BigDecimal(2341)), Some(BigDecimal(2342)), None, None, None, None, None, None, None))
  )

  val propertyPeriodicSubmission: PropertyPeriodicSubmission = PropertyPeriodicSubmission(
    Some(PeriodicSubmissionId("periodicSubmissionId")),
    submittedOn = Some(LocalDateTime.now),
    fromDate = LocalDate.now(),
    toDate = LocalDate.now(),
    foreignProperty = None,
    ukOtherProperty = Some(otherUkProperty)
  )

  val propertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(
    submittedOn = Some(LocalDateTime.now),
    foreignProperty = None,
    ukOtherProperty = Some(otherAnnualUkProperty)
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

  "mergeRentalsAndRaRIncome" should {
    "merge correctly with valid inputs" in {
      val mockData: JsObject = Json.obj(
        "isNonUKLandlord" -> true
      )
      when(mockJourneyAnswers.data).thenReturn(mockData)

      val result = mergeService.mergeRentalsAndRaRIncome(Some(propertyPeriodicSubmission), Some(mockJourneyAnswers))

      result shouldBe Some(
        RentalsAndRaRIncome(
          isNonUKLandlord = true,
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
      val result = mergeService.mergeRentalsAndRaRIncome(None, None)

      result shouldBe None
    }
  }

  "mergeRentalsExpenses" should {
    "merge correctly with valid inputs" in {
      val mockData: JsObject = Json.obj(
        "isConsolidatedExpenses" -> true
      )
      when(mockJourneyAnswers.data).thenReturn(mockData)

      val result = mergeService.mergeRentalsExpenses(Some(propertyPeriodicSubmission), Some(mockJourneyAnswers))

      result shouldBe Some(
        PropertyRentalsExpense(
          Some(ConsolidatedExpenses(isConsolidatedExpenses = true, Some(99))),
          Some(BigDecimal(2340)),
          Some(BigDecimal(2341)),
          Some(BigDecimal(2342)),
          None,
          None,
          None,
          None
        )
      )
    }

    "return None when no data is available" in {
      val result = mergeService.mergeRentalsExpenses(None, None)

      result shouldBe None
    }
  }

  "mergeRaRExpenses" should {
    "merge correctly with valid inputs" in {
      val mockData: JsObject = Json.obj(
        "isConsolidatedExpenses" -> true
      )
      when(mockJourneyAnswers.data).thenReturn(mockData)

      val result = mergeService.mergeRaRExpenses(Some(propertyPeriodicSubmission), Some(mockJourneyAnswers))

      result shouldBe Some(
        RentARoomExpenses(
          Some(ConsolidatedExpenses(isConsolidatedExpenses = true, Some(99))),
          Some(BigDecimal(2340)),
          Some(BigDecimal(2341)),
          None,
          None,
          None
        )
      )
    }

    "return None when no data is available" in {
      val result = mergeService.mergeRaRExpenses(None, None)

      result shouldBe None
    }
  }

  "mergeAllowances" should {
    "merge correctly with valid inputs" in {
      val mockData: JsObject = Json.obj(
        "isCapitalAllowancesForACar" -> true
      )
      when(mockJourneyAnswers.data).thenReturn(mockData)

      val result = mergeService.mergeAllowances(propertyAnnualSubmission, Some(mockJourneyAnswers))

      result shouldBe Some(
        RentalAllowances(
          Some(CapitalAllowancesForACar(isCapitalAllowancesForACar = true, None)),
          Some(BigDecimal(2340)),
          None,
          Some(BigDecimal(2341)),
          Some(BigDecimal(2342)),
          None,
          None
        )
      )
    }

    "return None merged when no data is available" in {
      val result = mergeService.mergeAllowances(propertyAnnualSubmission, None)

      result shouldBe Some(
        RentalAllowances(
          None,
          Some(BigDecimal(2340)),
          None,
          Some(BigDecimal(2341)),
          Some(BigDecimal(2342)),
          None,
          None
        )
      )
    }
  }

  "mergeRaRAllowances" should {
    "merge correctly with valid inputs" in {
      val mockData: JsObject = Json.obj(
        "isRaRCapitalAllowancesForACar" -> true
      )
      when(mockJourneyAnswers.data).thenReturn(mockData)

      val result = mergeService.mergeRaRAllowances(propertyAnnualSubmission, Some(mockJourneyAnswers))

      result shouldBe Some(
        RentARoomAllowances(
          None,
          None,
          Some(BigDecimal(2341)),
          None,
          None
        )
      )
    }

    "return None merged when no data is available" in {
      val result = mergeService.mergeRaRAllowances(propertyAnnualSubmission, None)

      result shouldBe Some(
        RentARoomAllowances(
          None,
          None,
          Some(BigDecimal(2341)),
          None,
          None
        )
      )
    }
  }

  "mergePropertyAbout" should {
    "merge correctly with valid inputs" in {
      val mockData: JsObject = Json.obj(
        "totalIncome" -> "200"
      )
      when(mockJourneyAnswers.data).thenReturn(mockData)

      val result = mergeService.mergePropertyAbout(Some(mockJourneyAnswers))

      result shouldBe Some(
        PropertyAbout(
          "200",
          None,
          None
        )
      )
    }

    "return None when no data is available" in {
      val result = mergeService.mergePropertyAbout(None)

      result shouldBe None
    }
  }

  /* TODO Finish the rest of the coverage for the following methods
  * mergeUkAndForeignPropertyAbout, mergeForeignPropertySelectCountry, mergePropertyRentalsAbout, mergeRentalsAndRaRAbout
  * mergeRaRAbout, mergeStatuses, mergeAdjustments, mergeRaRAdjustments, mergeEsbaInfo, mergeSbaInfo, mergeForeignPropertyTax
  * mergeForeignPropertySba, mergeForeignIncomeStatuses, mergeForeignStatuses
  * */

  "mergeForeignPropertyAllowances" should {

    "return a map of foreign property allowances" in {
      val zeroEmissionsCarAllowance = BigDecimal(75.75)
      val zeroEmissionsGoodsVehicleAllowance = BigDecimal(35.35)
      val costOfReplacingDomesticItems = BigDecimal(25.25)
      val otherCapitalAllowance = BigDecimal(45.45)
      val isCapitalAllowancesForACar = false
      val capitalAllowancesForACarAmount = None
      val countryCode = "GRE"

      val foreignPropertyAllowances = ForeignPropertyAllowances(
        annualInvestmentAllowance = Some(15.15),
        costOfReplacingDomesticItems = Some(costOfReplacingDomesticItems),
        zeroEmissionsGoodsVehicleAllowance = Some(zeroEmissionsGoodsVehicleAllowance),
        otherCapitalAllowance = Some(otherCapitalAllowance),
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
        zeroEmissionsCarAllowance = Some(zeroEmissionsCarAllowance),
        propertyAllowance = Some(85.85)
      )

      val aPropertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(
        submittedOn = Some(LocalDateTime.now()),
        ukOtherProperty = None,
        foreignProperty = Some(
          Seq(
            AnnualForeignProperty(
              countryCode = countryCode,
              adjustments = None,
              allowances = Some(
                ForeignPropertyAllowances(
                  annualInvestmentAllowance = Some(15.15),
                  costOfReplacingDomesticItems = Some(costOfReplacingDomesticItems),
                  zeroEmissionsGoodsVehicleAllowance = Some(zeroEmissionsGoodsVehicleAllowance),
                  otherCapitalAllowance = Some(otherCapitalAllowance),
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
                  zeroEmissionsCarAllowance = Some(zeroEmissionsCarAllowance),
                  propertyAllowance = Some(85.85)
                )
              )
            )
          )
        )
      )

      val repositoryAnswers = Some(
        Map(
        countryCode -> JourneyAnswers(
          mtditid = Mtditid("some-mtditid"),
          incomeSourceId = IncomeSourceId("some-income-source-id"),
          taxYear = TaxYear(2024),
          journey = JourneyName.ForeignPropertyAdjustments,
          countryCode = Some(countryCode),
          status = InProgress,
          data = Json.toJsObject(ForeignAllowancesStoreAnswers(
            zeroEmissionsCarAllowance = Some(zeroEmissionsCarAllowance),
            zeroEmissionsGoodsVehicleAllowance = Some(zeroEmissionsGoodsVehicleAllowance),
            costOfReplacingDomesticItems = Some(costOfReplacingDomesticItems),
            otherCapitalAllowance = Some(otherCapitalAllowance),
            isCapitalAllowancesForACar = None
          )),
          createdAt = Instant.now,
          updatedAt = Instant.now
        )))

      val service = new MergeService()
      val result = service.mergeForeignPropertyAllowances(aPropertyAnnualSubmission, repositoryAnswers)
      val expected = ForeignAllowancesAnswers(
        zeroEmissionsCarAllowance = foreignPropertyAllowances.zeroEmissionsCarAllowance,
        zeroEmissionsGoodsVehicleAllowance = foreignPropertyAllowances.zeroEmissionsGoodsVehicleAllowance,
        costOfReplacingDomesticItems = foreignPropertyAllowances.costOfReplacingDomesticItems,
        otherCapitalAllowance = foreignPropertyAllowances.otherCapitalAllowance,
        annualInvestmentAllowance = foreignPropertyAllowances.annualInvestmentAllowance,
        propertyAllowance = foreignPropertyAllowances.propertyAllowance,
        electricChargePointAllowance = foreignPropertyAllowances.electricChargePointAllowance,
        structuredBuildingAllowance = foreignPropertyAllowances.structuredBuildingAllowance,
        capitalAllowancesForACar = Some(ForeignCapitalAllowancesForACar(isCapitalAllowancesForACar, capitalAllowancesForACarAmount))
      )
      result shouldBe Some(
        Map(
          countryCode -> expected
        )
      )
    }

    "return an empty map when foreign properties have no allowances" in {
      val foreignProperties = Seq(
        AnnualForeignProperty(
          countryCode = "GRE",
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

  "mergeForeignPropertyIncome" should {

    "return a map of foreign property income" in {
      val countryCode = "GRE"
      val rentIncome = ForeignPropertyRentIncome(BigDecimal(12.34))
      val foreignTaxCreditRelief = true
      val premiumsGrantLeaseReceived = true
      val premiumsOfLeaseGrant = BigDecimal(56.78)
      val otherPropertyIncome = BigDecimal(90.12)
      val calculatedPremiumLeaseTaxable = false
      val twelveMonthPeriodsInLease = BigDecimal(2)
      val premiumsOfLeaseGrantAgreed = true
      val foreignTaxPaidOrDeducted = BigDecimal(34.56)
      val specialWithholdingTaxOrUkTaxPaid = BigDecimal(78.90)
      val fromDate = LocalDate.now()
      val toDate = LocalDate.now()
      val submissionId = PeriodicSubmissionId("submissionId")

      val foreignPropertyIncome = ForeignPropertyIncome(
        rentIncome = Some(rentIncome),
        foreignTaxCreditRelief = Some(foreignTaxCreditRelief),
        premiumsOfLeaseGrant = Some(premiumsOfLeaseGrant),
        otherPropertyIncome = Some(otherPropertyIncome),
        foreignTaxPaidOrDeducted = Some(foreignTaxPaidOrDeducted),
        specialWithholdingTaxOrUkTaxPaid = Some(specialWithholdingTaxOrUkTaxPaid)
      )

      val foreignIncomeAnswers = ForeignIncomeAnswers(
        rentIncome = Some(rentIncome.rentAmount),
        premiumsGrantLeaseReceived = premiumsGrantLeaseReceived,
        otherPropertyIncome = Some(otherPropertyIncome),
        calculatedPremiumLeaseTaxable = Some(CalculatedPremiumLeaseTaxable(calculatedPremiumLeaseTaxable, None)),
        receivedGrantLeaseAmount = Some(premiumsOfLeaseGrant),
        twelveMonthPeriodsInLease = Some(twelveMonthPeriodsInLease),
        premiumsOfLeaseGrantAgreed = Some(PremiumsOfLeaseGrantAgreed(premiumsOfLeaseGrantAgreed, Some(premiumsOfLeaseGrant)))
      )

      val aPropertyPeriodicSubmission: PropertyPeriodicSubmission = PropertyPeriodicSubmission(
        submissionId = Some(submissionId),
        submittedOn = Some(LocalDateTime.now()),
        ukOtherProperty = None,
        foreignProperty = Some(
          Seq(
            ForeignProperty(
              countryCode = countryCode,
              income = Some(foreignPropertyIncome),
              expenses = None
            )
          )
        ),
        fromDate = fromDate,
        toDate = toDate
      )

      val repositoryAnswers = Some(
        Map(
          countryCode -> JourneyAnswers(
            mtditid = Mtditid("some-mtditid"),
            incomeSourceId = IncomeSourceId("some-income-source-id"),
            taxYear = TaxYear(2024),
            journey = JourneyName.ForeignPropertyIncome,
            countryCode = Some(countryCode),
            status = InProgress,
            data = Json.toJsObject(ForeignIncomeStoreAnswers(
              premiumsGrantLeaseReceived = premiumsGrantLeaseReceived,
              premiumsOfLeaseGrantAgreed = premiumsOfLeaseGrantAgreed,
              calculatedPremiumLeaseTaxable = calculatedPremiumLeaseTaxable,
              twelveMonthPeriodsInLease = Some(twelveMonthPeriodsInLease),
              receivedGrantLeaseAmount = Some(premiumsOfLeaseGrant)
            )),
            createdAt = Instant.now,
            updatedAt = Instant.now
          )))

      val service = new MergeService()
      val result = service.mergeForeignPropertyIncome(Some(aPropertyPeriodicSubmission), repositoryAnswers)
      val expected = foreignIncomeAnswers
      result shouldBe Some(
        Map(
          countryCode -> expected
        )
      )
    }

    "return an empty map when foreign properties have no allowances" in {
      val foreignProperties = Seq(
        ForeignProperty(
                countryCode = "GRE",
                income = None,
                expenses = None
              )
        )

      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        submissionId = None,
        submittedOn = Some(LocalDateTime.now()),
        ukOtherProperty = None,
        foreignProperty = Some(foreignProperties),
        fromDate = LocalDate.now(),
        toDate = LocalDate.now()
      )

      val service = new MergeService()
      val result = service.mergeForeignPropertyIncome(Some(propertyPeriodicSubmission), None)

      result shouldBe None
    }

  }

  "mergeForeignPropertyExpenses" should {

    "return a map of foreign property expenses" in {
      val countryCode = "GRE"
      val premisesRunningCosts = BigDecimal(12.34)
      val repairsAndMaintenance = BigDecimal(56.78)
      val financialCosts = BigDecimal(22.34)
      val professionalFees = BigDecimal(90.12)
      val travelCosts = BigDecimal(34.56)
      val costOfServices = BigDecimal(78.90)
      val residentialFinancialCost = BigDecimal(34.56)
      val broughtFwdResidentialFinancialCost = BigDecimal(78.90)
      val other = BigDecimal(12.34)
      val consolidatedExpense = BigDecimal(56.78)
      val consolidateExpenseAmount = BigDecimal(90.12)
      val fromDate = LocalDate.now()
      val toDate = LocalDate.now()
      val isConsolidatedExpenses = true
      val submissionId = PeriodicSubmissionId("submissionId")

      val foreignPropertyExpenses = ForeignPropertyExpenses(
        premisesRunningCosts = Some(premisesRunningCosts),
        repairsAndMaintenance = Some(repairsAndMaintenance),
        financialCosts = Some(financialCosts),
        professionalFees = Some(professionalFees),
        travelCosts = Some(travelCosts),
        costOfServices = Some(costOfServices),
        residentialFinancialCost = Some(residentialFinancialCost),
        broughtFwdResidentialFinancialCost = Some(broughtFwdResidentialFinancialCost),
        other = Some(other),
        consolidatedExpense = Some(consolidatedExpense),
        consolidatedExpenseAmount = Some(consolidateExpenseAmount)
      )

      val foreignExpensesAnswers = ForeignExpensesAnswers(
        consolidatedExpenses = Some(ForeignConsolidatedExpenses(isConsolidatedExpenses, Some(consolidatedExpense))),
        premisesRunningCosts = Some(premisesRunningCosts),
        repairsAndMaintenance = Some(repairsAndMaintenance),
        financialCosts = Some(financialCosts),
        professionalFees = Some(professionalFees),
        costOfServices = Some(costOfServices),
        other = Some(other)
      )

      val aPropertyPeriodicSubmission: PropertyPeriodicSubmission = PropertyPeriodicSubmission(
        submissionId = Some(submissionId),
        submittedOn = Some(LocalDateTime.now()),
        ukOtherProperty = None,
        foreignProperty = Some(
          Seq(
            ForeignProperty(
              countryCode = countryCode,
              income = None,
              expenses = Some(foreignPropertyExpenses)
            )
          )
        ),
        fromDate = fromDate,
        toDate = toDate
      )

      val repositoryAnswers = Some(
        Map(
          countryCode -> JourneyAnswers(
            mtditid = Mtditid("some-mtditid"),
            incomeSourceId = IncomeSourceId("some-income-source-id"),
            taxYear = TaxYear(2024),
            journey = JourneyName.ForeignPropertyExpenses,
            countryCode = Some(countryCode),
            status = InProgress,
            data = Json.toJsObject(ForeignPropertyExpensesStoreAnswers(
              isConsolidatedExpenses = isConsolidatedExpenses
            )),
            createdAt = Instant.now,
            updatedAt = Instant.now
          )))

      val service = new MergeService()
      val result = service.mergeForeignPropertyExpenses(Some(aPropertyPeriodicSubmission), repositoryAnswers)
      val expected = foreignExpensesAnswers
      result shouldBe Some(
        Map(
          countryCode -> expected
        )
      )
    }

    "return an empty map when foreign properties have no allowances" in {
      val foreignProperties = Seq(
        ForeignProperty(
          countryCode = "GRE",
          income = None,
          expenses = None
        )
      )

      val propertyPeriodicSubmission = PropertyPeriodicSubmission(
        submissionId = None,
        submittedOn = Some(LocalDateTime.now()),
        ukOtherProperty = None,
        foreignProperty = Some(foreignProperties),
        fromDate = LocalDate.now(),
        toDate = LocalDate.now()
      )

      val service = new MergeService()
      val result = service.mergeForeignPropertyExpenses(Some(propertyPeriodicSubmission), None)

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
        ukOtherProperty = None
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

  "mergeForeignPropertyStatuses" should {
    "return a map of foreign journey with status" in {
      val countryCode = "GRE"
      val journeyName = "foreign-property-adjustments"
      val journeyStatus = "inProgress"
      val repositoryAnswers = Map(
        countryCode -> JourneyAnswers(
          mtditid = Mtditid("some-mtditid"),
          incomeSourceId = IncomeSourceId("some-income-source-id"),
          taxYear = TaxYear(2024),
          journey = JourneyName.ForeignPropertyAdjustments,
          countryCode = Some(countryCode),
          status = InProgress,
          data = Json.toJsObject(JourneyWithStatus(
            journeyName = journeyName,
            journeyStatus = journeyStatus
          )),
          createdAt = Instant.now,
          updatedAt = Instant.now
        )
      )

      val service = new MergeService()
      val result = service.mergeForeignStatuses(Map(journeyName -> repositoryAnswers))
      val expected = List(JourneyWithStatus(
        journeyName = journeyName,
        journeyStatus = journeyStatus
      ))
      result shouldBe Some(Map(countryCode -> expected))
    }
  }

  "mergeForeignIncomeDividends" should {
    "return a map of foreign income dividends" in {
      val countryCode = "GRE"
      val amountBeforeTax = BigDecimal(12.34)
      val taxTakenOff = BigDecimal(34.56)
      val specialWithholdingTax = BigDecimal(56.78)
      val foreignTaxCreditRelief = true
      val taxableAmount = BigDecimal(90.12)
      val customerReference = "reference"
      val grossAmount = BigDecimal(78.90)
      val foreignTaxDeductedFromDividendIncome = true
      val foreignIncomeDividends = ForeignDividend(
        countryCode = countryCode,
        amountBeforeTax = Some(amountBeforeTax),
        taxTakenOff = Some(taxTakenOff),
        specialWithholdingTax = Some(specialWithholdingTax),
        foreignTaxCreditRelief = Some(foreignTaxCreditRelief),
        taxableAmount = taxableAmount
      )
      val grossAmountWithReference: GrossAmountWithReference = GrossAmountWithReference(Some(customerReference), grossAmount)

      val aForeignIncomeSubmission: ForeignIncomeSubmission = ForeignIncomeSubmission(
        foreignDividend = Some(Seq(foreignIncomeDividends)),
        dividendIncomeReceivedWhilstAbroad = Some(Seq(foreignIncomeDividends)),
        stockDividend = Some(grossAmountWithReference),
        redeemableShares = Some(grossAmountWithReference),
        bonusIssuesOfSecurities = Some(grossAmountWithReference),
        closeCompanyLoansWrittenOff = Some(grossAmountWithReference)
      )

      val foreignIncomeDividendsAnswers: ForeignIncomeDividendsAnswers =
        ForeignIncomeDividendsAnswers(
          countryCode = countryCode, foreignTaxDeductedFromDividendIncome = foreignTaxDeductedFromDividendIncome
        )

      val repositoryAnswers =
        Some(JourneyAnswers(
          mtditid = Mtditid("some-mtditid"),
          incomeSourceId = IncomeSourceId("some-income-source-id"),
          taxYear = TaxYear(2024),
          journey = JourneyName.ForeignPropertyAdjustments,
          countryCode = Some(countryCode),
          status = InProgress,
          data = Json.toJsObject(ForeignIncomeDividendsStoreAnswers(
            Seq(foreignIncomeDividendsAnswers)
          )),
          createdAt = Instant.now,
          updatedAt = Instant.now
        )
      )

      val service = new MergeService()
      val result = service.mergeForeignIncomeDividends(Some(aForeignIncomeSubmission), repositoryAnswers)
      val expected = ForeignDividendsAnswers(
        amountBeforeTax = Some(amountBeforeTax),
        taxTakenOff = Some(taxTakenOff),
        specialWithholdingTax = Some(specialWithholdingTax),
        foreignTaxCreditRelief = Some(foreignTaxCreditRelief),
        taxableAmount = Some(taxableAmount),
        foreignTaxDeductedFromDividendIncome = Some(foreignTaxDeductedFromDividendIncome)
      )
      result shouldBe Some(Map(countryCode -> expected))
    }
  }
}
