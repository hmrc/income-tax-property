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

import cats.syntax.either._
import models.common.TaxYear
import models.errors.ServiceError
import models.request._
import models.responses._
import utils.CaseClassLevelDifferenceUtil.diff
import utils.TaxYearUtils.taxYear
import utils.UnitTest

import java.time.{LocalDate, LocalDateTime}

class PropertyPeriodicSubmissionSpec extends UnitTest {
  val expenses: Expenses = Expenses(
    consolidatedExpenses = None,
    rentsRatesAndInsurance = Some(100),
    repairsAndMaintenanceCosts = Some(200),
    loanInterestOrOtherFinancialCost = Some(300),
    otherProfessionalFees = Some(400),
    costsOfServicesProvided = Some(500),
    propertyBusinessTravelCosts = Some(600),
    otherAllowablePropertyExpenses = Some(700)
  )

  val propertyRentalsIncome = PropertyRentalsIncome(
    true,
    4321.12,
    987.65,
    Some(DeductingTax(true, Some(86.42))),
    Some(CalculatedFigureYourself(true, Some(35.75))),
    Some(123.65),
    Some(987.46),
    Some(PremiumsGrantLease(true, Some(98.56))),
    Some(ReversePremiumsReceived(true, Some(28.71)))
  )
  val date = LocalDate.now()

  val ukOtherPropertyIncome = UkOtherPropertyIncome(None, None, None, None, Some(BigDecimal(100.0)), None)
  val propertyPeriodicSubmission = PropertyPeriodicSubmission(
    None,
    None,
    date,
    date,
    None,
    Some(
      UkOtherProperty(
        Some(ukOtherPropertyIncome),
        Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
      )
    )
  )
  val createUKPropertyPeriodicSubmissionRequest: CreateUKPropertyPeriodicSubmissionRequest =
    CreateUKPropertyPeriodicSubmissionRequest(
      LocalDate.now(),
      LocalDate.now(),
      Some(
        UkOtherProperty(
          Some(ukOtherPropertyIncome),
          Some(
            UkOtherPropertyExpenses(
              premisesRunningCosts = Some(100),
              repairsAndMaintenance = Some(200),
              financialCosts = Some(300),
              professionalFees = Some(400),
              costOfServices = Some(500),
              travelCosts = Some(600),
              other = Some(700),
              residentialFinancialCost = None,
              residentialFinancialCostsCarriedForward = None,
              ukOtherRentARoom = None,
              consolidatedExpenses = None
            )
          )
        )
      )
    )

  val updateUKPropertyPeriodicSubmissionRequest: UpdateUKPropertyPeriodicSubmissionRequest =
    UpdateUKPropertyPeriodicSubmissionRequest(
      Some(
        UkOtherProperty(
          Some(ukOtherPropertyIncome),
          Some(
            UkOtherPropertyExpenses(
              premisesRunningCosts = Some(100),
              repairsAndMaintenance = Some(200),
              financialCosts = Some(300),
              professionalFees = Some(400),
              costOfServices = Some(500),
              travelCosts = Some(600),
              other = Some(700),
              residentialFinancialCost = None,
              residentialFinancialCostsCarriedForward = None,
              ukOtherRentARoom = None,
              consolidatedExpenses = None
            )
          )
        )
      )
    )
  val propertyPeriodicSubmissionWithAllFieldsExisting = PropertyPeriodicSubmission(
    submissionId = Some(PeriodicSubmissionId("periodic-submission-id")),
    submittedOn = Some(LocalDateTime.now()),
    fromDate = date,
    toDate = date,
    foreignProperty = Some(
      Seq(
        ForeignProperty(
          "UK",
          Some(
            ForeignPropertyIncome(
              Some(ForeignPropertyRentIncome(14.70)),
              Some(true),
              Some(36.92),
              Some(58.03),
              Some(69.25),
              Some(81.47)
            )
          ),
          Some(
            ForeignPropertyExpenses(
              Some(98.76),
              Some(54.32),
              Some(10.98),
              Some(76.54),
              Some(32.10),
              Some(97.53),
              Some(19.75),
              Some(31.97),
              Some(53.19),
              Some(75.31),
              None
            )
          )
        )
      )
    ),
    ukOtherProperty = Some(
      UkOtherProperty(
        Some(
          UkOtherPropertyIncome(
            Some(93.39),
            Some(82.26),
            Some(71.16),
            Some(60.06),
            Some(59.95),
            Some(RentARoomIncome(48.84))
          )
        ),
        Some(
          UkOtherPropertyExpenses(
            Some(12.21),
            Some(23.32),
            Some(34.43),
            Some(45.54),
            Some(56.65),
            Some(67.76),
            Some(78.87),
            Some(89.98),
            Some(90.09),
            Some(
              UkRentARoomExpense(13.31)
            ),
            Some(92.29)
          )
        )
      )
    )
  )

  private def generateUpdateRequestWithSameValues(
    propertyPeriodicSubmission: PropertyPeriodicSubmission
  ) =
    UpdateUKPropertyPeriodicSubmissionRequest(
      propertyPeriodicSubmission.ukOtherProperty
    )

  def propertyPeriodicSubmissionRequest(
    ukOtherPropertyIncomeMaybe: Option[UkOtherPropertyIncome],
    ukOtherPropertyExpensesMaybe: Option[UkOtherPropertyExpenses]
  ): CreateUKPropertyPeriodicSubmissionRequest = CreateUKPropertyPeriodicSubmissionRequest(
    LocalDate.now(),
    LocalDate.now(),
    Some(
      UkOtherProperty(
        ukOtherPropertyIncomeMaybe,
        ukOtherPropertyExpensesMaybe
      )
    )
  )

  "PropertyPeriodicSubmission" should {
    "be generated from expenses" in {

      CreateUKPropertyPeriodicSubmissionRequest.fromExpenses(
        TaxYear(taxYear),
        Some(propertyPeriodicSubmission),
        expenses
      ) shouldBe createUKPropertyPeriodicSubmissionRequest.asRight[ServiceError]
    }

    "be generated from uk rent a room" in {
      val ukRaRAbout = RaRAbout(
        true,
        1.23,
        ClaimExpensesOrRelief(true, Some(4.56))
      )
      CreateUKPropertyPeriodicSubmissionRequest.fromUkRaRAbout(
        TaxYear(taxYear),
        Some(propertyPeriodicSubmission),
        ukRaRAbout
      ) shouldBe
        propertyPeriodicSubmissionRequest(
          propertyPeriodicSubmission.ukOtherProperty.flatMap(
            _.income.map(_.copy(ukOtherRentARoom = Some(RentARoomIncome(ukRaRAbout.totalIncomeAmount))))
          ),
          propertyPeriodicSubmission.ukOtherProperty.flatMap(
            _.expenses.map(
              _.copy(ukOtherRentARoom = ukRaRAbout.claimExpensesOrRelief.rentARoomAmount.map(UkRentARoomExpense(_)))
            )
          )
        ).asRight[ServiceError]
    }
    "convert expenses having all None values to None for create" in {
      val fakeDate = LocalDate.now()

      CreateUKPropertyPeriodicSubmissionRequest
        .fromEntity(
          TaxYear(2024),
          Some(PropertyPeriodicSubmission(None, None, fakeDate, fakeDate, None, None)),
          RentalsAndRaRAbout(
            false,
            12.34,
            false,
            22.33,
            ClaimExpensesOrRelief(false, None)
          )
        ) shouldBe
        Right(
          CreateUKPropertyPeriodicSubmissionRequest(
            fakeDate,
            fakeDate,
            Some(
              UkOtherProperty(
                Some(UkOtherPropertyIncome(None, None, Some(22.33), None, None, Some(RentARoomIncome(12.34)))),
                None
              )
            )
          )
        )
    }
    "convert income having all None values to None for create" in {
      val fakeDate = LocalDate.now()

      CreateUKPropertyPeriodicSubmissionRequest
        .fromEntity(
          TaxYear(2024),
          None,
          RaRAbout(
            false,
            12.34,
            ClaimExpensesOrRelief(
              false,
              None
            )
          )
        )
        .map(_.copy(fromDate = fakeDate, toDate = fakeDate)) shouldBe Right(
        CreateUKPropertyPeriodicSubmissionRequest(
          fakeDate,
          fakeDate,
          Some(
            UkOtherProperty(
              Some(UkOtherPropertyIncome(None, None, None, None, None, Some(RentARoomIncome(12.34)))),
              None
            )
          )
        )
      )

    }
    "convert expenses having all None values to None for update" in {
      val fakeDate = LocalDate.now()

      UpdateUKPropertyPeriodicSubmissionRequest
        .fromEntity(
          Some(PropertyPeriodicSubmission(None, None, fakeDate, fakeDate, None, None)),
          RentalsAndRaRAbout(
            false,
            12.34,
            false,
            22.33,
            ClaimExpensesOrRelief(false, None)
          )
        ) shouldBe
        Right(
          UpdateUKPropertyPeriodicSubmissionRequest(
            Some(
              UkOtherProperty(
                Some(UkOtherPropertyIncome(None, None, Some(22.33), None, None, Some(RentARoomIncome(12.34)))),
                None
              )
            )
          )
        )
    }
    "convert income having all None values to None for update" in {

      UpdateUKPropertyPeriodicSubmissionRequest
        .fromEntity(
          None,
          RaRAbout(
            isJointlyLet = false,
            12.34,
            ClaimExpensesOrRelief(
              isClaimExpensesOrRelief = false,
              None
            )
          )
        ) shouldBe Right(
        UpdateUKPropertyPeriodicSubmissionRequest(
          Some(
            UkOtherProperty(
              Some(UkOtherPropertyIncome(None, None, None, None, None, Some(RentARoomIncome(12.34)))),
              None
            )
          )
        )
      )

    }
    "be generated from rar about and not override existing other fields" in {
      val updateRequestWithOriginalSubmissionValues =
        generateUpdateRequestWithSameValues(propertyPeriodicSubmissionWithAllFieldsExisting)
      val Right(propertyPeriodicSubmissionWithNewRaRAbout) = UpdateUKPropertyPeriodicSubmissionRequest
        .fromUkRaRAbout(
          Some(propertyPeriodicSubmissionWithAllFieldsExisting),
          RaRAbout(
            false,
            12.34,
            ClaimExpensesOrRelief(true, Some(56.78))
          )
        )

      val firstLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRaRAbout,
        updateRequestWithOriginalSubmissionValues
      )

      val secondLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRaRAbout.ukOtherProperty.get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty.get
      )

      val thirdLevelDiffOnIncome = diff(
        propertyPeriodicSubmissionWithNewRaRAbout.ukOtherProperty
          .flatMap(_.income)
          .get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty
          .flatMap(_.income)
          .get
      )

      val thirdLevelDiffOnExpense = diff(
        propertyPeriodicSubmissionWithNewRaRAbout.ukOtherProperty
          .flatMap(_.expenses)
          .get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty
          .flatMap(_.expenses)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("income", "expenses")
      thirdLevelDiffOnIncome should be(
        List("ukOtherRentARoom")
      )

      thirdLevelDiffOnExpense should be(
        List("ukOtherRentARoom")
      )
    }

    "be generated from rentals and rar about and not override existing other fields" in {
      val updateRequestWithOriginalSubmissionValues =
        generateUpdateRequestWithSameValues(propertyPeriodicSubmissionWithAllFieldsExisting)
      val Right(propertyPeriodicSubmissionWithNewRaRAbout) = UpdateUKPropertyPeriodicSubmissionRequest
        .fromRentalsAndRaRAbout(
          Some(propertyPeriodicSubmissionWithAllFieldsExisting),
          RentalsAndRaRAbout(
            false,
            12.34,
            true,
            22.33,
            ClaimExpensesOrRelief(true, Some(56.78))
          )
        )

      val firstLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRaRAbout,
        updateRequestWithOriginalSubmissionValues
      )

      val secondLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRaRAbout.ukOtherProperty.get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty.get
      )

      val thirdLevelDiffOnIncome = diff(
        propertyPeriodicSubmissionWithNewRaRAbout.ukOtherProperty
          .flatMap(_.income)
          .get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty
          .flatMap(_.income)
          .get
      )

      val thirdLevelDiffOnExpense = diff(
        propertyPeriodicSubmissionWithNewRaRAbout.ukOtherProperty
          .flatMap(_.expenses)
          .get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty
          .flatMap(_.expenses)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("income", "expenses")
      thirdLevelDiffOnIncome should be(
        List("ukOtherRentARoom")
      )

      thirdLevelDiffOnExpense should be(
        List("ukOtherRentARoom")
      )
    }

    "be generated from rentals expenses and not override existing other fields" in {
      val updateRequestWithOriginalSubmissionValues =
        generateUpdateRequestWithSameValues(propertyPeriodicSubmissionWithAllFieldsExisting)
      val Right(propertyPeriodicSubmissionWithNewRentARoomExpenses) = UpdateUKPropertyPeriodicSubmissionRequest
        .fromExpenses(
          Some(propertyPeriodicSubmissionWithAllFieldsExisting),
          Expenses(
            consolidatedExpenses = Some(
              ConsolidatedExpenses(
                true,
                Some(113.57)
              )
            ),
            rentsRatesAndInsurance = Some(191.35),
            repairsAndMaintenanceCosts = Some(179.13),
            loanInterestOrOtherFinancialCost = Some(158.14),
            otherProfessionalFees = Some(170.36),
            costsOfServicesProvided = Some(193.71),
            propertyBusinessTravelCosts = Some(159.37),
            otherAllowablePropertyExpenses = Some(115.93)
          )
        )

      val firstLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRentARoomExpenses,
        updateRequestWithOriginalSubmissionValues
      )

      val secondLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRentARoomExpenses.ukOtherProperty.get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty.get
      )

      val thirdLevelDiffOnExpense = diff(
        propertyPeriodicSubmissionWithNewRentARoomExpenses.ukOtherProperty
          .flatMap(_.expenses)
          .get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty
          .flatMap(_.expenses)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("expenses")

      thirdLevelDiffOnExpense should be(
        List(
          "premisesRunningCosts",
          "repairsAndMaintenance",
          "financialCosts",
          "professionalFees",
          "travelCosts",
          "costOfServices",
          "other"
        )
      )
    }

    "be generated from rar expenses and not override existing other fields" in {
      val updateRequestWithOriginalSubmissionValues =
        generateUpdateRequestWithSameValues(propertyPeriodicSubmissionWithAllFieldsExisting)
      val Right(propertyPeriodicSubmissionWithNewRentARoomExpenses) = UpdateUKPropertyPeriodicSubmissionRequest
        .fromRaRExpenses(
          Some(propertyPeriodicSubmissionWithAllFieldsExisting),
          RentARoomExpenses(
            Some(
              ConsolidatedExpenses(
                true,
                Some(13.57)
              )
            ),
            Some(91.35),
            Some(79.13),
            Some(58.14),
            Some(70.36),
            Some(93.71)
          )
        )

      val firstLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRentARoomExpenses,
        updateRequestWithOriginalSubmissionValues
      )

      val secondLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRentARoomExpenses.ukOtherProperty.get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty.get
      )

      val thirdLevelDiffOnExpense = diff(
        propertyPeriodicSubmissionWithNewRentARoomExpenses.ukOtherProperty
          .flatMap(_.expenses)
          .get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty
          .flatMap(_.expenses)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("expenses")

      thirdLevelDiffOnExpense should be(
        List(
          "premisesRunningCosts",
          "repairsAndMaintenance",
          "professionalFees",
          "costOfServices",
          "other",
          "consolidatedExpenses"
        )
      )
    }

    "be generated from rentals income and not override existing other fields" in {
      val updateRequestWithOriginalSubmissionValues =
        generateUpdateRequestWithSameValues(propertyPeriodicSubmissionWithAllFieldsExisting)
      val Right(propertyPeriodicSubmissionWithNewRentalsIncome) = UpdateUKPropertyPeriodicSubmissionRequest
        .fromPropertyRentalsIncome(
          Some(propertyPeriodicSubmissionWithAllFieldsExisting),
          PropertyRentalsIncome(
            true,
            12.34,
            56.78,
            Some(DeductingTax(true, Some(22.44))),
            Some(CalculatedFigureYourself(true, Some(48.28))),
            Some(12.21),
            Some(15.51),
            Some(PremiumsGrantLease(true, Some(35.53))),
            Some(ReversePremiumsReceived(true, Some(64.46)))
          )
        )

      val firstLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRentalsIncome,
        updateRequestWithOriginalSubmissionValues
      )

      val secondLevelDiff = diff(
        propertyPeriodicSubmissionWithNewRentalsIncome.ukOtherProperty.get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty.get
      )

      val thirdLevelDiffOnExpense = diff(
        propertyPeriodicSubmissionWithNewRentalsIncome.ukOtherProperty
          .flatMap(_.income)
          .get,
        updateRequestWithOriginalSubmissionValues.ukOtherProperty
          .flatMap(_.income)
          .get
      )

      firstLevelDiff shouldBe List("ukOtherProperty")
      secondLevelDiff shouldBe List("income")

      thirdLevelDiffOnExpense should be(
        List("premiumsOfLeaseGrant", "reversePremiums", "periodAmount", "taxDeducted", "otherIncome")
      )
    }

  }
}
