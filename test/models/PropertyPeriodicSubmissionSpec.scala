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
import models.errors.ServiceError
import models.request._
import models.responses._
import utils.UnitTest

import java.time.LocalDate

class PropertyPeriodicSubmissionSpec extends UnitTest {
  val expenses: Expenses = Expenses(
    consolidatedExpenses = None,
    rentsRatesAndInsurance = Some(100),
    repairsAndMaintenanceCosts = Some(200),
    loanInterest = Some(300),
    otherProfessionalFee = Some(400),
    costsOfServicesProvided = Some(500),
    propertyBusinessTravelCost = Some(600),
    otherAllowablePropertyExpenses = Some(700),
    legalManagementOtherFee = None,
    residentialPropertyFinanceCosts = None,
    unusedResidentialPropertyFinanceCostsBroughtFwd = None,
    otherPropertyExpenses = None
  )

  val saveIncome = SaveIncome(
    UkOtherPropertyIncome(Some(405), None, None, Some(51), None, None),
    Income(true, 55, true, ReversePremiumsReceived(false), None, None, None, None, None)
  )
  val date = LocalDate.now()
  val ukOtherPropertyIncome = UkOtherPropertyIncome(None, None, None, None, Some(BigDecimal(100.0)), None)
  val propertyPeriodicSubmission = PropertyPeriodicSubmission(
    None,
    None,
    date,
    date,
    None,
    None,
    None,
    Some(
      UkOtherProperty(
        Some(ukOtherPropertyIncome),
        Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
      )
    )
  )
  val createPropertyPeriodicSubmissionRequest: CreatePropertyPeriodicSubmissionRequest =
    CreatePropertyPeriodicSubmissionRequest(
      LocalDate.now(),
      LocalDate.now(),
      None,
      None,
      None,
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
              consolidatedExpense = None
            )
          )
        )
      )
    )

  val updatePropertyPeriodicSubmissionRequest: UpdatePropertyPeriodicSubmissionRequest =
    UpdatePropertyPeriodicSubmissionRequest(
      None,
      None,
      None,
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
              consolidatedExpense = None
            )
          )
        )
      )
    )

  def propertyPeriodicSubmissionRequest(
    ukOtherPropertyIncomeMaybe: Option[UkOtherPropertyIncome],
    ukOtherPropertyExpensesMaybe: Option[UkOtherPropertyExpenses]
  ): CreatePropertyPeriodicSubmissionRequest = CreatePropertyPeriodicSubmissionRequest(
    LocalDate.now(),
    LocalDate.now(),
    None,
    None,
    None,
    Some(
      UkOtherProperty(
        ukOtherPropertyIncomeMaybe,
        ukOtherPropertyExpensesMaybe
      )
    )
  )

  "PropertyPeriodicSubmission" should {
    "be generated from expenses" in {

      CreatePropertyPeriodicSubmissionRequest.fromExpenses(
        Some(propertyPeriodicSubmission),
        expenses
      ) shouldBe createPropertyPeriodicSubmissionRequest.asRight[ServiceError]
    }

    "be generated from income" in {

      CreatePropertyPeriodicSubmissionRequest.fromUkOtherPropertyIncome(
        Some(propertyPeriodicSubmission),
        saveIncome
      ) shouldBe
        propertyPeriodicSubmissionRequest(
          Some(saveIncome.ukOtherPropertyIncome),
          propertyPeriodicSubmission.ukOtherProperty.flatMap(_.expenses)
        ).asRight[ServiceError]
    }

    "be generated from uk rent a room" in {
      val ukRaRAbout = RaRAbout(
        true,
        1.23,
        ClaimExpensesOrRRR(true, Some(4.56))
      )
      CreatePropertyPeriodicSubmissionRequest.fromUkRaRAbout(Some(propertyPeriodicSubmission), ukRaRAbout) shouldBe
        propertyPeriodicSubmissionRequest(
          propertyPeriodicSubmission.ukOtherProperty.flatMap(
            _.income.map(_.copy(ukOtherRentARoom = Some(RentARoomIncome(ukRaRAbout.totalIncomeAmount))))
          ),
          propertyPeriodicSubmission.ukOtherProperty.flatMap(
            _.expenses.map(
              _.copy(ukOtherRentARoom = ukRaRAbout.claimExpensesOrRRR.rentARoomAmount.map(UkRentARoomExpense(_)))
            )
          )
        ).asRight[ServiceError]
    }
  }
}
