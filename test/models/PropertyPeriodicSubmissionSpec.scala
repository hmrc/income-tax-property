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

import models.errors.ServiceError
import models.request.Expenses
import models.responses._
import utils.UnitTest

import java.time.LocalDate
import cats.syntax.either._

class PropertyPeriodicSubmissionSpec extends UnitTest {
  val expenses = Expenses(
    rentsRatesAndInsurance = Some(100),
    repairsAndMaintenanceCosts = Some(200),
    loanInterest = Some(300),
    otherProfessionalFee = Some(400),
    costsOfServicesProvided = Some(500),
    propertyBusinessTravelCost = Some(600),
    otherAllowablePropertyExpenses = Some(700)
  )

  val date = LocalDate.now()
  val ukOtherPropertyIncome = UkOtherPropertyIncome(None, None, None, None, Some(BigDecimal(100.0)), None)
  val propertyPeriodicSubmission = PropertyPeriodicSubmission(None, None, date, date, None, None, None, Some(UkOtherProperty(
    ukOtherPropertyIncome,
    UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None)
  )))
  val propertyPeriodicSubmissionRequest = PropertyPeriodicSubmissionRequest(
    None,
    None,
    None,
    None,
    Some(
      UkOtherProperty(
        ukOtherPropertyIncome,
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

  "PropertyPeriodicSubmission" should {
    "be generated from expenses" in {

      PropertyPeriodicSubmissionRequest.fromExpenses(Some(propertyPeriodicSubmission), expenses) shouldBe propertyPeriodicSubmissionRequest.asRight[ServiceError]
    }
  }
}
