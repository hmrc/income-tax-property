/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.incometaxproperty.models.responses

import java.time.{LocalDate, LocalDateTime}

case class PropertyPeriodicSubmission(submittedOn: Option[LocalDateTime],
                                      fromTime: LocalDate,
                                      toTime: LocalDate,
                                      foreignFhlEea: Option[ForeignFhlEea],
                                      foreignProperty: Option[Seq[ForeignProperty]],
                                      ukFhlProperty: Option[UkFhlProperty],
                                      ukOtherProperty: Option[UkOtherProperty])

case class ForeignFhlEea(income: ForeignFhlIncome,
                         expenses: ForeignFhlExpenses)


case class ForeignFhlIncome(rentAmount: BigDecimal)

case class ForeignFhlExpenses(premisesRunningCosts: Option[BigDecimal],
                              repairsAndMaintenance: Option[BigDecimal],
                              financialCosts: Option[BigDecimal],
                              professionalFees: Option[BigDecimal],
                              costOfServices: Option[BigDecimal],
                              travelCosts: Option[BigDecimal],
                              other: Option[BigDecimal],
                              consolidatedExpense: Option[BigDecimal])

case class ForeignProperty(countryCode: String,
                           income: Option[ForeignPropertyIncome],
                           expenses: Option[ForeignPropertyExpenses])

case class ForeignPropertyIncome(rentIncome: Option[ForeignPropertyRentIncome],
                                 foreignTaxCreditRelief: Option[Boolean],
                                 premiumsOfLeaseGrant: Option[BigDecimal],
                                 otherPropertyIncome: Option[BigDecimal],
                                 foreignTaxPaidOrDeducted: Option[BigDecimal],
                                 specialWithholdingTaxOrUkTaxPaid: Option[BigDecimal])

case class ForeignPropertyExpenses(premisesRunningCosts: Option[BigDecimal],
                                   repairsAndMaintenance: Option[BigDecimal],
                                   financialCosts: Option[BigDecimal],
                                   professionalFees: Option[BigDecimal],
                                   travelCosts: Option[BigDecimal],
                                   costOfServices: Option[BigDecimal],
                                   residentialFinancialCost: Option[BigDecimal],
                                   broughtFwdResidentialFinancialCost: Option[BigDecimal],
                                   other: Option[BigDecimal],
                                   consolidatedExpense: Option[BigDecimal])

case class ForeignPropertyRentIncome(rentAmount: BigDecimal)

case class UkFhlProperty(income: UkFhlIncome,
                         expenses: UkPropertyExpenses)

case class UkFhlIncome(periodAmount: Option[BigDecimal],
                       taxDeducted: Option[BigDecimal],
                       ukFhlRentARoom: Option[RentARoomIncome])

case class RentARoomIncome(rentsReceived: BigDecimal)

case class UkPropertyExpenses(premisesRunningCosts: Option[BigDecimal],
                              repairsAndMaintenance: Option[BigDecimal],
                              financialCosts: Option[BigDecimal],
                              professionalFees: Option[BigDecimal],
                              travelCosts: Option[BigDecimal],
                              costOfServices: Option[BigDecimal],
                              other: Option[BigDecimal],
                              ukFhlRentARoom: Option[UkRentARoomExpense])

case class UkRentARoomExpense(amountClaimed: BigDecimal)

case class UkOtherProperty(income: UkOtherPropertyIncome,
                           expenses: UkOtherPropertyExpenses)

case class UkOtherPropertyIncome(premiumsOfLeaseGrant: Option[BigDecimal],
                                 reversePremiums: Option[BigDecimal],
                                 periodAmount: Option[BigDecimal],
                                 taxDeducted: Option[BigDecimal],
                                 otherIncome: Option[BigDecimal],
                                 ukOtherRentARoom: Option[RentARoomIncome])

case class UkOtherPropertyExpenses(premisesRunningCosts: Option[BigDecimal],
                                   repairsAndMaintenance: Option[BigDecimal],
                                   financialCosts: Option[BigDecimal],
                                   professionalFees: Option[BigDecimal],
                                   travelCosts: Option[BigDecimal],
                                   costOfServices: Option[BigDecimal],
                                   other: Option[BigDecimal],
                                   residentialFinancialCost: Option[BigDecimal],
                                   residentialFinancialCostsCarriedForward: Option[BigDecimal],
                                   ukOtherRentARoom: Option[UkRentARoomExpense],
                                   consolidatedExpense: Option[BigDecimal])
