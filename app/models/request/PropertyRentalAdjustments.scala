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

package models.request

import play.api.libs.json.{Json, OFormat}

final case class PropertyRentalAdjustments(
                                            privateUseAdjustment: BigDecimal,
                                            balancingCharge: BalancingCharge,
                                            propertyIncomeAllowance: BigDecimal, //Where is this used?
                                            renovationAllowanceBalancingCharge: RenovationAllowanceBalancingCharge,
                                            residentialFinancialCost: BigDecimal,
                                            unusedResidentialFinanceCost: BigDecimal)

object PropertyRentalAdjustments {
  implicit val format: OFormat[PropertyRentalAdjustments] = Json.format[PropertyRentalAdjustments]
}

final case class BalancingCharge(balancingChargeYesNo: Boolean, balancingChargeAmount: Option[BigDecimal])

object BalancingCharge {
  implicit val format: OFormat[BalancingCharge] = Json.format
}

final case class RenovationAllowanceBalancingCharge(renovationAllowanceBalancingChargeYesNo: Boolean,
                                                    renovationAllowanceBalancingChargeAmount: Option[BigDecimal])

object RenovationAllowanceBalancingCharge {
  implicit val format: OFormat[RenovationAllowanceBalancingCharge] = Json.format
}


