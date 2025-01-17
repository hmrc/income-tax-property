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

package models.request.foreign.adjustments

import models.request.BalancingCharge
import play.api.libs.json.{Format, Json}


case class ForeignPropertyAdjustmentsWithCountryCode(
                                                      countryCode: String,
                                                      privateUseAdjustment: BigDecimal,
                                                      balancingCharge: BalancingCharge,
                                                      residentialFinanceCost: BigDecimal,
                                                      unusedResidentialFinanceCost: ForeignUnusedResidentialFinanceCost,
                                                      unusedLossesPreviousYears: UnusedLossesPreviousYears
                                                    )


object ForeignPropertyAdjustmentsWithCountryCode {
  implicit val format: Format[ForeignPropertyAdjustmentsWithCountryCode] = Json.format[ForeignPropertyAdjustmentsWithCountryCode]
}


final case class ForeignUnusedResidentialFinanceCost (
                                                 foreignUnusedResidentialFinanceCostYesNo: Boolean,
                                                 foreignUnusedResidentialFinanceCostAmount: Option[BigDecimal]
                                               )
object ForeignUnusedResidentialFinanceCost {
  implicit val format: Format[ForeignUnusedResidentialFinanceCost] = Json.format[ForeignUnusedResidentialFinanceCost]
}


final case class UnusedLossesPreviousYears(
                                            unusedLossesPreviousYearsYesNo: Boolean,
                                            unusedLossesPreviousYearsAmount: Option[BigDecimal]
                                          )

object UnusedLossesPreviousYears {
  implicit val format: Format[UnusedLossesPreviousYears] = Json.format
}