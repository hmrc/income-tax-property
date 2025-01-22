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

import models.Enumerable
import models.request.BalancingCharge
import models.request.foreign.WithName
import play.api.libs.json.{Format, Json}


case class ForeignPropertyAdjustmentsWithCountryCode(
                                                      countryCode: String,
                                                      privateUseAdjustment: BigDecimal,
                                                      balancingCharge: BalancingCharge,
                                                      residentialFinanceCost: Option[BigDecimal],
                                                      unusedResidentialFinanceCost: Option[ForeignUnusedResidentialFinanceCost],
                                                      propertyIncomeAllowanceClaim: Option[BigDecimal],
                                                      unusedLossesPreviousYears: UnusedLossesPreviousYears,
                                                      whenYouReportedTheLoss: Option[ForeignWhenYouReportedTheLoss]
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

sealed trait ForeignWhenYouReportedTheLoss

object ForeignWhenYouReportedTheLoss extends Enumerable.Implicits {

  case object y2018to2019 extends WithName("y2018to2019") with ForeignWhenYouReportedTheLoss
  case object y2019to2020 extends WithName("y2019to2020") with ForeignWhenYouReportedTheLoss
  case object y2020to2021 extends WithName("y2020to2021") with ForeignWhenYouReportedTheLoss
  case object y2021to2022 extends WithName("y2021to2022") with ForeignWhenYouReportedTheLoss
  case object y2022to2023 extends WithName("y2022to2023") with ForeignWhenYouReportedTheLoss

  val values: Seq[ForeignWhenYouReportedTheLoss] = Seq(
    y2018to2019, y2019to2020, y2020to2021, y2021to2022, y2022to2023
  )

  implicit val enumerable: Enumerable[ForeignWhenYouReportedTheLoss] =
    Enumerable(values.map(v => v.toString -> v): _*)
}