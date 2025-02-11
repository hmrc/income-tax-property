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

package models.request.ukrentaroom

import models.Enumerable
import models.request.BalancingCharge
import models.request.foreign.WithName
import play.api.libs.json.{OFormat, Json}

final case class RaRAdjustments(
  balancingCharge: Option[BalancingCharge],
  unusedResidentialPropertyFinanceCostsBroughtFwd: Option[BigDecimal],
  unusedLossesBroughtForward: Option[RaRUnusedLossesBroughtForward],
  whenYouReportedTheLoss: Option[RarWhenYouReportedTheLoss]
)

object RaRAdjustments {
  implicit val format: OFormat[RaRAdjustments] = Json.format[RaRAdjustments]
}

final case class RaRUnusedLossesBroughtForward(raRUnusedLossesBroughtForwardYesOrNo: Boolean,
                                         raRUnusedLossesBroughtForwardAmount: Option[BigDecimal])

object RaRUnusedLossesBroughtForward {
  implicit val format: OFormat[RaRUnusedLossesBroughtForward] = Json.format[RaRUnusedLossesBroughtForward]
}

sealed trait RarWhenYouReportedTheLoss

object RarWhenYouReportedTheLoss extends Enumerable.Implicits {

  case object y2018to2019 extends WithName("y2018to2019") with RarWhenYouReportedTheLoss
  case object y2019to2020 extends WithName("y2019to2020") with RarWhenYouReportedTheLoss
  case object y2020to2021 extends WithName("y2020to2021") with RarWhenYouReportedTheLoss
  case object y2021to2022 extends WithName("y2021to2022") with RarWhenYouReportedTheLoss
  case object y2022to2023 extends WithName("y2022to2023") with RarWhenYouReportedTheLoss

  val values: Seq[RarWhenYouReportedTheLoss] = Seq(
    y2018to2019,
    y2019to2020,
    y2020to2021,
    y2021to2022,
    y2022to2023
  )

  implicit val enumerable: Enumerable[RarWhenYouReportedTheLoss] =
    Enumerable(values.map(v => v.toString -> v): _*)
}