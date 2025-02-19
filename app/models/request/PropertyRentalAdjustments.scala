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

import models.Enumerable
import models.request.foreign.WithName
import play.api.libs.json.{OFormat, Json}

final case class PropertyRentalAdjustments(
  privateUseAdjustment: BigDecimal,
  balancingCharge: BalancingCharge,
  propertyIncomeAllowance: Option[BigDecimal],
  renovationAllowanceBalancingCharge: RenovationAllowanceBalancingCharge,
  residentialFinanceCost: BigDecimal,
  unusedResidentialFinanceCost: Option[BigDecimal]
)

object PropertyRentalAdjustments {
  implicit val format: OFormat[PropertyRentalAdjustments] = Json.format[PropertyRentalAdjustments]
}

final case class BalancingCharge(balancingChargeYesNo: Boolean, balancingChargeAmount: Option[BigDecimal])

object BalancingCharge {
  implicit val format: OFormat[BalancingCharge] = Json.format
}

final case class RenovationAllowanceBalancingCharge(
  renovationAllowanceBalancingChargeYesNo: Boolean,
  renovationAllowanceBalancingChargeAmount: Option[BigDecimal]
)

object RenovationAllowanceBalancingCharge {
  implicit val format: OFormat[RenovationAllowanceBalancingCharge] = Json.format
}

final case class UnusedLossesBroughtForward(
                                             unusedLossesBroughtForwardYesOrNo: Boolean,
                                             unusedLossesBroughtForwardAmount: Option[BigDecimal]
                                           )

object UnusedLossesBroughtForward {
  implicit val format: OFormat[UnusedLossesBroughtForward] = Json.format
}

sealed trait WhenYouReportedTheLoss

object WhenYouReportedTheLoss extends Enumerable.Implicits {

  case object y2018to2019 extends WithName("y2018to2019") with WhenYouReportedTheLoss
  case object y2019to2020 extends WithName("y2019to2020") with WhenYouReportedTheLoss
  case object y2020to2021 extends WithName("y2020to2021") with WhenYouReportedTheLoss
  case object y2021to2022 extends WithName("y2021to2022") with WhenYouReportedTheLoss
  case object y2022to2023 extends WithName("y2022to2023") with WhenYouReportedTheLoss

  val values: Seq[WhenYouReportedTheLoss] = Seq(
    y2018to2019, y2019to2020, y2020to2021, y2021to2022, y2022to2023
  )

  implicit val enumerable: Enumerable[WhenYouReportedTheLoss] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
