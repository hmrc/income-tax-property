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

import models.request.{BalancingCharge, UnusedLossesBroughtForward, WhenYouReportedTheLoss}
import play.api.libs.json.{OFormat, Json}

final case class RaRAdjustments(
  balancingCharge: Option[BalancingCharge],
  unusedResidentialPropertyFinanceCostsBroughtFwd: Option[BigDecimal],
  unusedLossesBroughtForward: Option[UnusedLossesBroughtForward],
  whenYouReportedTheLoss: Option[WhenYouReportedTheLoss]
)

object RaRAdjustments {
  implicit val format: OFormat[RaRAdjustments] = Json.format[RaRAdjustments]
}
