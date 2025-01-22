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

package models.request.foreign

import models.request.BalancingCharge
import models.request.foreign.adjustments.{ForeignUnusedResidentialFinanceCost, ForeignWhenYouReportedTheLoss, UnusedLossesPreviousYears}
import play.api.libs.json.{Format, Json}

case class ForeignAdjustmentsAnswers(
  privateUseAdjustment: Option[BigDecimal],
  balancingCharge: Option[BalancingCharge],
  residentialFinanceCost: Option[BigDecimal],
  unusedResidentialFinanceCost: Option[ForeignUnusedResidentialFinanceCost],
  propertyIncomeAllowanceClaim: Option[BigDecimal],
  unusedLossesPreviousYears: Option[UnusedLossesPreviousYears],
  whenYouReportedTheLoss: Option[ForeignWhenYouReportedTheLoss]
)

object ForeignAdjustmentsAnswers {
  implicit val format: Format[ForeignAdjustmentsAnswers] = Json.format[ForeignAdjustmentsAnswers]
}