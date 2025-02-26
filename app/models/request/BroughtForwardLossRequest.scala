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

package models.request

import models.LossType
import play.api.libs.json.{Format, Json}

case class BroughtForwardLossRequest(
  taxYearBroughtForwardFrom: String,
  typeOfLoss: LossType,
  businessId: String,
  lossAmount: BigDecimal
)

object BroughtForwardLossRequest {
  implicit val format: Format[BroughtForwardLossRequest] = Json.format[BroughtForwardLossRequest]
}

case class BroughtForwardLossAmount(lossAmount: BigDecimal)

object BroughtForwardLossAmount {
  implicit val format: Format[BroughtForwardLossAmount] = Json.format[BroughtForwardLossAmount]
}
