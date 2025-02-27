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

package models.responses

import models.LossType
import play.api.libs.json.{Format, Json, OFormat}

case class BroughtForwardLossResponse(
  businessId: String,
  typeOfLoss: LossType,
  lossAmount: BigDecimal,
  taxYearBroughtForwardFrom: String,
  lastModified: String
)

object BroughtForwardLossResponse {
  implicit val format: Format[BroughtForwardLossResponse] = Json.format[BroughtForwardLossResponse]
}

case class BroughtForwardLossResponseWithId(
  lossId: String,
  businessId: String,
  typeOfLoss: LossType,
  lossAmount: BigDecimal,
  taxYearBroughtForwardFrom: String,
  lastModified: String
)

object BroughtForwardLossResponseWithId {
  implicit val format: Format[BroughtForwardLossResponseWithId] = Json.format[BroughtForwardLossResponseWithId]
}

case class BroughtForwardLossId(lossId: String)

object BroughtForwardLossId {
  implicit val format: OFormat[BroughtForwardLossId] = Json.format[BroughtForwardLossId]
}

case class BroughtForwardLosses(losses: Seq[BroughtForwardLossResponseWithId])

object BroughtForwardLosses {
  implicit val format: OFormat[BroughtForwardLosses] = Json.format[BroughtForwardLosses]
}


