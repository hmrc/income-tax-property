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

package models.request.foreign

import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.responses.ForeignPropertyAllowances
import play.api.libs.json._
import play.api.libs.ws.BodyWritable

import java.time.LocalDateTime

case class ForeignPropertyAdjustments(privateUseAdjustment: Option[BigDecimal], balancingCharge: Option[BigDecimal])

object ForeignPropertyAdjustments {
  implicit val format: OFormat[ForeignPropertyAdjustments] = Json.format[ForeignPropertyAdjustments]
}

case class AnnualForeignProperty(
  countryCode: String,
  adjustments: Option[ForeignPropertyAdjustments],
  allowances: Option[ForeignPropertyAllowances]
)

object AnnualForeignProperty {
  implicit val format: OFormat[AnnualForeignProperty] = Json.format[AnnualForeignProperty]
}

case class ForeignPropertyAnnualSubmission(
  submittedOn: Option[LocalDateTime],
  foreignProperty: Option[Seq[AnnualForeignProperty]]
)

object ForeignPropertyAnnualSubmission {
  implicit val format: OFormat[ForeignPropertyAnnualSubmission] = Json.format[ForeignPropertyAnnualSubmission]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  def fromForeignPropertyAllowances(
    foreignPropertyAnnualSubmissionFromDownstream: ForeignPropertyAnnualSubmission,
    foreignPropertyAllowancesWithCountryCode: ForeignPropertyAllowancesWithCountryCode
  ): ForeignPropertyAnnualSubmission =
    ForeignPropertyAnnualSubmission(None, None)

}
