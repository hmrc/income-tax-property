/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.incometaxproperty.models.responses

import IncomeSourceDetailsModel.TaxPayerDisplayResponse
import play.api.libs.json.{Format, JsValue, Json, OFormat}
import java.time.LocalDateTime

sealed trait IncomeSourceDetailsResponse {
  def toJson: JsValue
}

case class IncomeSourceDetailsModel(processingDate: LocalDateTime,
                                    taxPayerDisplayResponse: TaxPayerDisplayResponse
                                   ) extends IncomeSourceDetailsResponse {

  override def toJson: JsValue = Json.toJson(this)
}

object IncomeSourceDetailsModel {
  implicit val format: Format[IncomeSourceDetailsModel] = Json.format[IncomeSourceDetailsModel]

  case class TaxPayerDisplayResponse(safeId: String,
                                     nino: String,
                                     mtdId: String,
                                     yearOfMigration: Option[String],
                                     propertyIncome: Boolean,
                                     businessData: Option[Seq[BusinessDetailsModel]],
                                     propertyData: Option[Seq[PropertyDetailsModel]])

  object TaxPayerDisplayResponse {
    implicit val taxPayerDisplayResponseFormat: OFormat[TaxPayerDisplayResponse] = Json.format[TaxPayerDisplayResponse]
  }
}