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

package uk.gov.hmrc.incometaxproperty.models.Responses

import play.api.libs.functional.syntax._
import play.api.libs.json._

import java.time.LocalDate
import scala.util.{Success, Try}

case class PropertyDetailsModel(incomeSourceId: String,
                                accountingPeriod: AccountingPeriodModel,
                                contactDetails: Option[ContactDetailsModel],
                                propertiesRented: Option[PropertiesRentedModel],
                                cessation: Option[CessationModel],
                                paperless: Option[Boolean],
                                firstAccountingPeriodEndDate: Option[LocalDate],
                                incomeSourceType: Option[String],
                                tradingStartDate: Option[LocalDate],
                                latencyDetails: Option[LatencyDetails],
                                cashOrAccruals: Option[Boolean])

  object PropertyDetailsModel extends CustomReads {

    implicit val desReads: Reads[PropertyDetailsModel] = (
      (__ \ "incomeSourceId").read[String] and
        __.read(AccountingPeriodModel.desReads) and
        (__ \ "emailAddress").readNullable[String] and
        (__ \ "numPropRentedUK").readNullable[Int](readInt) and
        (__ \ "numPropRentedEEA").readNullable[Int](readInt) and
        (__ \ "numPropRentedNONEEA").readNullable[Int](readInt) and
        (__ \ "numPropRented").readNullable[Int](readInt) and
        (__ \ "cessationDate").readNullable[LocalDate] and
        (__ \ "cessationReason").readNullable[String] and
        (__ \ "paperLess").readNullable[Boolean] and
        (__ \ "firstAccountingPeriodEndDate").readNullable[LocalDate] and
        (__ \ "incomeSourceType").readNullable[String] and
        (__ \ "tradingStartDate").readNullable[LocalDate] and
        (__ \ "latencyDetails").readNullable[LatencyDetails] and
        (__ \ "cashOrAccruals").readNullable[Boolean]
      ) (PropertyDetailsModel.applyWithFields _)

    def applyWithFields(incomeSourceId: String,
                        accountingPeriod: AccountingPeriodModel,
                        email: Option[String],
                        uk: Option[Int],
                        eea: Option[Int],
                        nonEea: Option[Int],
                        total: Option[Int],
                        cessationDate: Option[LocalDate],
                        cessationReason: Option[String],
                        paperless: Option[Boolean],
                        firstAccountingPeriodEndDate: Option[LocalDate],
                        incomeSourceType: Option[String],
                        tradingStartDate: Option[LocalDate],
                        latencyDetails: Option[LatencyDetails],
                        cashOrAccruals: Option[Boolean]): PropertyDetailsModel = PropertyDetailsModel(
      incomeSourceId,
      accountingPeriod,
      ContactDetailsModel.propertyContactDetails(email),
      PropertiesRentedModel.propertiesRented(uk, eea, nonEea, total),
      CessationModel.cessation(cessationDate, cessationReason),
      paperless,
      firstAccountingPeriodEndDate,
      incomeSourceType,
      tradingStartDate,
      latencyDetails,
      cashOrAccruals
    )

    implicit val writes: OWrites[PropertyDetailsModel] = Json.writes[PropertyDetailsModel]

  }

case class PropertiesRentedModel(uk: Option[Int], eea: Option[Int], nonEea: Option[Int], total: Option[Int])

object PropertiesRentedModel extends CustomReads {

  def propertiesRented(uk: Option[Int], eea: Option[Int], nonEea: Option[Int], total: Option[Int]): Option[PropertiesRentedModel] = {
    (uk, eea, nonEea, total) match {
      case (None, None, None, None) => None
      case _ => Some(PropertiesRentedModel(uk, eea, nonEea, total))
    }
  }

  val desReads: Reads[PropertiesRentedModel] = (
    (__ \ "numPropRentedUK").readNullable[Int](readInt) and
      (__ \ "numPropRentedEEA").readNullable[Int](readInt) and
      (__ \ "numPropRentedNONEEA").readNullable[Int](readInt) and
      (__ \ "numPropRented").readNullable[Int](readInt)
    ) (PropertiesRentedModel.apply _)

  implicit val format: Format[PropertiesRentedModel] = Json.format[PropertiesRentedModel]

}

trait CustomReads {

  val readInt: Reads[Int] = implicitly[Reads[Int]].orElse(implicitly[Reads[String]]
    .map(x => Try(x.toInt))
    .collect(JsonValidationError(Seq("Parsing error"))) {
      case Success(a) => a
    })
}
