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

case class BusinessDetailsModel(incomeSourceId: String,
                                accountingPeriod: AccountingPeriodModel,
                                tradingName: Option[String],
                                address: Option[AddressModel],
                                contactDetails: Option[ContactDetailsModel],
                                tradingStartDate: Option[LocalDate],
                                cashOrAccruals: Option[Boolean],
                                seasonal: Option[Boolean],
                                cessation: Option[CessationModel],
                                paperless: Option[Boolean],
                                firstAccountingPeriodEndDate: Option[LocalDate],
                                latencyDetails: Option[LatencyDetails])

object BusinessDetailsModel {

  private val cashOrAccrualsUntilRelease10: Reads[Option[Boolean]] = (__ \ "cashOrAccruals").readNullable[String].map {
    case Some("cash") => Some(false)
    case Some("accruals") => Some(true)
    case _ => None
  }

  val desReads: Reads[BusinessDetailsModel] = (
    (__ \ "incomeSourceId").read[String] and
      __.read(AccountingPeriodModel.desReads) and
      (__ \ "tradingName").readNullable[String] and
      (__ \ "businessAddressDetails").readNullable(AddressModel.desReads) and
      (__ \ "businessContactDetails").readNullable(ContactDetailsModel.desReads) and
      (__ \ "tradingStartDate").readNullable[LocalDate] and
      (__ \ "cashOrAccruals").readNullable[Boolean].orElse(
        cashOrAccrualsUntilRelease10
      ) and
      (__ \ "seasonal").readNullable[Boolean] and
      (__ \ "cessationDate").readNullable[LocalDate] and
      (__ \ "cessationReason").readNullable[String] and
      (__ \ "paperLess").readNullable[Boolean] and
      (__ \ "firstAccountingPeriodEndDate").readNullable[LocalDate] and
      (__ \ "latencyDetails").readNullable[LatencyDetails]
    ) (BusinessDetailsModel.applyWithFields _)

  def applyWithFields(incomeSourceId: String,
                      accountingPeriodModel: AccountingPeriodModel,
                      tradingName: Option[String],
                      address: Option[AddressModel],
                      contactDetails: Option[ContactDetailsModel],
                      tradingStartDate: Option[LocalDate],
                      cashOrAccruals: Option[Boolean],
                      seasonal: Option[Boolean],
                      cessationDate: Option[LocalDate],
                      cessationReason: Option[String],
                      paperless: Option[Boolean],
                      firstAccountingPeriodEndDate: Option[LocalDate],
                      latencyDetails: Option[LatencyDetails]): BusinessDetailsModel =
    BusinessDetailsModel(
      incomeSourceId,
      accountingPeriodModel,
      tradingName,
      address,
      contactDetails,
      tradingStartDate,
      cashOrAccruals,
      seasonal,
      CessationModel.cessation(cessationDate, cessationReason),
      paperless,
      firstAccountingPeriodEndDate,
      latencyDetails
    )


  implicit val format: Format[BusinessDetailsModel] = Json.format[BusinessDetailsModel]
}

case class LatencyDetails(latencyEndDate: LocalDate, taxYear1: String, latencyIndicator1: String, taxYear2: String, latencyIndicator2: String)

object LatencyDetails {
  implicit val format: Format[LatencyDetails] = Json.format[LatencyDetails]
}

case class CessationModel(date: Option[LocalDate], reason: Option[String])

object CessationModel {

  val desReads: Reads[CessationModel] = (
    (__ \ "cessationDate").readNullable[LocalDate] and
      (__ \ "cessationReason").readNullable[String]
    ) (CessationModel.apply _)

  def cessation(date: Option[LocalDate], reason: Option[String]): Option[CessationModel] =
    (date, reason) match {
      case (None, None) => None
      case _ => Some(CessationModel(date, reason))
    }

  implicit val format: Format[CessationModel] = Json.format[CessationModel]
}

case class AddressModel(addressLine1: String,
                        addressLine2: Option[String],
                        addressLine3: Option[String],
                        addressLine4: Option[String],
                        postCode: Option[String],
                        countryCode: String)

object AddressModel {

  val desReads: Reads[AddressModel] = (
    (__ \ "addressLine1").read[String] and
      (__ \ "addressLine2").readNullable[String] and
      (__ \ "addressLine3").readNullable[String] and
      (__ \ "addressLine4").readNullable[String] and
      (__ \ "postalCode").readNullable[String] and
      (__ \ "countryCode").read[String]
    ) (AddressModel.apply _)

  implicit val format: Format[AddressModel] = Json.format[AddressModel]

}

case class AccountingPeriodModel(start: LocalDate, end: LocalDate)

object AccountingPeriodModel {

  val desReads: Reads[AccountingPeriodModel] = (
    (__ \ "accountingPeriodStartDate").read[LocalDate] and
      (__ \ "accountingPeriodEndDate").read[LocalDate]
    ) (AccountingPeriodModel.apply _)

  implicit val format: Format[AccountingPeriodModel] = Json.format[AccountingPeriodModel]
}

case class ContactDetailsModel(phoneNumber: Option[String],
                               mobileNumber: Option[String],
                               faxNumber: Option[String],
                               emailAddress: Option[String])

object ContactDetailsModel {

  val desReads: Reads[ContactDetailsModel] = (
    (__ \ "phoneNumber").readNullable[String] and
      (__ \ "mobileNumber").readNullable[String] and
      (__ \ "faxNumber").readNullable[String] and
      (__ \ "emailAddress").readNullable[String]
    ) (ContactDetailsModel.apply _)


  def propertyContactDetails(email: Option[String]): Option[ContactDetailsModel] =
    email match {
      case None => None
      case _ => Some(ContactDetailsModel(None, None, None, email))
    }

  implicit val format: Format[ContactDetailsModel] = Json.format[ContactDetailsModel]
}


