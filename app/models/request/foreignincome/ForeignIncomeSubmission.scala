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

package models.request.foreignincome

import models.domain.{FetchedUKPropertyData, FetchedPropertyData, FetchedForeignIncomeData, FetchedForeignPropertyData, FetchedData, FetchedUkAndForeignPropertyData}
import monocle.Optional
import play.api.libs.json.{OFormat, Writes, Json, JsValue}
import play.api.libs.ws.BodyWritable

case class ForeignIncomeSubmission(
  foreignDividend: Option[Seq[ForeignDividend]],
  dividendIncomeReceivedWhilstAbroad: Option[Seq[ForeignDividend]],
  stockDividend: Option[GrossAmountWithReference],
  redeemableShares: Option[GrossAmountWithReference],
  bonusIssuesOfSecurities: Option[GrossAmountWithReference],
  closeCompanyLoansWrittenOff: Option[GrossAmountWithReference]
)

case class ForeignDividend(
  countryCode: String,
  amountBeforeTax: Option[BigDecimal],
  taxTakenOff: Option[BigDecimal],
  specialWithholdingTax: Option[BigDecimal],
  foreignTaxCreditRelief: Option[Boolean],
  taxableAmount: BigDecimal
)

object ForeignDividend {
  implicit val format: OFormat[ForeignDividend] = Json.format[ForeignDividend]
}

case class GrossAmountWithReference(
  customerReference: Option[String],
  grossAmount: BigDecimal
)

object GrossAmountWithReference {
  implicit val format: OFormat[GrossAmountWithReference] = Json.format[GrossAmountWithReference]
}

object ForeignIncomeSubmission {
  implicit val format: OFormat[ForeignIncomeSubmission] = Json.format[ForeignIncomeSubmission]

  val emptyForeignIncomeSubmission: ForeignIncomeSubmission = ForeignIncomeSubmission(None, None, None, None, None, None)
  val emptyFetchedPropertyData: FetchedPropertyData = FetchedPropertyData(
    FetchedUKPropertyData(None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, List(), None),
    FetchedForeignPropertyData(None, None, None, None, None, None, None),
    FetchedUkAndForeignPropertyData(None)
  )
  val emptyFetchedForeignIncomeData: FetchedForeignIncomeData = FetchedForeignIncomeData(None, List())
  val emptyFetchedData = FetchedData(emptyFetchedPropertyData, emptyFetchedForeignIncomeData)

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  def fromForeignIncomeDividends(
    foreignIncomeSubmission: ForeignIncomeSubmission,
    request: ForeignIncomeDividendsWithCountryCode
  ): ForeignIncomeSubmission = {

    val foreignDividendLens: Optional[ForeignIncomeSubmission, Seq[ForeignDividend]] =
      Optional[ForeignIncomeSubmission, Seq[ForeignDividend]] {
        case ForeignIncomeSubmission(None, _, _, _, _, _) => Some(Seq.empty[ForeignDividend])
        case ForeignIncomeSubmission(fd, _, _, _, _, _)   => fd
      } { fd => dis =>
        dis.copy(foreignDividend = Some(fd))
      }

    val requestForeignIncomeDividends: Seq[ForeignIncomeDividend] = request.foreignIncomeDividends
    val downstreamForeignIncomeDividends: Seq[ForeignDividend] = foreignDividendLens
      .getOption(foreignIncomeSubmission).getOrElse(Seq.empty[ForeignDividend])
    val updatedForeignIncomeDividends: Seq[ForeignDividend] = requestForeignIncomeDividends.map { requestDividend =>
      val downstreamDividend = downstreamForeignIncomeDividends.find(_.countryCode == requestDividend.countryCode)
      requestDividend.toForeignDividend.copy(
        specialWithholdingTax = downstreamDividend.flatMap(_.specialWithholdingTax),
        taxableAmount = downstreamDividend.map(_.taxableAmount).getOrElse(0)
      )
    }

   foreignDividendLens.replace(updatedForeignIncomeDividends)(foreignIncomeSubmission)
  }
}
