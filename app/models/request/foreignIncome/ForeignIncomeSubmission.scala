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

package models.request.foreignIncome

import models.request.ForeignIncomeDividendsWithCountryCode
import monocle.Optional
import monocle.macros.GenLens
import play.api.libs.json.{OFormat, Writes, Json, JsValue}
import play.api.libs.ws.BodyWritable

case class  ForeignIncomeDividends(countryCode: String,
                                  amountBeforeTax: Option[BigDecimal],
                                  taxTakenOff: Option[BigDecimal],
                                  specialWithholdingTax: Option[BigDecimal],
                                  foreignTaxCreditRelief: Boolean,
                                  taxableAmount: BigDecimal)

object ForeignIncomeDividends {
  implicit val format: OFormat[ForeignIncomeDividends] = Json.format[ForeignIncomeDividends]
}

case class ForeignIncomeDividendsOption(countryCode: String, dividends: Option[ForeignIncomeDividends])

object ForeignIncomeDividendsOption {
  implicit val format: OFormat[ForeignIncomeDividendsOption] = Json.format[ForeignIncomeDividendsOption]
}

case class ForeignIncomeSubmissionDividends(
  foreignIncome: Option[Seq[ForeignIncomeDividends]]
                                           )

object ForeignIncomeSubmissionDividends {
  implicit val format: OFormat[ForeignIncomeDividends] = Json.format[ForeignIncomeDividends]

  implicit def jsonBodyWritable[T](implicit
                                   writes: Writes[T],
                                   jsValueBodyWritable: BodyWritable[JsValue]
                                  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  def fromForeignIncomeDividends(
    foreignIncomeDividendsWithCountryCode: ForeignIncomeDividendsWithCountryCode
                                ): ForeignIncomeSubmissionDividends = {
    val foreignIncomeLens = GenLens[ForeignIncomeSubmissionDividends](_.foreignIncome)
    val foreignIncomeDividendsLens = GenLens[ForeignIncomeDividendsOption](_.dividends)
    val firstForeignIncomeDividendsLens: Optional[ForeignIncomeSubmissionDividends, ForeignIncomeDividends] =
      foreignIncomeLens.some.index(0).andThen(foreignIncomeDividendsLens.some)

    val newForeignIncomeDividends = ForeignIncomeDividends(
      countryCode = foreignIncomeDividendsWithCountryCode.countryCode,
      amountBeforeTax = foreignIncomeDividendsWithCountryCode.amountBeforeTax,
      taxTakenOff = foreignIncomeDividendsWithCountryCode.taxTakenOff,
      specialWithholdingTax = foreignIncomeDividendsWithCountryCode.specialWithholdingTax,
      foreignTaxCreditRelief = foreignIncomeDividendsWithCountryCode.foreignTaxCreditRelief,
      taxableAmount = foreignIncomeDividendsWithCountryCode.taxableAmount
    )

    val emptyAllowances = ForeignIncomeSubmissionDividends(
      foreignIncome = Some(
        Seq(
          ForeignIncomeDividends("", None, None, None, false, 0)
        )
      )
    )

    val foreignIncomeSubmissionDividendsWithNewAllowances =
      firstForeignIncomeDividendsLens.replace(newForeignIncomeDividends)(emptyAllowances)

    foreignIncomeSubmissionDividendsWithNewAllowances
  }
}
