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

import monocle.Optional
import monocle.macros.GenLens
import play.api.libs.json.{OFormat, Writes, Json, JsValue}
import play.api.libs.ws.BodyWritable

case class  ForeignIncomeDividends(amountBeforeTax: Option[BigDecimal],
                                  taxTakenOff: Option[BigDecimal],
                                  specialWithholdingTax: Option[BigDecimal],
                                  foreignTaxCreditRelief: Boolean,
                                  taxableAmount: BigDecimal)

object ForeignIncomeDividends {
  implicit val format: OFormat[ForeignIncomeDividends] = Json.format[ForeignIncomeDividends]
}

case class ForeignIncome(countryCode: String, dividends: Option[ForeignIncomeDividends])

object ForeignIncome {
  implicit val format: OFormat[ForeignIncome] = Json.format[ForeignIncome]
}

case class ForeignIncomeSubmissionDividends(
       foreignIncome: Option[Seq[ForeignIncome]]
                                                     )

object ForeignIncomeSubmissionDividends {
  implicit val format: OFormat[ForeignIncomeSubmissionDividends] =
    Json.format[ForeignIncomeSubmissionDividends]
}

case class ForeignIncomeSubmission(
  foreignIncome: Option[Seq[ForeignIncome]]
                                           )

object ForeignIncomeSubmission {
  implicit val format: OFormat[ForeignIncomeSubmission] = Json.format[ForeignIncomeSubmission]

  implicit def jsonBodyWritable[T](implicit
                                   writes: Writes[T],
                                   jsValueBodyWritable: BodyWritable[JsValue]
                                  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  def fromForeignIncomeDividends(
    foreignIncomeDividendsWithCountryCode: ForeignIncomeDividendsWithCountryCode
                                ): ForeignIncomeSubmissionDividends = {

    val countryCode = foreignIncomeDividendsWithCountryCode.countryCode
    val foreignIncomeLens = GenLens[ForeignIncomeSubmissionDividends](_.foreignIncome)
    val foreignIncomeDividendsLens = GenLens[ForeignIncome](_.dividends)
    val firstForeignIncomeDividendsLens: Optional[ForeignIncomeSubmissionDividends, ForeignIncomeDividends] =
      foreignIncomeLens.some.index(0).andThen(foreignIncomeDividendsLens.some)

    val newForeignIncomeDividends = ForeignIncomeDividends(
      amountBeforeTax = foreignIncomeDividendsWithCountryCode.amountBeforeTax,
      taxTakenOff = foreignIncomeDividendsWithCountryCode.taxTakenOff,
      specialWithholdingTax = foreignIncomeDividendsWithCountryCode.specialWithholdingTax,
      foreignTaxCreditRelief = foreignIncomeDividendsWithCountryCode.foreignTaxCreditRelief,
      taxableAmount = foreignIncomeDividendsWithCountryCode.taxableAmount
    )

    val emptyAllowances = ForeignIncomeSubmissionDividends(
      foreignIncome = Some(
        Seq(
          ForeignIncome(
            countryCode = countryCode,
            dividends = Some(ForeignIncomeDividends(None, None, None, foreignTaxCreditRelief = false, 0))
        )
      )
    )
    )

    val foreignIncomeSubmissionDividendsWithNewAllowances =
      firstForeignIncomeDividendsLens.replace(newForeignIncomeDividends)(emptyAllowances)

    foreignIncomeSubmissionDividendsWithNewAllowances
  }
}
