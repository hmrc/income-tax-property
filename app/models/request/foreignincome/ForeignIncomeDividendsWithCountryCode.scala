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

import play.api.libs.json.{Format, Json}

case class ForeignIncomeDividendsWithCountryCode(
  foreignIncomeDividends: Seq[ForeignIncomeDividend]
)

object ForeignIncomeDividendsWithCountryCode {
  implicit val format: Format[ForeignIncomeDividendsWithCountryCode] = Json.format[ForeignIncomeDividendsWithCountryCode]
}

case class ForeignIncomeDividend(
  countryCode: String,
  incomeBeforeForeignTaxDeducted: BigDecimal,
  foreignTaxDeductedFromDividendIncome: Boolean,
  howMuchForeignTaxDeductedFromDividendIncome: Option[BigDecimal],
  claimForeignTaxCreditRelief: Option[Boolean]
){
  def toForeignDividend: ForeignDividend = {
    ForeignDividend(
      countryCode = this.countryCode,
      amountBeforeTax = Some(this.incomeBeforeForeignTaxDeducted),
      taxTakenOff = this.howMuchForeignTaxDeductedFromDividendIncome,
      specialWithholdingTax = None,
      foreignTaxCreditRelief = this.claimForeignTaxCreditRelief,
      taxableAmount = 0
    )
  }
}

object ForeignIncomeDividend {
  implicit val format: Format[ForeignIncomeDividend] = Json.format[ForeignIncomeDividend]
}
