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

package models.request

import play.api.libs.json.{Json, OFormat}

final case class StoredIncome(
  isNonUKLandlord: Boolean,
  isTaxDeducted: Option[Boolean],
  calculatedFigureYourself: Option[CalculatedFigureYourself],
  yearLeaseAmount: Option[BigDecimal],
  receivedGrantLeaseAmount: Option[BigDecimal],
  isPremiumsGrantLease: Option[Boolean]
)

object StoredIncome {
  implicit val format: OFormat[StoredIncome] = Json.format[StoredIncome]

  def fromRentalsIncome(rentalsIncome: PropertyRentalsIncome): StoredIncome =
    StoredIncome(
      isNonUKLandlord = rentalsIncome.isNonUKLandlord,
      isTaxDeducted = rentalsIncome.deductingTax.map(_.isTaxDeducted),
      calculatedFigureYourself = rentalsIncome.calculatedFigureYourself,
      yearLeaseAmount = rentalsIncome.yearLeaseAmount,
      receivedGrantLeaseAmount = rentalsIncome.receivedGrantLeaseAmount,
      isPremiumsGrantLease = rentalsIncome.premiumsGrantLease.map(_.premiumsGrantLeaseReceived)
    )

  def fromRentalsAndRaRIncome(rentalsAndRaRIncome: RentalsAndRaRIncome): StoredIncome =
    StoredIncome(
      isNonUKLandlord = rentalsAndRaRIncome.isNonUKLandlord,
      isTaxDeducted = rentalsAndRaRIncome.deductingTax.map(_.isTaxDeducted),
      calculatedFigureYourself = rentalsAndRaRIncome.calculatedFigureYourself,
      yearLeaseAmount = rentalsAndRaRIncome.yearLeaseAmount,
      receivedGrantLeaseAmount = rentalsAndRaRIncome.receivedGrantLeaseAmount,
      isPremiumsGrantLease = rentalsAndRaRIncome.premiumsGrantLease.map(_.premiumsGrantLeaseReceived)
    )
}

final case class PremiumsGrantLease(premiumsGrantLeaseReceived: Boolean, premiumsGrantLease: Option[BigDecimal])

object PremiumsGrantLease {
  implicit val format: OFormat[PremiumsGrantLease] = Json.format[PremiumsGrantLease]
}
final case class ReversePremiumsReceived(reversePremiumsReceived: Boolean, reversePremiums: Option[BigDecimal])

object ReversePremiumsReceived {
  implicit val format: OFormat[ReversePremiumsReceived] = Json.format[ReversePremiumsReceived]
}

final case class CalculatedFigureYourself(calculatedFigureYourself: Boolean, amount: Option[BigDecimal])

object CalculatedFigureYourself {
  implicit val format: OFormat[CalculatedFigureYourself] = Json.format[CalculatedFigureYourself]
}

final case class DeductingTax(isTaxDeducted: Boolean, taxDeductedAmount: Option[BigDecimal])

object DeductingTax {
  implicit val format: OFormat[DeductingTax] = Json.format[DeductingTax]
}
