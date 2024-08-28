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

package models.common

import enumeratum._

sealed abstract class JourneyName(override val entryName: String) extends EnumEntry {
  override def toString: String = entryName
}

object JourneyName extends Enum[JourneyName] with utils.PlayJsonEnum[JourneyName] {
  val values: IndexedSeq[JourneyName] = findValues

  case object NoJourney extends JourneyName("no-journey")

  case object AllJourneys extends JourneyName("all-journeys")

  case object About extends JourneyName("property-about")

  // Rental
  case object RentalAbout extends JourneyName("property-rental-about")

  case object RentalIncome extends JourneyName("rental-income")

  case object RentalAllowances extends JourneyName("rental-allowances")

  case object RentalExpenses extends JourneyName("rental-expenses")

  case object RentalAdjustments extends JourneyName("rental-adjustments")

  case object RentalSBA extends JourneyName("rental-sba")

  case object RentalESBA extends JourneyName("rental-esba")

  // RentARoom
  case object RentARoomAbout extends JourneyName("rent-a-room-about")

  case object RentARoomAllowances extends JourneyName("rent-a-room-allowances")

  case object RentARoomExpenses extends JourneyName("rent-a-room-expenses")

  case object RentARoomAdjustments extends JourneyName("rent-a-room-adjustments")

  // RentalAndRaR
  case object RentalsAndRaRAbout extends JourneyName("property-rentals-and-rent-a-room-about")

  case object RentalsAndRaRIncome extends JourneyName("property-rentals-and-rent-a-room-income")

  case object RentalsAndRaRAllowances extends JourneyName("property-rentals-and-rent-a-room-allowances")

  case object RentalsAndRaRExpenses extends JourneyName("property-rentals-and-rent-a-room-expenses")

  case object RentalsAndRaRAdjustments extends JourneyName("property-rentals-and-rent-a-room-adjustments")

}
