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

package models.domain

import models.RentalsAndRaRAbout
import models.request._
import models.request.esba.EsbaInfo
import models.request.foreign.ForeignPropertySelectCountry
import models.request.sba.SbaInfo
import models.request.ukrentaroom.RaRAdjustments
import play.api.libs.json.{Json, OFormat}

final case class FetchedUKPropertyData(
  capitalAllowancesForACar: Option[CapitalAllowancesForACar],
  propertyAbout: Option[PropertyAbout],
  propertyRentalsAbout: Option[PropertyRentalsAbout],
  rentalsAndRaRAbout: Option[RentalsAndRaRAbout],
  adjustments: Option[PropertyRentalAdjustments],
  rentalsAndRaRAdjustments: Option[PropertyRentalAdjustments],
  allowances: Option[RentalAllowances],
  rentalsAndRaRAllowances: Option[RentalAllowances],
  esbasWithSupportingQuestions: Option[EsbaInfo],
  rentalsAndRaREsbasWithSupportingQuestions: Option[EsbaInfo],
  sbasWithSupportingQuestions: Option[SbaInfo],
  rentalsAndRaRSbasWithSupportingQuestions: Option[SbaInfo],
  propertyRentalsIncome: Option[PropertyRentalsIncome],
  rentalsAndRaRIncome: Option[RentalsAndRaRIncome],
  propertyRentalsExpenses: Option[PropertyRentalsExpense],
  rentalsAndRaRExpenses: Option[PropertyRentalsExpense],
  raRAbout: Option[RaRAbout],
  rarExpenses: Option[RentARoomExpenses],
  raRAdjustments: Option[RaRAdjustments],
  rentARoomAllowances: Option[RentARoomAllowances],
  journeyStatuses: List[JourneyWithStatus],
  foreignPropertySelectCountry: Option[ForeignPropertySelectCountry]
)

object FetchedUKPropertyData {
  implicit val format: OFormat[FetchedUKPropertyData] = Json.format[FetchedUKPropertyData]
}
