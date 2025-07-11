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

package models.domain

import models.request.foreignincome.ForeignDividendsAnswers
import play.api.libs.json.{Json, OFormat}

case class FetchedData(
  propertyData: FetchedPropertyData,
  incomeData: FetchedForeignIncomeData
                      )

object FetchedData {
  implicit val format: OFormat[FetchedData] = Json.format[FetchedData]
}

final case class FetchedPropertyData(
  ukPropertyData: FetchedUKPropertyData,
  foreignPropertyData: FetchedForeignPropertyData,
  ukAndForeignPropertyData: FetchedUkAndForeignPropertyData
)

object FetchedPropertyData {
  implicit val format: OFormat[FetchedPropertyData] = Json.format[FetchedPropertyData]
}

final case class FetchedForeignIncomeData(
  foreignIncomeDividends: Option[Map[String, ForeignDividendsAnswers]],
  foreignIncomeJourneyStatuses: List[JourneyWithStatus]
)

object FetchedForeignIncomeData {
  implicit val format: OFormat[FetchedForeignIncomeData] = Json.format[FetchedForeignIncomeData]
}
