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

final case class JourneyContextWithNino(
  taxYear: TaxYear,
  incomeSourceId: IncomeSourceId,
  mtditid: Mtditid,
  nino: Nino
) {
  def toJourneyContext(journeyName: JourneyName): JourneyContext =
    JourneyContext(taxYear, incomeSourceId, mtditid, journeyName)

}

final case class JourneyContext(
  taxYear: TaxYear,
  incomeSourceId: IncomeSourceId,
  mtditid: Mtditid,
  journey: JourneyName
) {
  def toJourneyContextWithNino(nino: Nino): JourneyContextWithNino =
    JourneyContextWithNino(taxYear, incomeSourceId, mtditid, nino)
}
