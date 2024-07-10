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

package repositories

import java.time.temporal.ChronoUnit
import java.time.{Instant, Month, ZoneOffset}

/** This was getting used when timeToLive was agreed to be 4 years. Now the timeToLive is changed to 28 days so this is
  * not needed for now
  */
object ExpireAtCalculator {
  private val StartTaxYearDayOfMonth = 6
  private val HowManyTaxYearsToStore = 4
  private val TaxYearStartMonth = Month.APRIL
  private val zoneId = ZoneOffset.UTC

  def calculateExpireAt(nowInstant: Instant): Instant = {
    val now = nowInstant.atZone(zoneId)

    val startOfThisTaxYear = {
      val startOfYear = now.withMonth(TaxYearStartMonth.getValue).withDayOfMonth(StartTaxYearDayOfMonth)
      if (now.isBefore(startOfYear)) startOfYear.minusYears(1) else startOfYear
    }

    val startOfTaxYearFourYearsFromNow = startOfThisTaxYear
      .plusYears(HowManyTaxYearsToStore)
      .withDayOfMonth(StartTaxYearDayOfMonth)
      .truncatedTo(ChronoUnit.DAYS)

    startOfTaxYearFourYearsFromNow.toInstant
  }
}
