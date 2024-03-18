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

package uk.gov.hmrc.incometaxproperty.repositories

import java.time.temporal.ChronoUnit
import java.time.{Instant, Month, ZoneOffset}

object ExpireAtCalculator {
  private val StartTaxYearDayOfMonth = 6
  private val HowManyTaxYearsToStore = 4
  private val TaxYearStartMonth      = Month.APRIL
  private val zoneId                 = ZoneOffset.UTC

  def calculateExpireAt(nowInstant: Instant): Instant = {
    val now         = nowInstant.atZone(zoneId)
    val currentYear = now.getYear

    val startOfThisTaxYear =
      if (now.getMonthValue < TaxYearStartMonth.getValue || (now.getMonthValue == TaxYearStartMonth.getValue && now.getDayOfMonth < StartTaxYearDayOfMonth)) {
        now.withYear(currentYear - 1).withMonth(Month.APRIL.getValue).withDayOfMonth(StartTaxYearDayOfMonth)
      } else {
        now.withMonth(Month.APRIL.getValue).withDayOfMonth(StartTaxYearDayOfMonth)
      }

    val startOfTaxYearFourYearsFromNow = startOfThisTaxYear
      .plusYears(HowManyTaxYearsToStore)
      .withDayOfMonth(StartTaxYearDayOfMonth)
      .truncatedTo(ChronoUnit.DAYS)

    startOfTaxYearFourYearsFromNow.toInstant
  }
}
