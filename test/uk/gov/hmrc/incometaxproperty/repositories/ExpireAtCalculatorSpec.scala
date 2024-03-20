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

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time._

class ExpireAtCalculatorSpec extends AnyWordSpecLike with TableDrivenPropertyChecks with ScalaCheckPropertyChecks with Matchers {
  val testCases: TableFor2[Instant, Instant] = Table(
    ("now", "expectedExpireAt"),
    (Instant.parse("2023-01-01T00:00:00Z"), Instant.parse("2026-04-06T00:00:00Z")),
    (Instant.parse("2023-10-01T00:00:00Z"), Instant.parse("2027-04-06T00:00:00Z")),
    (toLondonZone(2023, 1, 1, 0, 0), Instant.parse("2026-04-06T00:00:00Z")),
    (Instant.parse("2023-04-05T23:59:59Z"), Instant.parse("2026-04-06T00:00:00Z")), // edge case for tax year 2022-23
    (toLondonZone(2023, 4, 5, 23, 59), Instant.parse("2026-04-06T00:00:00Z")),
    (Instant.parse("2023-04-06T00:00:00Z"), Instant.parse("2027-04-06T00:00:00Z")), // the first day of the tax year 2023-24
    (toLondonZone(2023, 4, 6, 23, 59), Instant.parse("2027-04-06T00:00:00Z"))
  )

  "calculateExpireAt" should {
    "calculate the expire date for specific cases" in {
      forAll(testCases) { (nowInstant, expectedExpireAt) =>
        ExpireAtCalculator.calculateExpireAt(nowInstant) shouldBe expectedExpireAt
      }
    }

    "calculate the correct expiration date for any given date" in {
      forAll { (nowInstant: Instant) =>
        whenever(nowInstant.isAfter(Instant.EPOCH)) {
          val now              = nowInstant.atZone(ZoneId.of("Europe/London"))
          val year             = if (isBeforeNewTaxYear(now)) now.getYear - 1 else now.getYear
          val expectedExpireAt = LocalDateTime.of(year + 4, 4, 6, 0, 0).atZone(ZoneOffset.UTC).toInstant

          val expireAt = ExpireAtCalculator.calculateExpireAt(nowInstant)

          expireAt shouldBe expectedExpireAt
        }
      }
    }
  }

  private def toLondonZone(year: Int, month: Int, dayOfMonth: Int, hour: Int, minute: Int) =
    LocalDateTime.of(year, month, dayOfMonth, hour, minute).atZone(ZoneId.of("Europe/London")).toInstant

  private def isBeforeNewTaxYear(now: ZonedDateTime) =
    now.getMonthValue < 4 || (now.getMonthValue == 4 && now.getDayOfMonth < 6)

}
