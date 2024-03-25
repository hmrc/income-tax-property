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

package utils.builders

import models.responses.IncomeSourceDetailsModel.TaxPayerDisplayResponse
import models.responses._

import java.time.{LocalDate, LocalDateTime, LocalTime}

object IncomeSourceDetailsBuilder {

  private val taxYear = LocalDate.now.getYear

  val anIncomeSourceDetails = IncomeSourceDetailsModel(
    processingDate = LocalDateTime.of(LocalDate.parse(s"${taxYear - 5}-03-13"), LocalTime.of(10, 11)),
    taxPayerDisplayResponse = TaxPayerDisplayResponse(
      safeId = "EK3074559847852",
      nino = "AC160000B",
      mtdId = "160000",
      yearOfMigration = Some("2022"),
      propertyIncome = false,
      businessData = Some(Seq(
        BusinessDetailsModel(
          incomeSourceId = "SJPR05893938418",
          accountingPeriodStartDate = LocalDate.parse("2023-02-20"),
          accountingPeriodEndDate = LocalDate.parse("2024-02-29"),
          tradingName = Some("Circus Performer"),
          businessAddressDetails = Some(BusinessAddressDetails(
            Some("Address line 1"),
            Some("Address line 2"),
            Some("Address line 3"),
            None,
            Some("POSTCODE"),
            Some("GB")
          )),
          businessContactDetails = Some(BusinessContactDetails(
            Some("160000"), None, None, Some("imanemail@email.com")
          )),
          tradingStartDate = Some(LocalDate.parse("2023-04-06")),
          latencyDetails = Some(LatencyDetails(
            latencyEndDate = LocalDate.parse("2020-02-27"),
            taxYear1 = "2019",
            taxYear2 = "2020",
            latencyIndicator1 = "A",
            latencyIndicator2 = "A"
          )),
          cashOrAccruals = Some(true),
          seasonal = None,
          cessationDate = Some(LocalDate.parse("2024-04-05")),
          paperless = None,
          firstAccountingPeriodEndDate = Some(LocalDate.parse("2020-02-29")),
          firstAccountingPeriodStartDate = Some(LocalDate.parse("2019-09-30"))
        )
      )),
      propertyData = Some(List(PropertyDetailsModel(
        incomeSourceType = Some("uk-property"),
        incomeSourceId = "3147a709e9-f3fa5-6edf-4e3c701-8f6500a-c10dde61c3c11afe8475",
        accountingPeriodStartDate = LocalDate.now().minusDays(1),
        accountingPeriodEndDate = LocalDate.now(),
        tradingStartDate = Some(LocalDate.now()),
        cashOrAccruals = Some(false),
        numPropRented = Some(1),
        numPropRentedUK = Some(1),
        numPropRentedEEA = None,
        numPropRentedNONEEA = None,
        email = Some("statebenefits@email.com"),
        cessationDate = None,
        paperLess = Some(true),
        incomeSourceStartDate = None,
        firstAccountingPeriodStartDate = None,
        firstAccountingPeriodEndDate = Some(LocalDate.now()),
        latencyDetails = None
      )))
    )
  )
}
