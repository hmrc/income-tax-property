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

package models

import models.request.BalancingCharge
import models.request.foreign.adjustments.{ForeignPropertyAdjustmentsWithCountryCode, ForeignUnusedResidentialFinanceCost, ForeignWhenYouReportedTheLoss, UnusedLossesPreviousYears}
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsError, JsSuccess, Json}

class ForeignPropertyAdjustmentsSpec extends PlaySpec {

  "ForeignPropertyAdjustments" should {

    "serialize to JSON correctly" in {
      val adjustments = ForeignPropertyAdjustmentsWithCountryCode(
        countryCode = "AUS",
        privateUseAdjustment = 15.15,
        balancingCharge = BalancingCharge(
          balancingChargeYesNo = true,
          balancingChargeAmount = Some(108)),
        residentialFinanceCost = None,
        unusedResidentialFinanceCost = None,
        propertyIncomeAllowanceClaim = Some(12.34),
        unusedLossesPreviousYears = UnusedLossesPreviousYears(
          unusedLossesPreviousYearsYesNo = true,
          unusedLossesPreviousYearsAmount = Some(109.09)
        ),
        whenYouReportedTheLoss = Some(ForeignWhenYouReportedTheLoss.y2018to2019)
      )

      val expectedJson = Json.parse(
        """
          |{
          |  "countryCode": "AUS",
          |  "privateUseAdjustment": 15.15,
          |  "balancingCharge": {
          |    "balancingChargeYesNo": true,
          |    "balancingChargeAmount": 108
          |  },
          |  "propertyIncomeAllowanceClaim": 12.34,
          |  "unusedLossesPreviousYears": {
          |    "unusedLossesPreviousYearsYesNo": true,
          |    "unusedLossesPreviousYearsAmount": 109.09
          |  },
          |  "whenYouReportedTheLoss": "y2018to2019"
          |}
          |""".stripMargin
      )

      Json.toJson(adjustments) mustEqual expectedJson
    }

    "deserialize from JSON correctly" in {
      val json = Json.parse(
        """
          |{
          |  "countryCode": "AUS",
          |  "privateUseAdjustment": 15.15,
          |  "balancingCharge": {
          |    "balancingChargeYesNo": true,
          |    "balancingChargeAmount": 108
          |  },
          |  "residentialFinanceCost": 300,
          |  "unusedResidentialFinanceCost": {
          |    "foreignUnusedResidentialFinanceCostYesNo": true,
          |    "foreignUnusedResidentialFinanceCostAmount": 110.10
          |  },
          |  "unusedLossesPreviousYears": {
          |    "unusedLossesPreviousYearsYesNo": true,
          |    "unusedLossesPreviousYearsAmount": 109.09
          |  }
          |}
          |""".stripMargin
      )

      val expectedAdjustments = ForeignPropertyAdjustmentsWithCountryCode(
        countryCode = "AUS",
        privateUseAdjustment = 15.15,
        balancingCharge = BalancingCharge(
          balancingChargeYesNo = true,
          balancingChargeAmount = Some(108)),
        residentialFinanceCost = Some(300.00),
        unusedResidentialFinanceCost = Some(ForeignUnusedResidentialFinanceCost(
          foreignUnusedResidentialFinanceCostYesNo = true,
          foreignUnusedResidentialFinanceCostAmount = Some(110.10)
        )),
        propertyIncomeAllowanceClaim = None,
        unusedLossesPreviousYears = UnusedLossesPreviousYears(
          unusedLossesPreviousYearsYesNo = true,
          unusedLossesPreviousYearsAmount = Some(109.09)
        ),
        whenYouReportedTheLoss = None
      )

      json.validate[ForeignPropertyAdjustmentsWithCountryCode] mustEqual JsSuccess(expectedAdjustments)
    }

    "return a JsError for invalid JSON" in {
      val invalidJson = Json.parse(
        """
          |{
          |  "countryCode": "AUS",
          |  "privateUseAdjustment": "invalid value"
          |}
          |""".stripMargin
      )

      invalidJson.validate[ForeignPropertyAdjustmentsWithCountryCode] mustBe a[JsError]
    }

    "handle optional fields correctly" in {
      val jsonWithMissingFields = Json.parse(
        """
          |{
          |  "countryCode": "AUS",
          |  "privateUseAdjustment": 15.15,
          |  "balancingCharge": {
          |    "balancingChargeYesNo": false
          |  },
          |  "residentialFinanceCost": 300,
          |  "unusedResidentialFinanceCost": {
          |    "foreignUnusedResidentialFinanceCostYesNo": false
          |  },
          |  "unusedLossesPreviousYears": {
          |    "unusedLossesPreviousYearsYesNo": false
          |  }
          |}
          |""".stripMargin
      )

      val expectedAdjustments = ForeignPropertyAdjustmentsWithCountryCode(
        countryCode = "AUS",
        privateUseAdjustment = 15.15,
        balancingCharge = BalancingCharge(
          balancingChargeYesNo = false,
          balancingChargeAmount = None
        ),
        residentialFinanceCost = Some(300.00),
        unusedResidentialFinanceCost = Some(ForeignUnusedResidentialFinanceCost(
          foreignUnusedResidentialFinanceCostYesNo = false,
          foreignUnusedResidentialFinanceCostAmount = None
        )),
        propertyIncomeAllowanceClaim = None,
        unusedLossesPreviousYears = UnusedLossesPreviousYears(
          unusedLossesPreviousYearsYesNo = false,
          unusedLossesPreviousYearsAmount = None
        ),
        whenYouReportedTheLoss = None
      )

      jsonWithMissingFields.validate[ForeignPropertyAdjustmentsWithCountryCode] mustEqual JsSuccess(expectedAdjustments)
    }

  }
}
