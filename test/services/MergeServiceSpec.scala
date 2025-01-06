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

package services
import models.domain.JourneyAnswers
import models.request.{DeductingTax, PropertyRentalsIncome}
import models.responses.{AnnualForeignProperty, ForeignPropertyAllowances, PeriodicSubmissionId, PropertyAnnualSubmission, PropertyPeriodicSubmission, UkOtherProperty, UkOtherPropertyIncome}
import org.mockito.Mockito.when
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsObject, Json}
import utils.UnitTest

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class MergeServiceSpec extends UnitTest with Matchers with MockitoSugar with ScalaCheckPropertyChecks {

  val mockJourneyAnswers: JourneyAnswers = mock[JourneyAnswers]
  val otherUkProperty: UkOtherProperty = UkOtherProperty(
    Some(UkOtherPropertyIncome(None, None, Some(BigDecimal(567)), Some(BigDecimal(2340)), Some(BigDecimal(999)), None)),
    None
  )

  val propertyPeriodicSubmission: PropertyPeriodicSubmission = PropertyPeriodicSubmission(
    Some(PeriodicSubmissionId("periodicSubmissionId")),
    submittedOn = Some(LocalDateTime.now),
    fromDate = LocalDate.now(),
    toDate = LocalDate.now(),
    foreignProperty = None,
    ukOtherProperty = Some(otherUkProperty)
  )

  val mergeService = new MergeService()

  "mergeRentalsIncome" should {
    "merge correctly with valid inputs" in {

      val mockData: JsObject = Json.obj(
        "isNonUKLandlord" -> true
      )
      when(mockJourneyAnswers.data).thenReturn(mockData)

      val result = mergeService.mergeRentalsIncome(Some(propertyPeriodicSubmission), Some(mockJourneyAnswers))

      result shouldBe Some(
        PropertyRentalsIncome(
          isNonUKLandlord = true,
          567,
          999,
          Some(DeductingTax(taxDeductedYesNo = true, Some(BigDecimal(2340)))),
          None,
          None,
          None,
          None,
          None
        )
      )
    }

    "return None when no data is available" in {
      val result = mergeService.mergeRentalsIncome(None, None)

      result shouldBe None
    }
  }
  "mergeForeignPropertyAllowances" should {

    "return a map of foreign property allowances" in {
      val foreignPropertyAllowances = ForeignPropertyAllowances(
        annualInvestmentAllowance = Some(15.15),
        costOfReplacingDomesticItems = Some(25.25),
        zeroEmissionsGoodsVehicleAllowance = Some(35.35),
        otherCapitalAllowance = Some(45.45),
        electricChargePointAllowance = Some(55.55),
        structuredBuildingAllowance = Some(65.65),
        zeroEmissionsCarAllowance = Some(75.75),
        propertyAllowance = Some(85.85)
      )

      val aPropertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(
        submittedOn = Some(LocalDateTime.now()),
        ukOtherProperty = None,
        foreignProperty = Some(
          Seq(
            AnnualForeignProperty(
              countryCode = "ESP",
              adjustments = None,
              allowances = Some(
                ForeignPropertyAllowances(
                  annualInvestmentAllowance = Some(15.15),
                  costOfReplacingDomesticItems = Some(25.25),
                  zeroEmissionsGoodsVehicleAllowance = Some(35.35),
                  otherCapitalAllowance = Some(45.45),
                  electricChargePointAllowance = Some(55.55),
                  structuredBuildingAllowance = Some(65.65),
                  zeroEmissionsCarAllowance = Some(75.75),
                  propertyAllowance = Some(85.85)
                )
              )
            )
          )
        )
      )

      val service = new MergeService()
      val result = service.mergeForeignPropertyAllowances(aPropertyAnnualSubmission)

      result shouldBe Some(
        Map(
          "ESP" -> foreignPropertyAllowances
        )
      )
    }

    "return an empty map when foreign properties have no allowances" in {
      val foreignProperties = Seq(
        AnnualForeignProperty(
          countryCode = "ESP",
          adjustments = None,
          allowances = None
        )
      )

      val propertyAnnualSubmission = PropertyAnnualSubmission(
        submittedOn = None,
        foreignProperty = Some(foreignProperties),
        ukOtherProperty = None
      )

      val service = new MergeService()
      val result = service.mergeForeignPropertyAllowances(propertyAnnualSubmission)

      result shouldBe Some(Map.empty[String, ForeignPropertyAllowances])
    }

  }
}
