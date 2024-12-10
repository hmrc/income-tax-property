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

import models.common._
import models.domain.{ForeignFetchedPropertyData, JourneyAnswers, JourneyWithStatus}
import models.request.foreign.{ForeignIncomeTax, ForeignPropertyTax}
import models.responses._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.Json
import utils.UnitTest

import java.time.{Instant, LocalDate}
import scala.concurrent.ExecutionContext.Implicits.global

class ForeignMergeServiceSpec extends UnitTest with ScalaCheckPropertyChecks {

  private val incomeSourceId = IncomeSourceId("test-foreign-merge-service-income-id")
  val taxYear: TaxYear = TaxYear(2024)
  val mtditid = "1234567890"
  val submissionId = "test-foreign-merge-service-submission-id"
  val underTest = new ForeignMergeService()

  ".mergeAll" should {
    "merge annual and periodic downstream data with repository data" in {

      val countryCode = "USA"
      val foreignTaxCreditRelief = Some(true)
      val foreignTaxPaidOrDeducted = Some(BigDecimal(56.78))
      val foreignProperty = ForeignProperty(
        countryCode = countryCode,
        income = Some(ForeignPropertyIncome(
          rentIncome = Some(ForeignPropertyRentIncome(rentAmount = BigDecimal(12.34))),
          foreignTaxCreditRelief = foreignTaxCreditRelief,
          premiumsOfLeaseGrant = Some(BigDecimal(13.34)),
          otherPropertyIncome = Some(BigDecimal(24.56)),
          foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted,
          specialWithholdingTaxOrUkTaxPaid = Some(BigDecimal(89.10))
        )),
        expenses = Some(models.responses.ForeignPropertyExpenses(
          premisesRunningCosts = Some(BigDecimal(23.34)),
          repairsAndMaintenance = Some(BigDecimal(32.21)),
          financialCosts = Some(BigDecimal(54.32)),
          professionalFees = Some(BigDecimal(65.43)),
          travelCosts = Some(BigDecimal(22.22)),
          costOfServices = Some(BigDecimal(10.10)),
          residentialFinancialCost = Some(BigDecimal(11.11)),
          broughtFwdResidentialFinancialCost = Some(BigDecimal(23.22)),
          other = Some(BigDecimal(44.44)),
          consolidatedExpense = Some(BigDecimal(90.05))
        ))
      )
      val foreignIncomeTaxYesNo = true
      val foreignPropertyTaxStoreAnswers: ForeignPropertyTaxStoreAnswers = ForeignPropertyTaxStoreAnswers(
        foreignIncomeTaxYesNo = Some(foreignIncomeTaxYesNo)
      )
      val journeyStatus: JourneyWithStatus = JourneyWithStatus(JourneyName.ForeignPropertyTax.entryName, JourneyStatus.InProgress.entryName)
      val propertyAnnualSubmission: PropertyAnnualSubmission = PropertyAnnualSubmission(None, None, None)
      val propertyPeriodicSubmission: PropertyPeriodicSubmission = PropertyPeriodicSubmission(
        submissionId = Some(PeriodicSubmissionId(submissionId)),
        submittedOn = None,
        fromDate = LocalDate.parse(TaxYear.startDate(taxYear)),
        toDate = LocalDate.parse(TaxYear.endDate(taxYear)),
        foreignProperty = Some(Seq(foreignProperty)),
        ukOtherProperty = None
      )
      val resultFromRepository: Map[String, Map[String, JourneyAnswers]] = Map(
        JourneyName.ForeignPropertyTax.entryName -> Map(
          countryCode -> JourneyAnswers(
            mtditid = Mtditid(mtditid),
            incomeSourceId = incomeSourceId,
            taxYear = taxYear,
            journey = JourneyName.ForeignPropertyTax,
            countryCode = Some(countryCode),
            status = JourneyStatus.InProgress,
            data = Json.toJsObject(foreignPropertyTaxStoreAnswers),
            createdAt = Instant.now(),
            updatedAt = Instant.now()
          )
        )
      )
      val expectedResult: ForeignFetchedPropertyData = ForeignFetchedPropertyData(
        foreignPropertyTax = Some(Map(
          countryCode -> ForeignPropertyTax(
            foreignIncomeTax = Some(ForeignIncomeTax(
              foreignIncomeTaxYesNo = foreignIncomeTaxYesNo,
              foreignTaxPaidOrDeducted = foreignTaxPaidOrDeducted
            )),
            foreignTaxCreditRelief = foreignTaxCreditRelief
          ))),
        foreignJourneyStatuses = Some(Map(
          countryCode -> List(
            journeyStatus
          )
        ))
      )
      underTest.mergeAll(
        propertyAnnualSubmission,
        Some(propertyPeriodicSubmission),
        resultFromRepository) shouldBe expectedResult
    }
  }

}
