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

package models.request.ukAndForeign

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json._

class ReportIncomeSpec extends AnyWordSpec with Matchers {

  "ReportIncome" should {

    "write and read correctly" in {
      val wantToReportJson = Json.toJson(ReportIncome.WantToReport.toString)
      val doNotWantToReportJson = Json.toJson(ReportIncome.DoNoWantToReport.toString)

      wantToReportJson shouldBe JsString("wantToReport")
      doNotWantToReportJson shouldBe JsString("doNotWantToReport")

      wantToReportJson.as[ReportIncome] shouldBe ReportIncome.WantToReport
      doNotWantToReportJson.as[ReportIncome] shouldBe ReportIncome.DoNoWantToReport
    }

    "return the correct values" in {
      ReportIncome.values should contain theSameElementsAs Seq(
        ReportIncome.WantToReport,
        ReportIncome.DoNoWantToReport
      )
    }

    "be enumerable" in {
      ReportIncome.enumerable.withName("wantToReport") shouldBe Some(ReportIncome.WantToReport)
      ReportIncome.enumerable.withName("doNotWantToReport") shouldBe Some(ReportIncome.DoNoWantToReport)
      ReportIncome.enumerable.withName("invalid") shouldBe None
    }
  }
}
