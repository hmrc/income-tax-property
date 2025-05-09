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

package models.request.ukandforeign

import models.Enumerable
import models.request.foreign.WithName

sealed trait ReportIncome

object ReportIncome extends Enumerable.Implicits {

  case object WantToReport extends WithName("wantToReport") with ReportIncome
  case object DoNoWantToReport extends WithName("doNotWantToReport") with ReportIncome

  val values: Seq[ReportIncome] = Seq(
    WantToReport, DoNoWantToReport
  )

  implicit val enumerable: Enumerable[ReportIncome] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
