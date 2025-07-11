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


sealed trait TotalPropertyIncome

object TotalPropertyIncome extends Enumerable.Implicits {

  final case object LessThan extends WithName("lessThan") with TotalPropertyIncome
  final case object Maximum extends WithName("maximum") with TotalPropertyIncome

  val values: Seq[TotalPropertyIncome] = Seq(
    LessThan, Maximum
  )

  implicit val enumerable: Enumerable[TotalPropertyIncome] =
    Enumerable(values.map(v => v.toString -> v): _*)
}

