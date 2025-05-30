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

import models.request.foreign.WithName


sealed trait IncomeSourceType

object IncomeSourceType extends Enumerable.Implicits {
  case object UKPropertyOther extends WithName("02") with IncomeSourceType
  case object FHLPropertyEEA extends WithName("03") with IncomeSourceType
  case object UKPropertyFHL extends WithName("04") with IncomeSourceType
  case object ForeignProperty extends WithName("15") with IncomeSourceType

  val values: Seq[IncomeSourceType] = Seq(
    UKPropertyOther,
    FHLPropertyEEA,
    UKPropertyFHL,
    ForeignProperty
  )

  implicit val enumerable: Enumerable[IncomeSourceType] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
