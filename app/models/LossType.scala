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


sealed trait LossType

object LossType extends Enumerable.Implicits {
  case object UKProperty extends WithName("uk-property") with LossType
  case object SelfEmployment extends WithName("self-employment") with LossType
  case object UKPropertyFhl extends WithName("uk-property-fhl") with LossType
  case object ForeignPropertyFhlEaa extends WithName("foreign-property-fhl-eea") with LossType
  case object ForeignProperty extends WithName("foreign-property") with LossType

  val values: Seq[LossType] = Seq(
    UKProperty,
    SelfEmployment,
    UKPropertyFhl,
    ForeignPropertyFhlEaa,
    ForeignProperty
  )

  implicit val enumerable: Enumerable[LossType] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
