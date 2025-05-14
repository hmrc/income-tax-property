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


sealed trait HipLossType

object HipLossType extends Enumerable.Implicits {
  case object UKProperty extends WithName("02") with HipLossType
  case object ForeignPropertyFhlEaa extends WithName("03") with HipLossType
  case object UKPropertyFhl extends WithName("04") with HipLossType
  case object ForeignProperty extends WithName("15") with HipLossType

  val values: Seq[HipLossType] = Seq(
    UKProperty,
    ForeignPropertyFhlEaa,
    UKPropertyFhl,
    ForeignProperty
  )

  implicit val enumerable: Enumerable[HipLossType] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
