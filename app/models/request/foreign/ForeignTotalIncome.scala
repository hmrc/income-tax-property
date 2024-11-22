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

package models.request.foreign

import models.Enumerable
sealed trait ForeignTotalIncome

object ForeignTotalIncome extends Enumerable.Implicits {

  case object LessThanOneThousand extends WithName("lessThanOneThousand") with ForeignTotalIncome
  case object OneThousandAndMore extends WithName("oneThousandAndMore") with ForeignTotalIncome

  val values: Seq[ForeignTotalIncome] = Seq(
    LessThanOneThousand, OneThousandAndMore
  )


  implicit val enumerable: Enumerable[ForeignTotalIncome] =
    Enumerable(values.map(v => v.toString -> v): _*)

}