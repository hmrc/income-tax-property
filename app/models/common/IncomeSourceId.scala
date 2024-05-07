/*
 * Copyright 2023 HM Revenue & Customs
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

package models.common

import play.api.libs.json.{Format, Json}
import play.api.mvc.PathBindable

final case class IncomeSourceId(value: String) extends AnyVal {
  override def toString: String = value
}

object IncomeSourceId {
  implicit def pathBindable(implicit strBinder: PathBindable[String]): PathBindable[IncomeSourceId] =
    new PathBindable[IncomeSourceId] {

      override def bind(key: String, value: String): Either[String, IncomeSourceId] =
        strBinder.bind(key, value).map(IncomeSourceId.apply)

      override def unbind(key: String, businessId: IncomeSourceId): String =
        strBinder.unbind(key, businessId.value)
    }

  implicit val format: Format[IncomeSourceId] = Json.valueFormat[IncomeSourceId]

}
