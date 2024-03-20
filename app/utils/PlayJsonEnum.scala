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

package utils

import enumeratum._
import play.api.libs.json._

trait PlayJsonEnum[A <: EnumEntry] { self: Enum[A] =>
  implicit val keyWrites: KeyWrites[A] = EnumFormats.keyWrites(this)

  implicit def contraKeyWrites[K <: A]: KeyWrites[K] = {
    val w = this.keyWrites

    new KeyWrites[K] {
      def writeKey(k: K) = w.writeKey(k)
    }
  }

  implicit val jsonFormat: Format[A]               = EnumFormats.formats(this)
  implicit def contraJsonWrites[B <: A]: Writes[B] = jsonFormat.contramap[B](b => b: A)
}
