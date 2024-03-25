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

import enumeratum._

sealed abstract class JourneyName(override val entryName: String) extends EnumEntry {
  override def toString: String = entryName
}

object JourneyName extends Enum[JourneyName] with utils.PlayJsonEnum[JourneyName] {
  val values: IndexedSeq[JourneyName] = findValues

  case object About extends JourneyName("property-about")

  case object Income extends JourneyName("income")

}
