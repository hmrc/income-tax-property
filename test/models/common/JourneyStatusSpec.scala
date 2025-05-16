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

package models.common

import models.common.JourneyStatus._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsPath, JsString, JsSuccess, Json}

class JourneyStatusSpec extends AnyFreeSpec with Matchers {

  "JourneyStatus" - {
    "must contain the correct values" in {
      JourneyStatus.values mustEqual IndexedSeq[JourneyStatus](
        NotStarted,
        InProgress,
        Completed,
        CheckNow,
        UnderMaintenance
      )
    }

    "must parse each element as JSON successfully" - {

      JourneyStatus.values.foreach { journeyStatus =>
        s"for ${journeyStatus.toString}" - {

          "serialize to JSON" in {
            Json.toJson(journeyStatus) mustBe JsString(journeyStatus.toString)
          }

          "deserialize from JSON" in {
            JsString(journeyStatus.toString).validate[JourneyStatus] mustBe JsSuccess(journeyStatus, JsPath())
          }
        }
      }
    }
  }
}
