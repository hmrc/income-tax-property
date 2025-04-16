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
import play.api.libs.json.{JsPath, JsSuccess, Json}

class JourneyStatusSpec extends AnyFreeSpec with Matchers {

  "Completed" - {

    "must parse to and from json" in {
      val underTest = Json.toJson(Completed)

      underTest.toString() mustBe s"\"$Completed\""
      underTest.validate[JourneyStatus] mustBe JsSuccess(Completed, JsPath())
    }
  }

  "InProgress" - {

    "must parse to and from json" in {
      val underTest = Json.toJson(InProgress)

      underTest.toString() mustBe s"\"$InProgress\""
      underTest.validate[JourneyStatus] mustBe JsSuccess(InProgress, JsPath())
    }
  }

  "CheckNow" - {

    "must parse to and from json" in {
      val underTest = Json.toJson(CheckNow)

      underTest.toString() mustBe s"\"$CheckNow\""
      underTest.validate[JourneyStatus] mustBe JsSuccess(CheckNow, JsPath())
    }
  }

  "NotStarted" - {

    "must parse to and from json" in {
      val underTest = Json.toJson(NotStarted)

      underTest.toString() mustBe s"\"$NotStarted\""
      underTest.validate[JourneyStatus] mustBe JsSuccess(NotStarted, JsPath())
    }
  }

  "UnderMaintenance" - {

    "must parse to and from json" in {
      val underTest = Json.toJson(UnderMaintenance)

      underTest.toString() mustBe s"\"$UnderMaintenance\""
      underTest.validate[JourneyStatus] mustBe JsSuccess(UnderMaintenance, JsPath())
    }
  }

}
