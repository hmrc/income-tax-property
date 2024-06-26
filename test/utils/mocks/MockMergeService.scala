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

package utils.mocks

import models.domain.JourneyAnswers
import models.responses._
import services.MergeService

import org.scalamock.scalatest.MockFactory

trait MockMergeService extends MockFactory {
  protected val mergeService = mock[MergeService]

  def mockMergeServiceMergeAll(returnValue: FetchedPropertyData) =
    (mergeService
      .mergeAll(_: PropertyAnnualSubmission, _: Option[PropertyPeriodicSubmission], _: Map[String, JourneyAnswers]))
      .expects(*, *, *)
      .returning(
        returnValue
      )
}
