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

import models.domain.{FetchedData, JourneyAnswers}
import models.request.foreignincome.ForeignIncomeSubmission
import models.responses._
import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite
import services.MergeService

trait MockMergeService extends MockFactory { _: TestSuite =>
  protected val mergeService: MergeService = mock[MergeService]

  def mockMergeServiceMergeAll(returnValue: FetchedData) =
    (mergeService
      .mergeAll(_: Option[PropertyAnnualSubmission],
        _: Option[PropertyPeriodicSubmission],
        _: Map[String, JourneyAnswers],
        _: Map[String, Map[String, JourneyAnswers]],
        _: Option[ForeignIncomeSubmission],
        _: Map[String, JourneyAnswers]
      ))
      .expects(*, *, *, *, *, *)
      .returning(
        returnValue
      )
}
