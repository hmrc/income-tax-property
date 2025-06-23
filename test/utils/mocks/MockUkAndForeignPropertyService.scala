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

package utils.mocks

import cats.data.EitherT
import models.common._
import models.errors.ServiceError
import models.request.ukandforeign.UkAndForeignAbout
import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite
import services.UkAndForeignPropertyService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockUkAndForeignPropertyService extends MockFactory { _: TestSuite =>

  protected val mockUkAndForeignPropertyService: UkAndForeignPropertyService = mock[UkAndForeignPropertyService]

  def mockSaveUkAndForeignPropertyAbout(
    journeyContext: JourneyContext,
    ukAndForeignPropertiesInformation: UkAndForeignAbout,
    result: Either[ServiceError, Boolean]
  ): CallHandler2[JourneyContext, UkAndForeignAbout, EitherT[Future, ServiceError, Boolean]] =

    (mockUkAndForeignPropertyService
      .saveUkAndForeignPropertyAbout(
        _: JourneyContext,
        _: UkAndForeignAbout
      ))
      .expects(journeyContext, ukAndForeignPropertiesInformation)
      .returning(EitherT.fromEither(result))

}
