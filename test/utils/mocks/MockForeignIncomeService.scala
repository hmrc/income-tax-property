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
import models.request.foreignincome.ForeignIncomeDividendsWithCountryCode
import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import services.ForeignIncomeService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockForeignIncomeService extends MockFactory {

  protected val mockForeignIncomeService: ForeignIncomeService = mock[ForeignIncomeService]

  def mockSaveForeignIncomeDividendsSection(
                                                 journeyContext: JourneyContext,
                                                 nino: Nino,
                                                 foreignIncomeDividendsWithCountryCode: ForeignIncomeDividendsWithCountryCode,
                                                 result: Either[ServiceError, Boolean]
                                               ):
  CallHandler4[JourneyContext, Nino, ForeignIncomeDividendsWithCountryCode, HeaderCarrier, EitherT[Future, ServiceError, Boolean]] =
    (mockForeignIncomeService
      .saveForeignIncomeDividends(
        _: JourneyContext,
        _: Nino,
        _: ForeignIncomeDividendsWithCountryCode
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, foreignIncomeDividendsWithCountryCode, *)
      .returning(EitherT.fromEither(result))

}
