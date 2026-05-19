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
import models.domain.FetchedData
import models.errors.ServiceError
import models.request.foreignincome.{ForeignIncomeDividendsWithCountryCode, ForeignIncomeSubmission}
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import services.ForeignIncomeService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockForeignIncomeService extends MockitoSugar {

  protected val mockForeignIncomeService: ForeignIncomeService = mock[ForeignIncomeService]

  def mockGetForeignIncomeSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    result: Either[ServiceError, ForeignIncomeSubmission]
  ): Unit =
    when(mockForeignIncomeService.getForeignIncomeSubmission(any[TaxYear], any[Nino])(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockCreateOrUpdateForeignIncomeSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    body: ForeignIncomeSubmission,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockForeignIncomeService.createOrUpdateForeignIncomeSubmission(any[TaxYear], any[Nino], eqTo(body))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockDeleteForeignIncomeSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    result: Either[ServiceError, Unit]
  ): Unit =
    when(mockForeignIncomeService.deleteForeignIncomeSubmission(any[TaxYear], any[Nino])(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveForeignIncomeDividends(
    ctx: JourneyContext,
    taxableEntityId: Nino,
    foreignDividendsWithCountryCode: ForeignIncomeDividendsWithCountryCode,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockForeignIncomeService.saveForeignIncomeDividends(any[JourneyContext], any[Nino], eqTo(foreignDividendsWithCountryCode))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockGetFetchedIncomeDataMerged(
    ctx: JourneyContext,
    taxableEntityId: Nino,
    result: Either[ServiceError, FetchedData]
  ): Unit =
    when(mockForeignIncomeService.getFetchedIncomeDataMerged(eqTo(ctx), any[Nino])(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))
}
