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
import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import services.ForeignIncomeService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockForeignIncomeService extends MockFactory {

  protected val mockForeignIncomeService: ForeignIncomeService = mock[ForeignIncomeService]

  def mockGetForeignIncomeSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    result: Either[ServiceError, ForeignIncomeSubmission]
  ): CallHandler3[TaxYear, Nino, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    ForeignIncomeSubmission
  ]] =
    (mockForeignIncomeService
      .getForeignIncomeSubmission(_: TaxYear, _: Nino)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, *)
      .returning(EitherT.fromEither(result))

  def mockCreateOrUpdateForeignIncomeSubmission(
                                      taxYear: TaxYear,
                                      taxableEntityId: Nino,
                                      body: ForeignIncomeSubmission,
                                      result: Either[ServiceError, Boolean]
                                    ): CallHandler4[TaxYear, Nino, ForeignIncomeSubmission, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockForeignIncomeService
      .createOrUpdateForeignIncomeSubmission(_: TaxYear, _: Nino, _: ForeignIncomeSubmission)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, body, *)
      .returning(EitherT.fromEither(result))

  def mockDeleteForeignIncomeSubmission(
                                      taxYear: TaxYear,
                                      taxableEntityId: Nino,
                                      result: Either[ServiceError, Unit]
                                    ): CallHandler3[TaxYear, Nino, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Unit
  ]] =
    (mockForeignIncomeService
      .deleteForeignIncomeSubmission(_: TaxYear, _: Nino)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, *)
      .returning(EitherT.fromEither(result))

  def mockSaveForeignIncomeDividends(
                                      ctx: JourneyContext,
                                      taxableEntityId: Nino,
                                      foreignDividendsWithCountryCode: ForeignIncomeDividendsWithCountryCode,
                                      result: Either[ServiceError, Boolean]
                                    ): CallHandler4[JourneyContext, Nino, ForeignIncomeDividendsWithCountryCode, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockForeignIncomeService
      .saveForeignIncomeDividends(_: JourneyContext, _: Nino, _: ForeignIncomeDividendsWithCountryCode)(_: HeaderCarrier))
      .expects(ctx, taxableEntityId, foreignDividendsWithCountryCode, *)
      .returning(EitherT.fromEither(result))

  def mockGetFetchedIncomeDataMerged(
                                      ctx: JourneyContext,
                                      taxableEntityId: Nino,
                                      result: Either[ServiceError, FetchedData]
                                    ): CallHandler3[JourneyContext, Nino, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    FetchedData
  ]] =
    (mockForeignIncomeService
      .getFetchedIncomeDataMerged(_: JourneyContext, _: Nino)(_: HeaderCarrier))
      .expects(ctx, taxableEntityId, *)
      .returning(EitherT.fromEither(result))
}
