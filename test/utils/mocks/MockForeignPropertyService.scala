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

package utils.mocks

import cats.data.EitherT
import models.common._
import models.errors.ServiceError
import models.request.foreign._
import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.request.foreign.expenses.ForeignPropertyExpensesWithCountryCode
import models.responses.PeriodicSubmissionId
import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import services.ForeignPropertyService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockForeignPropertyService extends MockFactory {

  protected val mockForeignPropertyService: ForeignPropertyService = mock[ForeignPropertyService]

  def mockSaveSelectCountrySection(
    journeyContext: JourneyContext,
    foreignPropertiesInformation: ForeignPropertySelectCountry,
    result: Either[ServiceError, Boolean]
  ): CallHandler3[JourneyContext, ForeignPropertySelectCountry, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockForeignPropertyService
      .saveForeignPropertySelectCountry(
        _: JourneyContext,
        _: ForeignPropertySelectCountry
      )(_: HeaderCarrier))
      .expects(journeyContext, foreignPropertiesInformation, *)
      .returning(EitherT.fromEither(result))

  def mockSaveForeignPropertyTax(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertyTaxWithCountryCode: ForeignPropertyTaxWithCountryCode,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler4[JourneyContext, Nino, ForeignPropertyTaxWithCountryCode, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Option[PeriodicSubmissionId]
  ]] =
    (mockForeignPropertyService
      .saveForeignPropertyTax(
        _: JourneyContext,
        _: Nino,
        _: ForeignPropertyTaxWithCountryCode
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, foreignPropertyTaxWithCountryCode, *)
      .returning(EitherT.fromEither(result))

  def mockSaveForeignPropertyExpenses(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertyExpenses: ForeignPropertyExpensesWithCountryCode,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler4[JourneyContext, Nino, ForeignPropertyExpensesWithCountryCode, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Option[PeriodicSubmissionId]
  ]] =
    (mockForeignPropertyService
      .saveForeignPropertyExpenses(
        _: JourneyContext,
        _: Nino,
        _: ForeignPropertyExpensesWithCountryCode
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, foreignPropertyExpenses, *)
      .returning(EitherT.fromEither(result))

  def mockSaveForeignIncomeSection(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignIncome: ForeignIncome,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler4[JourneyContext, Nino, ForeignIncome, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Option[PeriodicSubmissionId]
  ]] =
    (mockForeignPropertyService
      .saveForeignIncome(
        _: JourneyContext,
        _: Nino,
        _: ForeignIncome
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, foreignIncome, *)
      .returning(EitherT.fromEither(result))

  def mockSaveForeignPropertyAllowancesSection(
                                                journeyContext: JourneyContext,
                                                nino: Nino,
                                                foreignPropertyAllowances: ForeignPropertyAllowancesWithCountryCode,
                                                result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler4[JourneyContext, Nino, ForeignPropertyAllowancesWithCountryCode, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Option[PeriodicSubmissionId]
  ]] =
    (mockForeignPropertyService
      .saveForeignPropertyAllowances(
        _: JourneyContext,
        _: Nino,
        _: ForeignPropertyAllowancesWithCountryCode
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, foreignPropertyAllowances, *)
      .returning(EitherT.fromEither(result))

}
