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
import models.request.foreign.adjustments.ForeignPropertyAdjustmentsWithCountryCode
import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.request.foreign.expenses.ForeignPropertyExpensesWithCountryCode
import models.request.foreign.sba.ForeignPropertySbaWithCountryCode
import models.responses.PeriodicSubmissionId
import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite
import services.ForeignPropertyService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockForeignPropertyService extends MockFactory { _: TestSuite =>

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
    foreignIncome: ForeignIncomeWithCountryCode,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler4[JourneyContext, Nino, ForeignIncomeWithCountryCode, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Option[PeriodicSubmissionId]
  ]] =
    (mockForeignPropertyService
      .saveForeignIncome(
        _: JourneyContext,
        _: Nino,
        _: ForeignIncomeWithCountryCode
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, foreignIncome, *)
      .returning(EitherT.fromEither(result))

  def mockSaveForeignPropertyAllowancesSection(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertyAllowancesWithCountryCode: ForeignPropertyAllowancesWithCountryCode,
    result: Either[ServiceError, Boolean]
  ): CallHandler4[JourneyContext, Nino, ForeignPropertyAllowancesWithCountryCode, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockForeignPropertyService
      .saveForeignPropertyAllowances(
        _: JourneyContext,
        _: Nino,
        _: ForeignPropertyAllowancesWithCountryCode
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, foreignPropertyAllowancesWithCountryCode, *)
      .returning(EitherT.fromEither(result))


  def mockSaveForeignPropertyAdjustmentsSection(
                                                 journeyContext: JourneyContext,
                                                 nino: Nino,
                                                 foreignPropertyAdjustmentsWithCountryCode: ForeignPropertyAdjustmentsWithCountryCode,
                                                 result: Either[ServiceError, Boolean]
                                              ):
  CallHandler4[JourneyContext, Nino, ForeignPropertyAdjustmentsWithCountryCode, HeaderCarrier, EitherT[Future, ServiceError, Boolean]] =
    (mockForeignPropertyService
      .saveForeignPropertyAdjustments(
        _: JourneyContext,
        _: Nino,
        _: ForeignPropertyAdjustmentsWithCountryCode
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, foreignPropertyAdjustmentsWithCountryCode, *)
      .returning(EitherT.fromEither(result))

  def mockSaveForeignPropertySbaSection(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertySbaWithCountryCode: ForeignPropertySbaWithCountryCode,
    result: Either[ServiceError, Boolean]
  ): CallHandler4[JourneyContext, Nino, ForeignPropertySbaWithCountryCode, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] = {
    val compareSba: ForeignPropertySbaWithCountryCode => Boolean = other =>
      foreignPropertySbaWithCountryCode.claimStructureBuildingAllowance == other.claimStructureBuildingAllowance &&
      foreignPropertySbaWithCountryCode.countryCode == other.countryCode &&
        foreignPropertySbaWithCountryCode.allowances.map(_.toSeq) == other.allowances.map(_.toSeq)
    (mockForeignPropertyService
      .saveForeignPropertySba(
        _: JourneyContext,
        _: Nino,
        _: ForeignPropertySbaWithCountryCode
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, argThat[ForeignPropertySbaWithCountryCode](compareSba), *)
      .returning(EitherT.fromEither(result))
  }

}
