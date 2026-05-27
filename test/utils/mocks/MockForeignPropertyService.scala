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
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import services.ForeignPropertyService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockForeignPropertyService extends MockitoSugar {

  protected val mockForeignPropertyService: ForeignPropertyService = mock[ForeignPropertyService]

  def mockSaveSelectCountrySection(
    journeyContext: JourneyContext,
    foreignPropertiesInformation: ForeignPropertySelectCountry,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockForeignPropertyService.saveForeignPropertySelectCountry(eqTo(journeyContext), eqTo(foreignPropertiesInformation))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveForeignPropertyTax(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertyTaxWithCountryCode: ForeignPropertyTaxWithCountryCode,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockForeignPropertyService.saveForeignPropertyTax(eqTo(journeyContext), any[Nino], eqTo(foreignPropertyTaxWithCountryCode))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveForeignPropertyExpenses(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertyExpenses: ForeignPropertyExpensesWithCountryCode,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockForeignPropertyService.saveForeignPropertyExpenses(eqTo(journeyContext), any[Nino], eqTo(foreignPropertyExpenses))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveForeignIncomeSection(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignIncome: ForeignIncomeWithCountryCode,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockForeignPropertyService.saveForeignIncome(eqTo(journeyContext), any[Nino], eqTo(foreignIncome))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveForeignPropertyAllowancesSection(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertyAllowancesWithCountryCode: ForeignPropertyAllowancesWithCountryCode,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockForeignPropertyService.saveForeignPropertyAllowances(eqTo(journeyContext), any[Nino], eqTo(foreignPropertyAllowancesWithCountryCode))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveForeignPropertyAdjustmentsSection(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertyAdjustmentsWithCountryCode: ForeignPropertyAdjustmentsWithCountryCode,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockForeignPropertyService.saveForeignPropertyAdjustments(eqTo(journeyContext), any[Nino], eqTo(foreignPropertyAdjustmentsWithCountryCode))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveForeignPropertySbaSection(
    journeyContext: JourneyContext,
    nino: Nino,
    foreignPropertySbaWithCountryCode: ForeignPropertySbaWithCountryCode,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockForeignPropertyService.saveForeignPropertySba(
      eqTo(journeyContext),
      any[Nino],
      eqTo(foreignPropertySbaWithCountryCode)
    )(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockDeleteForeignPropertyAnswers(
    journeyContext: JourneyContext,
    deleteJourneyAnswers: DeleteJourneyAnswers,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockForeignPropertyService.deleteForeignPropertyAnswers(eqTo(journeyContext), eqTo(deleteJourneyAnswers)))
      .thenReturn(EitherT.fromEither[Future](result))

}
