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
import models.ITPEnvelope.ITPEnvelope
import models.common._
import models.domain.FetchedData
import models.errors.ServiceError
import models.request._
import models.request.esba.EsbaInfo
import models.request.sba.SbaInfo
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import models.{ITPEnvelope, PropertyPeriodicSubmissionResponse, RentalsAndRaRAbout}
import org.mockito.ArgumentMatchersSugar
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import services.PropertyService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockPropertyService extends MockitoSugar with ArgumentMatchersSugar {

  protected val mockPropertyService: PropertyService = mock[PropertyService]

  def mockGetAllPeriodicSubmissions(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ServiceError, PropertyPeriodicSubmissionResponse]
  ): Unit =
    when(mockPropertyService.getPropertyPeriodicSubmissions(eqTo(taxYear), eqTo(taxableEntityId), eqTo(incomeSourceId))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockGetPeriodicSubmission(
    taxYear: TaxYear,
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ServiceError, Option[PropertyPeriodicSubmission]]
  ): Unit =
    when(mockPropertyService.getPeriodicSubmission(eqTo(taxYear), eqTo(nino), eqTo(incomeSourceId))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockGetAnnualSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ServiceError, PropertyAnnualSubmission]
  ): Unit =
    when(mockPropertyService.getPropertyAnnualSubmission(eqTo(taxYear), eqTo(taxableEntityId), eqTo(incomeSourceId))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockCreatePeriodicSubmissions(
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
    body: CreateUKPropertyPeriodicSubmissionRequest,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockPropertyService.createPeriodicSubmission(eqTo(taxableEntityId), eqTo(incomeSourceId), eqTo(taxYear), any[CreateUKPropertyPeriodicSubmissionRequest])(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSavePropertyAbout(
    ctx: JourneyContext,
    propertyAbout: PropertyAbout,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockPropertyService.savePropertyAbout(eqTo(ctx), eqTo(propertyAbout))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSavePropertyRentalAbout(
    ctx: JourneyContext,
    propertyRentalsAbout: PropertyRentalsAbout,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockPropertyService.savePropertyRentalAbout(eqTo(ctx), eqTo(propertyRentalsAbout))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveExpenses(
    ctx: JourneyContext,
    nino: Nino,
    expenses: Expenses,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockPropertyService.saveExpenses(eqTo(ctx), eqTo(nino), eqTo(expenses))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveEsbas(
    ctx: JourneyContext,
    nino: Nino,
    esbaInfo: EsbaInfo,
    result: Either[ServiceError, Unit]
  ): Unit =
    when(mockPropertyService.saveEsbas(eqTo(ctx), eqTo(nino), eqTo(esbaInfo))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveSBA(
    ctx: JourneyContext,
    nino: Nino,
    sbaInfo: SbaInfo,
    result: Either[ServiceError, Unit]
  ): Unit =
    when(mockPropertyService.saveSBA(eqTo(ctx), eqTo(nino), eqTo(sbaInfo))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveIncome(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
    journeyContext: JourneyContext,
    propertyRentalsIncome: PropertyRentalsIncome,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockPropertyService.saveIncome(eqTo(journeyContext), eqTo(nino), eqTo(propertyRentalsIncome))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveRentalsAndRaRIncome(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
    journeyContext: JourneyContext,
    rentalsAndRaRIncome: RentalsAndRaRIncome,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockPropertyService.saveRentalsAndRaRIncome(eqTo(journeyContext), eqTo(nino), eqTo(rentalsAndRaRIncome))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockUpdatePeriodicSubmissions(
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
    submissionId: String,
    body: UpdateUKPropertyPeriodicSubmissionRequest,
    result: Either[ServiceError, String]
  ): Unit =
    when(mockPropertyService.updatePeriodicSubmission(eqTo(taxableEntityId), eqTo(incomeSourceId), eqTo(taxYear), eqTo(submissionId), eqTo(body))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockDeleteAnnualSubmission(
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    taxYear: TaxYear,
    result: Either[ServiceError, Unit]
  ): Unit =
    when(mockPropertyService.deletePropertyAnnualSubmission(eqTo(incomeSourceId), eqTo(taxableEntityId), eqTo(taxYear))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockCreateOrUpdateAnnualSubmissions(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino,
    body: PropertyAnnualSubmission,
    result: Either[ServiceError, Unit]
  ): Unit =
    when(mockPropertyService.createOrUpdateAnnualSubmission(eqTo(taxYear), eqTo(incomeSourceId), eqTo(nino), any[PropertyAnnualSubmission])(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockGetFetchedPropertyDataMerged(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    mtdItId: Mtditid,
    result: Either[ServiceError, FetchedData]
  ): Unit = {
    val ctx = JourneyContext(taxYear, incomeSourceId, mtdItId, JourneyName.AllJourneys)
    when(mockPropertyService.getFetchedPropertyDataMerged(eqTo(ctx), any[Nino], eqTo(incomeSourceId))(any[HeaderCarrier]))
      .thenReturn(ITPEnvelope.liftEither(result))
  }

  def mockSavePropertyRentalAllowances[A](
    ctx: JourneyContext,
    nino: Nino,
    answers: RentalAllowances
  ): Unit =
    when(mockPropertyService.savePropertyRentalAllowances(any(), any(), any())(any()))
      .thenReturn(EitherT.pure[Future, ServiceError](true))

  def mockSavePropertyRentalAdjustments[A](
    journeyContext: JourneyContext,
    nino: Nino,
    propertyRentalAdjustments: PropertyRentalAdjustments
  ): Unit =
    when(mockPropertyService.savePropertyRentalAdjustments(any(), any(), any())(any()))
      .thenReturn(EitherT.pure[Future, ServiceError](true))

  def mockSaveUkRentARoomAbout[A](
    journeyContext: JourneyContext,
    nino: Nino,
    ukRaRAbout: RaRAbout,
    result: Boolean
  ): Unit =
    when(mockPropertyService.saveRaRAbout(eqTo(journeyContext), eqTo(nino), eqTo(ukRaRAbout))(any[HeaderCarrier]))
      .thenReturn(EitherT.pure[Future, ServiceError](result))

  def mockSaveRentalsAndRentARoomAbout(
    journeyContext: JourneyContext,
    nino: Nino,
    rentalsAndRaRAbout: RentalsAndRaRAbout,
    result: Boolean
  ): Unit =
    when(mockPropertyService.saveRentalsAndRaRAbout(eqTo(journeyContext), eqTo(nino), eqTo(rentalsAndRaRAbout))(any[HeaderCarrier]))
      .thenReturn(EitherT.pure[Future, ServiceError](result))

  def mockSaveUkRentARoomAllowances(
    journeyContext: JourneyContextWithNino,
    ukRaRAllowances: RentARoomAllowances
  ): Unit =
    when(mockPropertyService.saveRentARoomAllowances(any(), any())(any()))
      .thenReturn(EitherT.pure[Future, ServiceError](true))

  def mockSaveUkRaRExpenses[A](
    journeyContext: JourneyContext,
    nino: Nino,
    raRExpenses: RentARoomExpenses,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): Unit =
    when(mockPropertyService.saveRaRExpenses(eqTo(journeyContext), eqTo(nino), eqTo(raRExpenses))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))

  def mockSaveUkRaRAdjustments[A](
    journeyContext: JourneyContext,
    nino: Nino,
    raRAdjustments: RaRAdjustments,
    result: Either[ServiceError, Boolean]
  ): Unit =
    when(mockPropertyService.saveRaRAdjustments(eqTo(journeyContext), eqTo(nino), eqTo(raRAdjustments))(any[HeaderCarrier]))
      .thenReturn(EitherT.fromEither[Future](result))
}
