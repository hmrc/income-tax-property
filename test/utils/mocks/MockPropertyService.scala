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
import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite
import services.PropertyService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait MockPropertyService extends MockFactory { _: TestSuite =>

  protected val mockPropertyService: PropertyService = mock[PropertyService]

  def mockGetAllPeriodicSubmissions(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ServiceError, PropertyPeriodicSubmissionResponse]
  ): CallHandler4[TaxYear, Nino, IncomeSourceId, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    PropertyPeriodicSubmissionResponse
  ]] =
    (mockPropertyService
      .getPropertyPeriodicSubmissions(_: TaxYear, _: Nino, _: IncomeSourceId)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(EitherT.fromEither(result))

  def mockGetPeriodicSubmission(
    taxYear: TaxYear,
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ServiceError, Option[PropertyPeriodicSubmission]]
  ): CallHandler4[TaxYear, Nino, IncomeSourceId, HeaderCarrier, ITPEnvelope[Option[PropertyPeriodicSubmission]]] =
    (mockPropertyService
      .getPeriodicSubmission(_: TaxYear, _: Nino, _: IncomeSourceId)(_: HeaderCarrier))
      .expects(taxYear, nino, incomeSourceId, *)
      .returning(EitherT.fromEither(result))

  def mockGetAnnualSubmission(
    taxYear: TaxYear,
    taxableEntityId: Nino,
    incomeSourceId: IncomeSourceId,
    result: Either[ServiceError, PropertyAnnualSubmission]
  ): CallHandler4[TaxYear, Nino, IncomeSourceId, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    PropertyAnnualSubmission
  ]] =
    (mockPropertyService
      .getPropertyAnnualSubmission(_: TaxYear, _: Nino, _: IncomeSourceId)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(EitherT.fromEither(result))

  def mockCreatePeriodicSubmissions(
                                     taxableEntityId: Nino,
                                     incomeSourceId: IncomeSourceId,
                                     taxYear: TaxYear,
                                     body: CreateUKPropertyPeriodicSubmissionRequest,
                                     result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler5[Nino, IncomeSourceId, TaxYear, CreateUKPropertyPeriodicSubmissionRequest, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Option[PeriodicSubmissionId]
  ]] =
    (mockPropertyService
      .createPeriodicSubmission(_: Nino, _: IncomeSourceId, _: TaxYear, _: CreateUKPropertyPeriodicSubmissionRequest)(
        _: HeaderCarrier
      ))
      .expects(taxableEntityId, incomeSourceId, taxYear, *, *)
      .returning(EitherT.fromEither(result))

  def mockSavePropertyAbout(
    ctx: JourneyContext,
    propertyAbout: PropertyAbout,
    result: Either[ServiceError, Boolean]
  ): CallHandler3[JourneyContext, PropertyAbout, HeaderCarrier, EitherT[Future, ServiceError, Boolean]] =
    (mockPropertyService
      .savePropertyAbout(
        _: JourneyContext,
        _: PropertyAbout,
    )(_: HeaderCarrier))
      .expects(ctx, propertyAbout, *)
      .returning(EitherT.fromEither(result))

  def mockSavePropertyRentalAbout(
    ctx: JourneyContext,
    propertyRentalsAbout: PropertyRentalsAbout,
    result: Either[ServiceError, Boolean]
  ): CallHandler3[JourneyContext, PropertyRentalsAbout, HeaderCarrier, EitherT[Future, ServiceError, Boolean]] =
    (mockPropertyService
      .savePropertyRentalAbout(
        _: JourneyContext,
        _: PropertyRentalsAbout,
      )(_: HeaderCarrier))
      .expects(ctx, propertyRentalsAbout, *)
      .returning(EitherT.fromEither(result))

  def mockSaveExpenses(
    ctx: JourneyContext,
    nino: Nino,
    expenses: Expenses,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler4[JourneyContext, Nino, Expenses, HeaderCarrier, EitherT[Future, ServiceError, Option[
    PeriodicSubmissionId
  ]]] =
    (mockPropertyService
      .saveExpenses(
        _: JourneyContext,
        _: Nino,
        _: Expenses
      )(_: HeaderCarrier))
      .expects(ctx, nino, expenses, *)
      .returning(EitherT.fromEither(result))

  def mockSaveEsbas(
    ctx: JourneyContext,
    nino: Nino,
    esbaInfo: EsbaInfo,
    result: Either[ServiceError, Unit]
  ): CallHandler4[JourneyContext, Nino, EsbaInfo, HeaderCarrier, ITPEnvelope[Unit]] =
    (mockPropertyService
      .saveEsbas(
        _: JourneyContext,
        _: Nino,
        _: EsbaInfo
      )(_: HeaderCarrier))
      .expects(ctx, nino, esbaInfo, *)
      .returning(EitherT.fromEither(result))

  def mockSaveSBA(
    ctx: JourneyContext,
    nino: Nino,
    sbaInfo: SbaInfo,
    result: Either[ServiceError, Unit]
  ): CallHandler4[JourneyContext, Nino, SbaInfo, HeaderCarrier, ITPEnvelope[Unit]] =
    (mockPropertyService
      .saveSBA(
        _: JourneyContext,
        _: Nino,
        _: SbaInfo
      )(_: HeaderCarrier))
      .expects(ctx, nino, sbaInfo, *)
      .returning(EitherT.fromEither(result))

  def mockSaveIncome(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
    journeyContext: JourneyContext,
    propertyRentalsIncome: PropertyRentalsIncome,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler4[JourneyContext, Nino, PropertyRentalsIncome, HeaderCarrier, EitherT[Future, ServiceError, Option[
    PeriodicSubmissionId
  ]]] =
    (mockPropertyService
      .saveIncome(
        _: JourneyContext,
        _: Nino,
        _: PropertyRentalsIncome
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, propertyRentalsIncome, *)
      .returning(EitherT.fromEither(result))

  def mockSaveRentalsAndRaRIncome(
    nino: Nino,
    incomeSourceId: IncomeSourceId,
    taxYear: TaxYear,
    journeyContext: JourneyContext,
    rentalsAndRaRIncome: RentalsAndRaRIncome,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler4[JourneyContext, Nino, RentalsAndRaRIncome, HeaderCarrier, EitherT[Future, ServiceError, Option[
    PeriodicSubmissionId
  ]]] =
    (mockPropertyService
      .saveRentalsAndRaRIncome(
        _: JourneyContext,
        _: Nino,
        _: RentalsAndRaRIncome
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, rentalsAndRaRIncome, *)
      .returning(EitherT.fromEither(result))

  def mockUpdatePeriodicSubmissions(
                                     taxableEntityId: Nino,
                                     incomeSourceId: IncomeSourceId,
                                     taxYear: TaxYear,
                                     submissionId: String,
                                     body: UpdateUKPropertyPeriodicSubmissionRequest,
                                     result: Either[ServiceError, String]
  ): CallHandler6[
    Nino,
    IncomeSourceId,
    TaxYear,
    String,
    UpdateUKPropertyPeriodicSubmissionRequest,
    HeaderCarrier,
    EitherT[
      Future,
      ServiceError,
      String
    ]
  ] =
    (mockPropertyService
      .updatePeriodicSubmission(
        _: Nino,
        _: IncomeSourceId,
        _: TaxYear,
        _: String,
        _: UpdateUKPropertyPeriodicSubmissionRequest
      )(
        _: HeaderCarrier
      ))
      .expects(taxableEntityId, incomeSourceId, taxYear, submissionId, body, *)
      .returning(EitherT.fromEither(result))

  def mockDeleteAnnualSubmission(
    incomeSourceId: IncomeSourceId,
    taxableEntityId: Nino,
    taxYear: TaxYear,
    result: Either[ServiceError, Unit]
  ): CallHandler4[IncomeSourceId, Nino, TaxYear, HeaderCarrier, EitherT[Future, ServiceError, Unit]] =
    (mockPropertyService
      .deletePropertyAnnualSubmission(_: IncomeSourceId, _: Nino, _: TaxYear)(_: HeaderCarrier))
      .expects(incomeSourceId, taxableEntityId, taxYear, *)
      .returning(EitherT.fromEither[Future](result))

  def mockCreateOrUpdateAnnualSubmissions(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    nino: Nino,
    body: PropertyAnnualSubmission,
    result: Either[ServiceError, Unit]
  ): CallHandler5[TaxYear, IncomeSourceId, Nino, PropertyAnnualSubmission, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Unit
  ]] =
    (mockPropertyService
      .createOrUpdateAnnualSubmission(_: TaxYear, _: IncomeSourceId, _: Nino, _: PropertyAnnualSubmission)(
        _: HeaderCarrier
      ))
      .expects(taxYear, incomeSourceId, nino, *, *)
      .returning(EitherT.fromEither(result))

  def mockGetFetchedPropertyDataMerged(
    taxYear: TaxYear,
    incomeSourceId: IncomeSourceId,
    mtdItId: Mtditid,
    result: Either[ServiceError, FetchedData]
  ): CallHandler4[JourneyContext, Nino, IncomeSourceId, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    FetchedData
  ]] = {
    val ctx = JourneyContext(taxYear, incomeSourceId, mtdItId, JourneyName.AllJourneys)
    (mockPropertyService
      .getFetchedPropertyDataMerged(_: JourneyContext, _: Nino, _: IncomeSourceId)(_: HeaderCarrier))
      .expects(ctx, *, incomeSourceId, *)
      .returning(ITPEnvelope.liftEither(result))
  }

  def mockSavePropertyRentalAllowances[A](
    ctx: JourneyContext,
    nino: Nino,
    answers: RentalAllowances
  ): CallHandler4[JourneyContext, Nino, RentalAllowances, HeaderCarrier, EitherT[Future, ServiceError, Boolean]] =
    (mockPropertyService
      .savePropertyRentalAllowances(_: JourneyContext, _:Nino, _: RentalAllowances)(_: HeaderCarrier))
      .expects(*, *, *, *)
      .returning(EitherT.pure(true))

  def mockSavePropertyRentalAdjustments[A](
    journeyContext: JourneyContext,
    nino: Nino,
    propertyRentalAdjustments: PropertyRentalAdjustments
  ): CallHandler4[JourneyContext, Nino, PropertyRentalAdjustments, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockPropertyService
      .savePropertyRentalAdjustments(_: JourneyContext, _: Nino, _: PropertyRentalAdjustments)(_: HeaderCarrier))
      .expects(*, *, *, *)
      .returning(EitherT.pure(true))

  def mockSaveUkRentARoomAbout[A](
    journeyContext: JourneyContext,
    nino: Nino,
    ukRaRAbout: RaRAbout,
    result: Boolean
  ): CallHandler4[JourneyContext, Nino, RaRAbout, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockPropertyService
      .saveRaRAbout(_: JourneyContext, _: Nino, _: RaRAbout)(_: HeaderCarrier))
      .expects(journeyContext, nino, ukRaRAbout, *)
      .returning(EitherT.pure(result))

  def mockSaveRentalsAndRentARoomAbout(
    journeyContext: JourneyContext,
    nino: Nino,
    rentalsAndRaRAbout: RentalsAndRaRAbout,
    result: Boolean
  ): CallHandler4[JourneyContext, Nino, RentalsAndRaRAbout, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockPropertyService
      .saveRentalsAndRaRAbout(_: JourneyContext, _: Nino, _: RentalsAndRaRAbout)(_: HeaderCarrier))
      .expects(journeyContext, nino, rentalsAndRaRAbout, *)
      .returning(EitherT.pure(result))

  def mockSaveUkRentARoomAllowances(
                                     journeyContext: JourneyContextWithNino,
                                     ukRaRAllowances: RentARoomAllowances
                                 ): CallHandler3[JourneyContextWithNino, RentARoomAllowances, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockPropertyService
      .saveRentARoomAllowances(_: JourneyContextWithNino, _: RentARoomAllowances)(_: HeaderCarrier))
      .expects(*, *, *)
      .returning(EitherT.pure(true))

  def mockSaveUkRaRExpenses[A](
                                   journeyContext: JourneyContext,
                                   nino: Nino,
                                   raRExpenses: RentARoomExpenses,
                                   result: Either[ServiceError, Option[PeriodicSubmissionId]]
                                 ): CallHandler4[JourneyContext, Nino, RentARoomExpenses, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Option[PeriodicSubmissionId]
  ]] =
    (mockPropertyService
      .saveRaRExpenses(_: JourneyContext, _: Nino, _: RentARoomExpenses)(_: HeaderCarrier))
      .expects(journeyContext, nino, raRExpenses, *)
      .returning(EitherT.fromEither(result))

  def mockSaveUkRaRAdjustments[A](
    journeyContext: JourneyContext,
    nino: Nino,
    raRAdjustments: RaRAdjustments,
    result: Either[ServiceError, Boolean]
  ): CallHandler4[JourneyContext, Nino, RaRAdjustments, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockPropertyService
      .saveRaRAdjustments(_: JourneyContext, _: Nino, _: RaRAdjustments)(_: HeaderCarrier))
      .expects(journeyContext, nino, raRAdjustments, *)
      .returning(EitherT.fromEither(result))
}
