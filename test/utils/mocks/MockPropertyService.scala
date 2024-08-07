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
import models.errors.ServiceError
import models.request._
import models.request.esba.EsbaInfo
import models.request.sba.SbaInfo
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import models.{ITPEnvelope, PropertyPeriodicSubmissionResponse, RentalsAndRaRAbout}
import org.scalamock.handlers._
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.Writes
import services.PropertyService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

trait MockPropertyService extends MockFactory {

  protected val mockPropertyService: PropertyService = mock[PropertyService]

  def mockGetAllPeriodicSubmissions(
    taxYear: Int,
    taxableEntityId: String,
    incomeSourceId: String,
    result: Either[ServiceError, PropertyPeriodicSubmissionResponse]
  ): CallHandler4[Int, String, String, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    PropertyPeriodicSubmissionResponse
  ]] =
    (mockPropertyService
      .getPropertyPeriodicSubmissions(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(EitherT.fromEither(result))

  def mockGetCurrentPeriodicSubmission(
    taxYear: Int,
    nino: String,
    taxableEntityId: String,
    incomeSourceId: String,
    result: Either[ServiceError, Option[PropertyPeriodicSubmission]]
  ): CallHandler4[Int, String, String, HeaderCarrier, ITPEnvelope[Option[PropertyPeriodicSubmission]]] =
    (mockPropertyService
      .getCurrentPeriodicSubmission(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, nino, incomeSourceId, *)
      .returning(EitherT.fromEither(result))

  def mockGetAnnualSubmission(
    taxYear: Int,
    taxableEntityId: String,
    incomeSourceId: String,
    result: Either[ServiceError, PropertyAnnualSubmission]
  ): CallHandler4[Int, String, String, HeaderCarrier, EitherT[Future, ServiceError, PropertyAnnualSubmission]] =
    (mockPropertyService
      .getPropertyAnnualSubmission(_: Int, _: String, _: String)(_: HeaderCarrier))
      .expects(taxYear, taxableEntityId, incomeSourceId, *)
      .returning(EitherT.fromEither(result))

  def mockCreatePeriodicSubmissions(
    taxableEntityId: String,
    incomeSourceId: String,
    taxYear: Int,
    body: CreatePropertyPeriodicSubmissionRequest,
    result: Either[ServiceError, Option[PeriodicSubmissionId]]
  ): CallHandler5[String, String, Int, CreatePropertyPeriodicSubmissionRequest, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Option[PeriodicSubmissionId]
  ]] =
    (mockPropertyService
      .createPeriodicSubmission(_: String, _: String, _: Int, _: CreatePropertyPeriodicSubmissionRequest)(
        _: HeaderCarrier
      ))
      .expects(taxableEntityId, incomeSourceId, taxYear, *, *)
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

  def mockSaveSbas(
    ctx: JourneyContext,
    nino: Nino,
    sbaInfo: SbaInfo,
    result: Either[ServiceError, Unit]
  ): CallHandler4[JourneyContext, Nino, SbaInfo, HeaderCarrier, ITPEnvelope[Unit]] =
    (mockPropertyService
      .saveSbas(
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

  def mockUpdatePeriodicSubmissions(
    taxableEntityId: String,
    incomeSourceId: String,
    taxYear: Int,
    submissionId: String,
    body: UpdatePropertyPeriodicSubmissionRequest,
    result: Either[ServiceError, String]
  ): CallHandler6[String, String, Int, String, UpdatePropertyPeriodicSubmissionRequest, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    String
  ]] =
    (mockPropertyService
      .updatePeriodicSubmission(_: String, _: String, _: Int, _: String, _: UpdatePropertyPeriodicSubmissionRequest)(
        _: HeaderCarrier
      ))
      .expects(taxableEntityId, incomeSourceId, taxYear, submissionId, body, *)
      .returning(EitherT.fromEither(result))

  def mockDeleteAnnualSubmission(
    incomeSourceId: String,
    taxableEntityId: String,
    taxYear: Int,
    result: Either[ServiceError, Unit]
  ): CallHandler4[String, String, Int, HeaderCarrier, EitherT[Future, ServiceError, Unit]] =
    (mockPropertyService
      .deletePropertyAnnualSubmission(_: String, _: String, _: Int)(_: HeaderCarrier))
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
    result: Either[ServiceError, FetchedPropertyData]
  ): CallHandler5[JourneyContext, Nino, String, ExecutionContext, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    FetchedPropertyData
  ]] = {
    val ctx = JourneyContext(taxYear, incomeSourceId, mtdItId, JourneyName.AllJourneys)
    (mockPropertyService
      .getFetchedPropertyDataMerged(_: JourneyContext, _: Nino, _: String)(_: ExecutionContext, _: HeaderCarrier))
      .expects(ctx, *, incomeSourceId.value, *, *)
      .returning(ITPEnvelope.liftEither(result))
  }

  def mockPersistAnswers[A](
    ctx: JourneyContext,
    answers: A
  ): CallHandler3[JourneyContext, A, Writes[A], EitherT[Future, ServiceError, Boolean]] =
    (mockPropertyService
      .persistAnswers(_: JourneyContext, _: A)(_: Writes[A]))
      .expects(*, *, *)
      .returning(EitherT.pure(true))

  def mockSavePropertyRentalAllowances[A](
    ctx: JourneyContextWithNino,
    answers: RentalAllowances
  ): CallHandler3[JourneyContextWithNino, RentalAllowances, HeaderCarrier, EitherT[Future, ServiceError, Boolean]] =
    (mockPropertyService
      .savePropertyRentalAllowances(_: JourneyContextWithNino, _: RentalAllowances)(_: HeaderCarrier))
      .expects(*, *, *)
      .returning(EitherT.pure(true))

  def mockSavePropertyRentalAdjustments[A](
    journeyContextWithNino: JourneyContextWithNino,
    propertyRentalAdjustments: PropertyRentalAdjustments
  ): CallHandler3[JourneyContextWithNino, PropertyRentalAdjustments, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockPropertyService
      .savePropertyRentalAdjustments(_: JourneyContextWithNino, _: PropertyRentalAdjustments)(_: HeaderCarrier))
      .expects(*, *, *)
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
