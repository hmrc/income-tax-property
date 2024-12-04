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
import models.request.foreign.{ForeignPropertySelectCountry, ForeignPropertyTaxWithCountryCode}
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
    nino: Nino,
    foreignPropertiesInformation: ForeignPropertySelectCountry,
    result: Either[ServiceError, Boolean]
  ): CallHandler4[JourneyContext, Nino, ForeignPropertySelectCountry, HeaderCarrier, EitherT[
    Future,
    ServiceError,
    Boolean
  ]] =
    (mockForeignPropertyService
      .saveForeignPropertySelectCountry(
        _: JourneyContext,
        _: Nino,
        _: ForeignPropertySelectCountry
      )(_: HeaderCarrier))
      .expects(journeyContext, nino, foreignPropertiesInformation, *)
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

}
