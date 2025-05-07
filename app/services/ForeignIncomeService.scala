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

package services

import cats.data.EitherT
import cats.implicits.catsSyntaxEitherId
import connectors.IntegrationFrameworkConnector
import models.ITPEnvelope.ITPEnvelope
import models.common.{JourneyContext, Nino, TaxYear}
import models.errors.{ApiServiceError, DataNotFoundError, ServiceError}
import models.request.foreignincome.{ForeignIncomeDividendsWithCountryCode, ForeignIncomeSubmission}
import models.{ForeignIncomeDividendsAnswers, ForeignIncomeDividendsStoreAnswers, ITPEnvelope}
import play.api.Logging
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ForeignIncomeService  @Inject() (
                                        connector: IntegrationFrameworkConnector,
                                        mongoService: MongoJourneyAnswersService
                                      )(implicit ec: ExecutionContext)
  extends Logging {

  def getForeignIncomeSubmission(
      taxYear: TaxYear,
      nino: Nino
    )(implicit hc: HeaderCarrier): ITPEnvelope[ForeignIncomeSubmission] = {
    EitherT(connector.getForeignIncomeSubmission(taxYear, nino))
      .map { dis =>
        logger.debug(s"[getDividendsIncomeSubmission] Dividend income details from IF: $dis")
        dis
      }
      .leftMap { error =>
        logger.error(s"[getDividendsIncomeSubmission] Dividend income details error")
        ApiServiceError(error.status)
      }
      .subflatMap { dividendsIncomeSubmission =>
        dividendsIncomeSubmission.fold[Either[ServiceError, ForeignIncomeSubmission]] {
          logger.error(s"[getDividendsIncomeSubmission] Dividend income details not found in IF, returning empty foreign income submission")
          ForeignIncomeSubmission.emptyForeignIncomeSubmission.asRight[ServiceError]
        } { data =>
          logger.info(s"[getDividendsIncomeSubmission] Dividend income data found: $data")
          data.asRight[ServiceError]
        }
      }
  }

  def createOrUpdateForeignIncomeSubmission(
    taxYear: TaxYear,
    nino: Nino,
    body: ForeignIncomeSubmission
  )(implicit hc: HeaderCarrier): ITPEnvelope[Boolean] =
    body match {
      case ForeignIncomeSubmission(None, None, None, None, None, None) =>
        ITPEnvelope.liftPure(false)
      case _ =>
        EitherT(
          connector.createOrUpdateForeignIncomeSubmission(taxYear, nino, body)
        ).map(_ => true)
          .leftMap(e => ApiServiceError(e.status))
    }

  def deleteForeignIncomeSubmission(
    taxYear: TaxYear,
    nino: Nino
  )(implicit hc: HeaderCarrier
  ): ITPEnvelope[Unit] =
    EitherT(connector.deleteForeignIncomeSubmission(taxYear, nino))
      .leftMap(e => ApiServiceError(e.status))

    def saveForeignIncomeDividends(
      ctx: JourneyContext,
      nino: Nino,
      foreignDividendsWithCountryCode: ForeignIncomeDividendsWithCountryCode
    )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {
      for {
        foreignIncomeSubmission <- getForeignIncomeSubmission(ctx.taxYear, nino)
        _ <- createOrUpdateForeignIncomeSubmission(
          ctx.taxYear,
          nino,
          ForeignIncomeSubmission.fromForeignIncomeDividends(foreignIncomeSubmission, foreignDividendsWithCountryCode)
        )
        res <- mongoService.persistAnswers(
          ctx,
          ForeignIncomeDividendsStoreAnswers(
            foreignDividendsWithCountryCode.foreignIncomeDividends.map { foreignIncomeDividend =>
              ForeignIncomeDividendsAnswers(
                countryCode = foreignIncomeDividend.countryCode,
                foreignTaxDeductedFromDividendIncome = foreignIncomeDividend.foreignTaxDeductedFromDividendIncome
              )
            }
          )
        )
      } yield res
    }

}
