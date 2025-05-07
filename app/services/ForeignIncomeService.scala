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
import play.api.Logging
import repositories.MongoJourneyAnswersRepository
import connectors.IntegrationFrameworkConnector
import models.{ITPEnvelope, ForeignIncomeDividendsStoreAnswers, ForeignIncomeDividendsAnswers}
import models.ITPEnvelope.ITPEnvelope
import models.common.{Nino, TaxYear, JourneyContext}
import models.errors.{ServiceError, ApiServiceError, RepositoryError}
import models.request.foreignIncome.ForeignIncomeDividendsWithCountryCode
import uk.gov.hmrc.http.HeaderCarrier
import models.request.foreignIncome.{ForeignIncomeSubmissionDividends, ForeignIncomeSubmission}
import play.api.libs.json.{Writes, Json}

import scala.concurrent.{ExecutionContext, Future}
import javax.inject.Inject

class ForeignIncomeService  @Inject() (
                                        connector: IntegrationFrameworkConnector,
                                        repository: MongoJourneyAnswersRepository
                                      )(implicit ec: ExecutionContext)
  extends Logging {

  private def persistForeignIncomeAnswers[A](ctx: JourneyContext, answers: A, countryCode: String)(implicit
                                                                                             writes: Writes[A]
  ): EitherT[Future, ServiceError, Boolean] =
    EitherT(
      repository.foreignUpsertAnswers(ctx, Json.toJson(answers), countryCode).map {
        case false => RepositoryError.asLeft[Boolean]
        case true => true.asRight[ServiceError]
      }
    )

  def createOrUpdateForeignDividendsSubmission(
                                                taxYear: TaxYear,
                                                nino: Nino,
                                                body: ForeignIncomeSubmissionDividends
                                              )(implicit hc: HeaderCarrier): ITPEnvelope[Boolean] =
    body match {
      case ForeignIncomeSubmissionDividends(None) =>
        ITPEnvelope.liftPure(false)
      case _ =>
        EitherT(
          connector.createOrUpdateForeignDividendsSubmission(taxYear, nino, body)
        ).map(_ => true)
          .leftMap(e => ApiServiceError(e.status))
    }

    def saveForeignIncomeDividends(
                                  journeyContext: JourneyContext,
                                  nino: Nino,
                                  foreignDividendsWithCountryCode: ForeignIncomeDividendsWithCountryCode
                                )(implicit hc: HeaderCarrier): EitherT[Future, ServiceError, Boolean] = {
      for {
        _ <- {
          val foreignDividendsSubmission = ForeignIncomeSubmission.fromForeignIncomeDividends(foreignDividendsWithCountryCode)
          createOrUpdateForeignDividendsSubmission(
            journeyContext.taxYear,
            nino,
            foreignDividendsSubmission
          )
        }
        res <- persistForeignIncomeAnswers(
          journeyContext,
          ForeignIncomeDividendsStoreAnswers(
            List(ForeignIncomeDividendsAnswers(foreignDividendsWithCountryCode.countryCode, foreignDividendsWithCountryCode.foreignTaxCreditRelief))
          ),
          foreignDividendsWithCountryCode.countryCode
        )
      } yield res
    }

}
