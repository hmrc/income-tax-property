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

package uk.gov.hmrc.incometaxproperty.services

import play.api.libs.json.{JsValue, Json, Writes}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.incometaxproperty.connectors.IntegrationFrameworkConnector
import uk.gov.hmrc.incometaxproperty.models.PropertyPeriodicSubmissionResponse
import uk.gov.hmrc.incometaxproperty.models.common.JourneyContext
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiServiceError, DataNotFoundError, ServiceError}
import uk.gov.hmrc.incometaxproperty.models.responses.{PeriodicSubmissionId, PeriodicSubmissionIdModel, PropertyAnnualSubmission, PropertyPeriodicSubmission}
import uk.gov.hmrc.incometaxproperty.repositories.MongoJourneyAnswersRepository

import java.time.Period
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PropertyService @Inject()(connector: IntegrationFrameworkConnector, repository: MongoJourneyAnswersRepository)
                               (implicit ec: ExecutionContext) {

  def getPropertyPeriodicSubmissions(taxYear: Int,
                                     taxableEntityId: String,
                                     incomeSourceId: String)
                                    (implicit hc: HeaderCarrier): Future[Either[ServiceError, PropertyPeriodicSubmissionResponse]] = {
    connector.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId).flatMap {
      case Left(error) => Future.successful(Left(ApiServiceError(error.status)))
      case Right(periodicSubmissionIds) =>
        val propertyPeriodicSubmissions = getPropertySubmissions(taxYear, taxableEntityId, incomeSourceId, periodicSubmissionIds)
        propertyPeriodicSubmissions.map(transformToResponse)
    }
  }

  def getPropertyAnnualSubmission(taxYear: Int,
                                  taxableEntityId: String,
                                  incomeSourceId: String)
                                 (implicit hc: HeaderCarrier): Future[Either[ServiceError, PropertyAnnualSubmission]] = {
    connector.getPropertyAnnualSubmission(taxYear, taxableEntityId, incomeSourceId).map {
      case Left(error) => Left(ApiServiceError(error.status))
      case Right(annualSubmission) => annualSubmission.map(Right.apply).getOrElse(Left(DataNotFoundError))
    }
  }

  def   deletePropertyAnnualSubmission(incomeSourceId: String,
                                        taxableEntityId: String,
                                        taxYear: Int)
                                 (implicit hc: HeaderCarrier): Future[Either[ServiceError, Unit]] = {

    connector.deletePropertyAnnualSubmission(incomeSourceId, taxableEntityId, taxYear).flatMap {
      case Left(error) => Future.successful(Left(ApiServiceError(error.status)))
      case Right(_) => Future.successful(Right())
    }
  }

  def createPeriodicSubmission(nino: String, incomeSourceId: String, taxYear: Int, body: Option[JsValue])
                              (implicit hc: HeaderCarrier): Future[Either[ServiceError, PeriodicSubmissionId]] = {

    connector.createPeriodicSubmission(taxYear, nino, incomeSourceId, body.get).flatMap {
      case Left(error) => Future.successful(Left(ApiServiceError(error.status)))
      case Right(periodicSubmissionId) => Future.successful(Right(periodicSubmissionId.get))
    }
  }

  def updatePeriodicSubmission(nino: String, incomeSourceId: String, taxYear: Int, submissionId: String, body: Option[JsValue])
                              (implicit hc: HeaderCarrier): Future[Either[ServiceError, String]] = {

    connector.updatePeriodicSubmission(nino, incomeSourceId, taxYear, submissionId, body.get).flatMap {
      case Left(error) => Future.successful(Left(ApiServiceError(error.status)))
      case Right(_) => Future.successful(Right(""))
    }
  }

  def createOrUpdateAnnualSubmission(nino: String, incomeSourceId: String, taxYear: Int, body: Option[JsValue])
                                    (implicit hc: HeaderCarrier): Future[Either[ServiceError, Unit]] = {

    connector.createOrUpdateAnnualSubmission(taxYear, nino, incomeSourceId, body.get).flatMap {
      case Left(error) => Future.successful(Left(ApiServiceError(error.status)))
      case Right(_) => Future.successful(Right())
    }
  }

  private def getPropertySubmissions(taxYear: Int, taxableEntityId: String, incomeSourceId: String, periodicSubmissionIds: List[PeriodicSubmissionIdModel])
                                    (implicit hc: HeaderCarrier): Future[List[PropertyPeriodicSubmission]] = {
    val propertyPeriodicSubmissions = periodicSubmissionIds
      .filter(submissionId => Period.between(submissionId.fromDate, submissionId.toDate).getYears >= 1)
      .map { submissionId =>
        // get each of the property periodic submission details
        connector.getPropertyPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId, submissionId.submissionId).map {
          case Left(_) => None
          case Right(propertyPeriodicSubmission) => propertyPeriodicSubmission
        }
      }
    Future.sequence(propertyPeriodicSubmissions).map(_.flatten)
  }

  private def transformToResponse(submissions: List[PropertyPeriodicSubmission]): Either[ServiceError, PropertyPeriodicSubmissionResponse] = {
    if (submissions.nonEmpty) {
      Right(PropertyPeriodicSubmissionResponse(submissions))
    } else {
      Left(DataNotFoundError)
    }
  }

  def persistAnswers[A](ctx: JourneyContext, answers: A)(implicit
                                                         writes: Writes[A]): Future[Boolean] =
    repository.upsertAnswers(ctx, Json.toJson(answers))

}
