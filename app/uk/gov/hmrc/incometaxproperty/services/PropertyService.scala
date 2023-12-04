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

import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.incometaxproperty.connectors.IntegrationFrameworkConnector
import uk.gov.hmrc.incometaxproperty.models.PropertyPeriodicSubmissionResponse
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiServiceError, DataNotFoundError, ServiceError}
import uk.gov.hmrc.incometaxproperty.models.responses.{PeriodicSubmissionIdModel, PropertyPeriodicSubmission}

import java.time.Period
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PropertyService @Inject()(connector: IntegrationFrameworkConnector)
                               (implicit ec: ExecutionContext) {

  def getPropertyPeriodicSubmissions(taxYear: Int,
                                     taxableEntityId: String,
                                     incomeSourceId: String)
                                    (implicit hc: HeaderCarrier): Future[Either[ServiceError, PropertyPeriodicSubmissionResponse]] = {
    connector.getAllPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId).flatMap {
      case Left(error) => Future.successful(Left(ApiServiceError(error.status.toString)))
      case Right(periodicSubmissionIds) =>
        val propertyPeriodicSubmissions = getPropertySubmissions(taxYear, taxableEntityId, incomeSourceId, periodicSubmissionIds)
        propertyPeriodicSubmissions.map(transformToResponse)
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

  private def transformToResponse(submissions: List[PropertyPeriodicSubmission]): Either[ServiceError, PropertyPeriodicSubmissionResponse] ={
    if (submissions.nonEmpty) {
      Right(PropertyPeriodicSubmissionResponse(submissions))
    } else {
      Left(DataNotFoundError)
    }
  }
}
