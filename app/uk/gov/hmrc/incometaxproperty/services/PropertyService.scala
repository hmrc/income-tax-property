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
import uk.gov.hmrc.incometaxproperty.models.errors.{ApiServiceError, DataNotFoundError, ServiceError}
import uk.gov.hmrc.incometaxproperty.models.responses.PeriodicSubmissionIdModel
import uk.gov.hmrc.incometaxproperty.models.{PeriodicSubmission, PeriodicSubmissionResponse}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PropertyService @Inject()(integrationFrameworkConnector: IntegrationFrameworkConnector)
                               (implicit ec: ExecutionContext) {

  def getPeriodicSubmission(taxYear: String,
                            taxableEntityId: String,
                            incomeSourceId: String)
                           (implicit hc: HeaderCarrier): Future[Either[ServiceError, PeriodicSubmissionResponse]] = {
    integrationFrameworkConnector.getPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId).map {
      case Left(error) => Left(ApiServiceError(error.status.toString))
      case Right(data) => val periodicSubmissionIds = data.periodicSubmissionIdModels.map { x: PeriodicSubmissionIdModel =>
        PeriodicSubmission(x.submissionId, x.fromDate, x.toDate)
      }
        if (periodicSubmissionIds.nonEmpty) {
          Right(PeriodicSubmissionResponse(periodicSubmissionIds))
        } else {
          Left(DataNotFoundError)
        }
    }
  }
}
