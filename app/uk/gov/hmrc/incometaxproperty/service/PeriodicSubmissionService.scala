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

package uk.gov.hmrc.incometaxproperty.service

import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.incometaxproperty.connectors.PeriodicSubmissionConnector
import uk.gov.hmrc.incometaxproperty.models.error.ApiServiceError
import uk.gov.hmrc.incometaxproperty.models.propertyperiodicsubmission.response.PropertyPeriodicSubmissionResponse

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PeriodicSubmissionService @Inject()(periodicSubmissionConnector: PeriodicSubmissionConnector)
                                         (implicit ec: ExecutionContext) {

  def getPeriodicSubmission(taxYear: String, taxableEntityId: String, incomeSourceId: String)
                           (implicit hc: HeaderCarrier): Future[Either[ApiServiceError, PropertyPeriodicSubmissionResponse]] = {
    periodicSubmissionConnector.getPeriodicSubmission(taxYear, taxableEntityId, incomeSourceId).map {
      case Left(error) => Left(ApiServiceError(error.status.toString))
      case Right(data) => Right(data)
    }
  }

}
