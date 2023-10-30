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
import uk.gov.hmrc.incometaxproperty.models.BusinessDetails
import uk.gov.hmrc.incometaxproperty.models.errors.ApiServiceError

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class IntegrationFrameworkService @Inject()(connector: IntegrationFrameworkConnector)
                                           (implicit ec: ExecutionContext) {

  def getBusinessDetails(nino: String)(implicit hc: HeaderCarrier): Future[Either[ApiServiceError, Option[BusinessDetails]]] = {
    connector.getBusinessDetails(nino).map {
      case Left(error) => Left(ApiServiceError(error.status.toString))
      case Right(allBusinessDetails) => Right(allBusinessDetails)
    }
  }
}
