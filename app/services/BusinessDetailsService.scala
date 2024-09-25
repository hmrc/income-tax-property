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

package services

import connectors.BusinessDetailsConnector
import models.BusinessDetailsResponse
import models.errors.{ApiServiceError, DataNotFoundError, ServiceError}
import models.responses.PropertyDetailsModel
import models.responses.PropertyDetailsModel.toResponseModel
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class BusinessDetailsService @Inject() (connector: BusinessDetailsConnector)(implicit ec: ExecutionContext) {

  def getBusinessDetails(
    nino: String
  )(implicit hc: HeaderCarrier): Future[Either[ServiceError, BusinessDetailsResponse]] =
    connector.getBusinessDetails(nino).map {
      case Left(error) => Left(ApiServiceError(error.status))
      case Right(allBusinessDetails) =>
        allBusinessDetails
          .flatMap(_.taxPayerDisplayResponse.propertyData)
          .fold[Either[ServiceError, BusinessDetailsResponse]](Left(DataNotFoundError))(propDetailsList =>
            Right(BusinessDetailsResponse(filterProperty(propDetailsList).map(property => toResponseModel(property))))
          )
    }

  private def filterProperty(propertyDetailsList: Seq[PropertyDetailsModel]): Seq[PropertyDetailsModel] =
    propertyDetailsList.filter { propData =>
      propData.incomeSourceType.exists(sourceType => sourceType == "uk-property" || sourceType == "foreign-property")
    }

}
