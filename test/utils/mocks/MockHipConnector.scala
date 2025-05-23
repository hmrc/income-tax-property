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

import connectors.HipConnector
import models.common.Nino
import models.errors.ApiError
import models.responses._
import org.scalamock.handlers.CallHandler3
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait MockHipConnector extends MockFactory {

  protected val mockHipConnector: HipConnector = mock[HipConnector]

  def mockHipGetPropertyBroughtForwardLossSubmission(
    nino: Nino,
    lossId: String,
    result: Either[ApiError, HipPropertyBFLResponse]
  ): CallHandler3[Nino, String, HeaderCarrier, Future[
    Either[ApiError, HipPropertyBFLResponse]
  ]] =
    (mockHipConnector
      .getPropertyBroughtForwardLoss(_: Nino, _: String)(_: HeaderCarrier))
      .expects(nino, lossId, *)
      .returning(Future.successful(result))
}
