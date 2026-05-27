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

import models.auth.DelegatedAuthRules
import models.auth.Enrolment.Individual
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.{EmptyPredicate, Predicate}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Retrieval, ~}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait MockAuthConnector extends MockitoSugar {

  protected val mockAuthConnector: AuthConnector = mock[AuthConnector]

  val primaryAgentPredicate: String => Enrolment = mtdId =>
    Enrolment(Individual.key)
      .withIdentifier(Individual.value, mtdId)
      .withDelegatedAuthRule(DelegatedAuthRules.agentDelegatedAuthRule)

  def mockAuthConnectorResponse[A](predicate: Predicate, retrieval: Retrieval[A])
                                  (returnValue: Future[A]): Unit =
    when(mockAuthConnector.authorise(eqTo(predicate), eqTo(retrieval))(any[HeaderCarrier], any[ExecutionContext]))
      .thenReturn(returnValue)

  def mockAuthAffinityGroup(affinityGroup: AffinityGroup): Unit =
    mockAuthConnectorResponse(EmptyPredicate, Retrievals.affinityGroup)(Future.successful(Some(affinityGroup)))

  def mockAuthReturnException[A](predicate: Predicate, retrieval: Retrieval[A])(exception: Exception): Unit =
    mockAuthConnectorResponse(predicate, retrieval)(Future.failed(exception))

  def mockAuthAsPrimaryAgent(mtdId: String)(enrolments: Enrolments): Unit =
    mockAuthConnectorResponse(primaryAgentPredicate(mtdId), Retrievals.allEnrolments)(Future.successful(enrolments))

  def mockAuthAsIndividual(response: Enrolments ~ ConfidenceLevel): Unit =
    mockAuthConnectorResponse(EmptyPredicate, Retrievals.allEnrolments and Retrievals.confidenceLevel)(Future.successful(response))
}
