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
import models.auth.Enrolment.{Individual, SupportingAgent}
import org.scalamock.handlers.CallHandler4
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.{EmptyPredicate, Predicate}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Retrieval, ~}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait MockAuthConnector extends MockFactory {

  protected val mockAuthConnector: AuthConnector = mock[AuthConnector]

  type AuthConnectorResponse[A] = CallHandler4[Predicate, Retrieval[A], HeaderCarrier, ExecutionContext, Future[A]]

  val primaryAgentPredicate: String => Enrolment = mtdId =>
    Enrolment(Individual.key)
      .withIdentifier(Individual.value, mtdId)
      .withDelegatedAuthRule(DelegatedAuthRules.agentDelegatedAuthRule)

  val secondaryAgentPredicate: String => Enrolment = mtdId =>
    Enrolment(SupportingAgent.key)
      .withIdentifier(SupportingAgent.value, mtdId)
      .withDelegatedAuthRule(DelegatedAuthRules.supportingAgentDelegatedAuthRule)

  def mockAuthConnectorResponse[A](predicate: Predicate, retrieval: Retrieval[A])
                                  (returnValue: Future[A]): CallHandler4[Predicate, Retrieval[A], HeaderCarrier, ExecutionContext, Future[A]] =
    (mockAuthConnector.authorise(_: Predicate, _: Retrieval[A])(_: HeaderCarrier, _: ExecutionContext))
      .expects(predicate, retrieval, *, *)
      .returning(returnValue)

  def mockAuthAffinityGroup(affinityGroup: AffinityGroup): AuthConnectorResponse[Option[AffinityGroup]] =
    mockAuthConnectorResponse(EmptyPredicate, Retrievals.affinityGroup)(Future.successful(Some(affinityGroup)))

  def mockAuthReturnException[A](predicate: Predicate, retrieval: Retrieval[A])(exception: Exception): AuthConnectorResponse[A] =
    mockAuthConnectorResponse(predicate, retrieval)(Future.failed(exception))

  def mockAuthAsPrimaryAgent(mtdId: String)(enrolments: Enrolments): AuthConnectorResponse[Enrolments] =
    mockAuthConnectorResponse(primaryAgentPredicate(mtdId), Retrievals.allEnrolments)(Future.successful(enrolments))

  def mockAuthAsSecondaryAgent(mtdId: String)(enrolments: Enrolments): AuthConnectorResponse[Enrolments] =
    mockAuthConnectorResponse(secondaryAgentPredicate(mtdId), Retrievals.allEnrolments)(Future.successful(enrolments))

  def mockAuthAsIndividual(response: Enrolments ~ ConfidenceLevel): AuthConnectorResponse[Enrolments ~ ConfidenceLevel] =
    mockAuthConnectorResponse(EmptyPredicate, Retrievals.allEnrolments and Retrievals.confidenceLevel)(Future.successful(response))
}
