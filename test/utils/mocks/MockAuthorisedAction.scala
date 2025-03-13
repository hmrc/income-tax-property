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

import actions.AuthorisedAction
import models.auth.Enrolment.{Individual, Nino}
import org.scalamock.scalatest.MockFactory
import play.api.mvc._
import play.api.test.Helpers.stubMessagesControllerComponents
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.auth.core.syntax.retrieved.authSyntaxForRetrieved
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel, Enrolment, EnrolmentIdentifier, Enrolments}

import scala.concurrent.ExecutionContext.Implicits.global

trait MockAuthorisedAction extends MockFactory with MockAuthConnector {

  private val mcc = stubMessagesControllerComponents()
  private val defaultActionBuilder: DefaultActionBuilder = DefaultActionBuilder(mcc.parsers.default)

  protected val mockAuthorisedAction: AuthorisedAction =
    new AuthorisedAction(defaultActionBuilder, mockAuthConnector, mcc)

  def mockAuthorisation(): AuthConnectorResponse[Enrolments ~ ConfidenceLevel] = {
    val individualEnrolments: Enrolments = Enrolments(
      Set(
        Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, "1234567890")), "Activated"),
        Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, "1234567890")), "Activated")
      )
    )

    mockAuthAffinityGroup(AffinityGroup.Individual)
    mockAuthAsIndividual(individualEnrolments and ConfidenceLevel.L250)
  }
}
