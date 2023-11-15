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

package uk.gov.hmrc.incometaxproperty.actions

import uk.gov.hmrc.incometaxproperty.models.User
import uk.gov.hmrc.incometaxproperty.models.auth.Enrolment.{Agent, Individual, Nino}
import play.api.Logger
import play.api.mvc.Results.Unauthorized
import play.api.mvc._
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector, AuthorisationException, AuthorisedFunctions, ConfidenceLevel, Enrolment, EnrolmentIdentifier, Enrolments, NoActiveSession}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals.{affinityGroup, allEnrolments, confidenceLevel}
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

case class AuthorisationRequest[T](user: User, request: Request[T]) extends WrappedRequest[T](request)

class AuthorisedAction @Inject()(defaultActionBuilder: DefaultActionBuilder,
                                 val authConnector: AuthConnector,
                                 cc: ControllerComponents) extends AuthorisedFunctions {

  private lazy val logger: Logger = Logger.apply(this.getClass)
  private implicit val executionContext: ExecutionContext = cc.executionContext

  private val agentDelegatedAuthRuleKey: String = "mtd-it-auth"
  private val minimumConfidenceLevel: Int = ConfidenceLevel.L250.level
  private val unauthorized: Future[Result] = Future.successful(Unauthorized)

  def async(block: AuthorisationRequest[AnyContent] => Future[Result]): Action[AnyContent] = defaultActionBuilder.async { implicit request =>
    request.headers.get("mtditid").fold {
      val logMessage = "[AuthorisedAction][async] - No MTDITID in the header. Returning unauthorised."
      logger.warn(logMessage)
      unauthorized
    } {
      mtdItId =>
        implicit val headerCarrier: HeaderCarrier = HeaderCarrierConverter.fromRequest(request)
        authorised().retrieve(affinityGroup) {
          case Some(AffinityGroup.Agent) => agentAuthentication(block, mtdItId)(request, headerCarrier)
          case Some(AffinityGroup.Individual) => individualAuthentication(block, mtdItId)(request, headerCarrier)
        } recover {
          case _: NoActiveSession =>
            val logMessage = s"[AuthorisedAction][async] - No active session."
            logger.info(logMessage)
            Unauthorized
          case _: AuthorisationException =>
            val logMessage = s"[AuthorisedAction][async] - User failed to authenticate"
            logger.info(logMessage)
            Unauthorized
        }
    }
  }

  private[actions] def individualAuthentication[A](block: AuthorisationRequest[A] => Future[Result], requestMtdItId: String)
                                                  (implicit request: Request[A], hc: HeaderCarrier): Future[Result] = {
    authorised().retrieve(allEnrolments and confidenceLevel) {
      case enrolments ~ userConfidence if userConfidence.level >= minimumConfidenceLevel =>
        val optionalMtdItId: Option[String] = enrolmentGetIdentifierValue(Individual.key, Individual.value, enrolments)
        val optionalNino: Option[String] = enrolmentGetIdentifierValue(Nino.key, Nino.value, enrolments)

        (optionalMtdItId, optionalNino) match {
          case (Some(authMTDITID), Some(_)) =>
            enrolments.enrolments.collectFirst {
              case Enrolment(Individual.key, enrolmentIdentifiers, _, _)
                if enrolmentIdentifiers.exists(identifier => identifier.key == Individual.value && identifier.value == requestMtdItId) =>
                block(AuthorisationRequest(User(requestMtdItId, None), request))
            } getOrElse {
              val logMessage = s"[AuthorisedAction][individualAuthentication] Non-agent with an invalid MTDITID. " +
                s"MTDITID in auth matches MTDITID in request: ${authMTDITID == requestMtdItId}"
              logger.info(logMessage)
              unauthorized
            }
          case (_, None) =>
            val logMessage = s"[AuthorisedAction][individualAuthentication] - User has no nino."
            logger.info(logMessage)
            unauthorized
          case (None, _) =>
            val logMessage = s"[AuthorisedAction][individualAuthentication] - User has no MTD IT enrolment."
            logger.info(logMessage)
            unauthorized
        }
      case _ =>
        val logMessage = "[AuthorisedAction][individualAuthentication] User has confidence level below 250."
        logger.info(logMessage)
        unauthorized
    }
  }

  private[actions] def agentAuthentication[A](block: AuthorisationRequest[A] => Future[Result], mtdItId: String)
                                             (implicit request: Request[A], hc: HeaderCarrier): Future[Result] = {
    lazy val agentAuthPredicate: String => Enrolment = identifierId =>
      Enrolment(Individual.key)
        .withIdentifier(Individual.value, identifierId)
        .withDelegatedAuthRule(agentDelegatedAuthRuleKey)

    authorised(agentAuthPredicate(mtdItId))
      .retrieve(allEnrolments) { enrolments =>
        enrolmentGetIdentifierValue(Agent.key, Agent.value, enrolments) match {
          case Some(arn) =>
            block(AuthorisationRequest(User(mtdItId, Some(arn)), request))
          case None =>
            val logMessage = "[AuthorisedAction][agentAuthentication] Agent with no HMRC-AS-AGENT enrolment."
            logger.info(logMessage)
            unauthorized
        }
      } recover {
      case _: NoActiveSession =>
        val logMessage = s"[AuthorisedAction][agentAuthentication] - No active session."
        logger.info(logMessage)
        Unauthorized
      case _: AuthorisationException =>
        val logMessage = s"[AuthorisedAction][agentAuthentication] - Agent does not have delegated authority for Client."
        logger.info(logMessage)
        Unauthorized
    }
  }

  private[actions] def enrolmentGetIdentifierValue(checkedKey: String,
                                                   checkedIdentifier: String,
                                                   enrolments: Enrolments): Option[String] = enrolments.enrolments.collectFirst {
    case Enrolment(`checkedKey`, enrolmentIdentifiers, _, _) => enrolmentIdentifiers.collectFirst {
      case EnrolmentIdentifier(`checkedIdentifier`, identifierValue) => identifierValue
    }
  }.flatten
}

