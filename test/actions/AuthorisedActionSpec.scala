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

package actions

import models.auth.DelegatedAuthRules
import models.auth.Enrolment.{Agent, Individual, Nino, SupportingAgent}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.SystemMaterializer
import play.api.http.Status._
import play.api.mvc.Results._
import play.api.mvc._
import play.api.test.{FakeRequest, Helpers}
import uk.gov.hmrc.auth.core.ConfidenceLevel.L250
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.syntax.retrieved.authSyntaxForRetrieved
import uk.gov.hmrc.http.HeaderCarrier
import utils.UnitTest
import utils.mocks.{MockAppConfig, MockAuthConnector}
import utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthorisedActionSpec extends UnitTest
  with MockAuthConnector
  with MockAppConfig
  with FakeRequestProvider {

  val mtditid = "1234567890"
  val arn = "0987654321"
  val nino = "AA123456C"
  private val requestWithMtditid: FakeRequest[AnyContentAsEmpty.type] = fakeRequest.withHeaders("mtditid" -> mtditid)
  private implicit val emptyHeaderCarrier: HeaderCarrier = HeaderCarrier()
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val mockControllerComponents: ControllerComponents = Helpers.stubControllerComponents()
  private val defaultActionBuilder: DefaultActionBuilder = DefaultActionBuilder(mockControllerComponents.parsers.default)
  implicit val materializer: SystemMaterializer = SystemMaterializer(actorSystem)

  private val underTest: AuthorisedAction = new AuthorisedAction(defaultActionBuilder, mockAuthConnector, mockControllerComponents, mockAppConfig)

  lazy val block: AuthorisationRequest[AnyContent] => Future[Result] = request =>
    Future.successful(Ok(s"mtditid: ${request.user.mtditid}${request.user.arn.fold("")(arn => " arn: " + arn)}"))

  ".async" should {
    "perform the block action" when {
      "the user is successfully verified as an Primary Agent" which {
        "should return an OK(200) status" in {
          mockAuthAffinityGroup(AffinityGroup.Agent)
          mockAuthAsPrimaryAgent(mtditid)(Enrolments(Set(
            Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated"),
            Enrolment(Agent.key, Seq(EnrolmentIdentifier(Agent.value, arn)), "Activated")
          )))

          val result = await(underTest.async(block)(requestWithMtditid))

          result.header.status shouldBe OK
          await(result.body.consumeData.map(_.utf8String)) shouldBe s"mtditid: $mtditid arn: $arn"
        }
      }

      "the user is successfully verified as a Secondary Agent (EMA Supporting Agent enabled)" which {
        "should return an OK(200) status" in {
          MockAppConfig.emaSupportingAgentsEnabled(true)
          mockAuthAffinityGroup(AffinityGroup.Agent)
          mockAuthReturnException(primaryAgentPredicate(mtditid), Retrievals.allEnrolments)(new InsufficientConfidenceLevel)
          mockAuthAsSecondaryAgent(mtditid)(Enrolments(Set(
            Enrolment(
              key = SupportingAgent.key,
              identifiers = Seq(EnrolmentIdentifier(Individual.value, mtditid)),
              state = "Activated",
              delegatedAuthRule = Some(DelegatedAuthRules.supportingAgentDelegatedAuthRule)
            ),
            Enrolment(Agent.key, Seq(EnrolmentIdentifier(Agent.value, arn)), "Activated")
          )))

          val result = await(underTest.async(block)(requestWithMtditid))

          result.header.status shouldBe OK
          await(result.body.consumeData.map(_.utf8String)) shouldBe s"mtditid: $mtditid arn: $arn"
        }
      }

      "the user is successfully verified as an individual" in {
        mockAuthAffinityGroup(AffinityGroup.Individual)
        mockAuthAsIndividual(Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated"),
          Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, mtditid)), "Activated")
        )) and L250)

        val result = await(underTest.async(block)(requestWithMtditid))

        result.header.status shouldBe OK
        await(result.body.consumeData.map(_.utf8String)) shouldBe s"mtditid: $mtditid"
      }
    }

    "return an Unauthorised" when {
      "the authorisation service returns an AuthorisationException exception" in {
        mockAuthReturnException(EmptyPredicate, Retrievals.affinityGroup)(InsufficientEnrolments())
        await(underTest.async(block)(requestWithMtditid)).header.status shouldBe UNAUTHORIZED
      }

      "the authorisation service returns a NoActiveSession exception" in {
        mockAuthReturnException(EmptyPredicate, Retrievals.affinityGroup)(MissingBearerToken())
        await(underTest.async(block)(requestWithMtditid)).header.status shouldBe UNAUTHORIZED
      }

      "the request does not contain mtditid header" in {
        await(underTest.async(block)(FakeRequest())).header.status shouldBe UNAUTHORIZED
      }
    }
  }

  ".individualAuthentication" should {
    "perform the block action" when {
      "the correct enrolment exist and nino exist" which {

        lazy val result: Result = {
          mockAuthAsIndividual(Enrolments(Set(
            Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated"),
            Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, mtditid)), "Activated")
          )) and ConfidenceLevel.L250)
          await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
        }

        "returns an OK status" in {
          result.header.status shouldBe OK
        }

        "returns a body of the mtditid" in {
          await(result.body.consumeData.map(_.utf8String)) shouldBe s"mtditid: $mtditid"
        }
      }
    }

    "return a UNAUTHORIZED" when {
      "the correct enrolment is missing" in {

        mockAuthAsIndividual(Enrolments(Set(
          Enrolment("notAnIndividualOops", Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated")
        )) and L250)

        val result: Result =
          await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))

        result.header.status shouldBe UNAUTHORIZED
      }

      "the correct enrolment and nino exist but the request is for a different id" in {

        mockAuthAsIndividual(Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, "123456")), "Activated"),
          Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, mtditid)), "Activated")
        )) and L250)

        val result: Result = await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))

        result.header.status shouldBe UNAUTHORIZED
      }

      "the correct enrolment and nino exist but low CL" in {

        mockAuthAsIndividual(Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated"),
          Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, mtditid)), "Activated")
        )) and ConfidenceLevel.L50)

        val result: Result = await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))

        result.header.status shouldBe UNAUTHORIZED
      }

      "the correct enrolment exist but no nino" in {

        mockAuthAsIndividual(Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated")
        )) and ConfidenceLevel.L250)

        val result: Result = await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))

        result.header.status shouldBe UNAUTHORIZED
      }

      "the correct nino exist but no enrolment" in {

        mockAuthAsIndividual(Enrolments(Set(
          Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, nino)), "Activated")
        )) and ConfidenceLevel.L250)

        val result: Result = await(underTest.individualAuthentication(block, nino)(requestWithMtditid, emptyHeaderCarrier))

        result.header.status shouldBe UNAUTHORIZED
      }
    }
  }

  ".agentAuthentication" should {
    "perform the block action" when {
      "the agent is authorised for the given user (Primary Agent)" which {

        lazy val result = {
          mockAuthAsPrimaryAgent(mtditid)(Enrolments(Set(
            Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated"),
            Enrolment(Agent.key, Seq(EnrolmentIdentifier(Agent.value, arn)), "Activated")
          )))
          await(underTest.agentAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
        }

        "has a status of OK" in {
          result.header.status shouldBe OK
        }

        "has the correct body" in {
          await(result.body.consumeData.map(_.utf8String)) shouldBe s"mtditid: $mtditid arn: $arn"
        }
      }

      "the agent is authorised for the given user (Secondary Agent - EMA Supporting Agent Feature enabled)" which {

        lazy val result = {
          MockAppConfig.emaSupportingAgentsEnabled(true)
          mockAuthReturnException(primaryAgentPredicate(mtditid), Retrievals.allEnrolments)(InsufficientEnrolments())
          mockAuthConnectorResponse(secondaryAgentPredicate(mtditid), Retrievals.allEnrolments)(Future.successful(
            Enrolments(Set(
              Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated"),
              Enrolment(Agent.key, Seq(EnrolmentIdentifier(Agent.value, arn)), "Activated")
            ))
          ))

          await(underTest.agentAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
        }

        "has a status of OK" in {
          result.header.status shouldBe OK
        }

        "has the correct body" in {
          await(result.body.consumeData.map(_.utf8String)) shouldBe s"mtditid: $mtditid arn: $arn"
        }
      }
    }

    "return an Unauthorised" when {

      "the authorisation service returns a NoActiveSession exception" in {

        mockAuthReturnException(primaryAgentPredicate(mtditid), Retrievals.allEnrolments)(BearerTokenExpired())

        val result =underTest.agentAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier)

        await(result).header.status shouldBe UNAUTHORIZED
      }

      "the authorisation service returns an AuthorisationException exception (EMA Supporting Agent Disabled)" in {

        MockAppConfig.emaSupportingAgentsEnabled(false)
        mockAuthReturnException(primaryAgentPredicate(mtditid), Retrievals.allEnrolments)(InsufficientEnrolments())

        val result = underTest.agentAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier)

        await(result).header.status shouldBe UNAUTHORIZED
      }

      "the authorisation service returns an AuthorisationException for secondary agent check (EMA Supporting Agent Enabled)" in {

        MockAppConfig.emaSupportingAgentsEnabled(true)
        mockAuthReturnException(primaryAgentPredicate(mtditid), Retrievals.allEnrolments)(InsufficientEnrolments())
        mockAuthReturnException(secondaryAgentPredicate(mtditid), Retrievals.allEnrolments)(InsufficientEnrolments())

        val result = underTest.agentAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier)

        await(result).header.status shouldBe UNAUTHORIZED
      }

      "the user does not have an enrolment for the agent" in {

        mockAuthAsPrimaryAgent(mtditid)(
          Enrolments(Set(Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated")))
        )

        val result = underTest.agentAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier)

        await(result).header.status shouldBe UNAUTHORIZED
      }
    }
  }

  ".enrolmentGetIdentifierValue" should {
    "return the value for a given identifier" in {
      val returnValue = "anIdentifierValue"
      val returnValueAgent = "anAgentIdentifierValue"
      val enrolments = Enrolments(Set(
        Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, returnValue)), "Activated"),
        Enrolment(Agent.key, Seq(EnrolmentIdentifier(Agent.value, returnValueAgent)), "Activated")
      ))

      underTest.enrolmentGetIdentifierValue(Individual.key, Individual.value, enrolments) shouldBe Some(returnValue)
      underTest.enrolmentGetIdentifierValue(Agent.key, Agent.value, enrolments) shouldBe Some(returnValueAgent)
    }

    "return a None" when {
      val key = "someKey"
      val identifierKey = "anIdentifier"
      val returnValue = "anIdentifierValue"
      val enrolments = Enrolments(Set(Enrolment(key, Seq(EnrolmentIdentifier(identifierKey, returnValue)), "someState")))

      "the given identifier cannot be found" in {
        underTest.enrolmentGetIdentifierValue(key, "someOtherIdentifier", enrolments) shouldBe None
      }

      "the given key cannot be found" in {
        underTest.enrolmentGetIdentifierValue("someOtherKey", identifierKey, enrolments) shouldBe None
      }
    }
  }
}
