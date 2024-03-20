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

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.SystemMaterializer
import play.api.http.Status._
import play.api.mvc.Results._
import play.api.mvc._
import play.api.test.{FakeRequest, Helpers}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.Retrieval
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.syntax.retrieved.authSyntaxForRetrieved
import uk.gov.hmrc.http.HeaderCarrier
import models.auth.Enrolment.{Agent, Individual, Nino}
import utils.UnitTest
import utils.mocks.MockAuthConnector
import utils.providers.FakeRequestProvider

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class AuthorisedActionSpec extends UnitTest
  with MockAuthConnector
  with FakeRequestProvider {

  private val requestWithMtditid: FakeRequest[AnyContentAsEmpty.type] = fakeRequest.withHeaders("mtditid" -> "1234567890")
  private implicit val emptyHeaderCarrier: HeaderCarrier = HeaderCarrier()
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val mockControllerComponents: ControllerComponents = Helpers.stubControllerComponents()
  private val defaultActionBuilder: DefaultActionBuilder = DefaultActionBuilder(mockControllerComponents.parsers.default)
  implicit val materializer: SystemMaterializer = SystemMaterializer(actorSystem)

  private val underTest: AuthorisedAction = new AuthorisedAction(defaultActionBuilder, mockAuthConnector, mockControllerComponents)

  ".async" should {
    lazy val block: AuthorisationRequest[AnyContent] => Future[Result] = request =>
      Future.successful(Ok(s"mtditid: ${request.user.mtditid}${request.user.arn.fold("")(arn => " arn: " + arn)}"))

    "perform the block action" when {
      "the user is successfully verified as an agent" which {
        val agentEnrolments: Enrolments = Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, "1234567890")), "Activated"),
          Enrolment(Agent.key, Seq(EnrolmentIdentifier(Agent.value, "0987654321")), "Activated")
        ))

        mockAuthAsAgent(agentEnrolments)

        val result = await(underTest.async(block)(requestWithMtditid))

        "should return an OK(200) status" in {
          result.header.status shouldBe OK
          await(result.body.consumeData.map(_.utf8String)) shouldBe "mtditid: 1234567890 arn: 0987654321"
        }
      }

      "the user is successfully verified as an individual" in {
        val individualEnrolments: Enrolments = Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, "1234567890")), "Activated"),
          Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, "1234567890")), "Activated")
        ))

        mockAuth(individualEnrolments)

        lazy val result = await(underTest.async(block)(requestWithMtditid))

        result.header.status shouldBe OK
        await(result.body.consumeData.map(_.utf8String)) shouldBe "mtditid: 1234567890"
      }
    }

    "return an Unauthorised" when {
      "the authorisation service returns an AuthorisationException exception" in {
        object AuthException extends AuthorisationException("Some reason")

        mockAuthReturnException(AuthException)

        await(underTest.async(block)(requestWithMtditid)).header.status shouldBe UNAUTHORIZED
      }
    }

    "return an Unauthorised" when {
      "the authorisation service returns a NoActiveSession exception" in {
        object NoActiveSession extends NoActiveSession("Some reason")

        mockAuthReturnException(NoActiveSession)

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
        val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
        val mtditid = "AAAAAA"
        val enrolments = Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated"),
          Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, mtditid)), "Activated")
        ))
        lazy val result: Result = {
          (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
            .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
            .returning(Future.successful(enrolments and ConfidenceLevel.L250))

          await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
        }

        "returns an OK status" in {
          result.header.status shouldBe OK
        }

        "returns a body of the mtditid" in {
          await(result.body.consumeData.map(_.utf8String)) shouldBe mtditid
        }
      }

      "the correct enrolment and nino exist but the request is for a different id" which {
        val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
        val mtditid = "AAAAAA"
        val enrolments = Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, "123456")), "Activated"),
          Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, mtditid)), "Activated")
        ))
        lazy val result: Result = {
          (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
            .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
            .returning(Future.successful(enrolments and ConfidenceLevel.L250))

          await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
        }

        "returns an UNAUTHORIZED status" in {
          result.header.status shouldBe UNAUTHORIZED
        }
      }

      "the correct enrolment and nino exist but low CL" which {
        val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
        val mtditid = "AAAAAA"
        val enrolments = Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated"),
          Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, mtditid)), "Activated")
        ))
        lazy val result: Result = {
          (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
            .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
            .returning(Future.successful(enrolments and ConfidenceLevel.L50))

          await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
        }

        "returns an UNAUTHORIZED status" in {
          result.header.status shouldBe UNAUTHORIZED
        }
      }

      "the correct enrolment exist but no nino" which {
        val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
        val mtditid = "AAAAAA"
        val enrolments = Enrolments(Set(Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated")))
        lazy val result = {
          (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
            .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
            .returning(Future.successful(enrolments and ConfidenceLevel.L250))

          await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
        }

        "returns an 401 status" in {
          result.header.status shouldBe UNAUTHORIZED
        }
      }

      "the correct nino exist but no enrolment" which {
        val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
        val id = "AAAAAA"
        val enrolments = Enrolments(Set(Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, id)), "Activated")))
        lazy val result: Result = {
          (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
            .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
            .returning(Future.successful(enrolments and ConfidenceLevel.L250))

          await(underTest.individualAuthentication(block, id)(requestWithMtditid, emptyHeaderCarrier))
        }

        "returns an 401 status" in {
          result.header.status shouldBe UNAUTHORIZED
        }
      }
    }

    "return a UNAUTHORIZED" when {
      "the correct enrolment is missing" which {
        val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
        val mtditid = "AAAAAA"
        val enrolments = Enrolments(Set(Enrolment("notAnIndividualOops", Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated")))
        lazy val result: Result = {
          (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
            .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
            .returning(Future.successful(enrolments and ConfidenceLevel.L250))

          await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
        }

        "returns a forbidden" in {
          result.header.status shouldBe UNAUTHORIZED
        }
      }
    }

    "the correct enrolment and nino exist but the request is for a different id" which {
      val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
      val mtditid = "AAAAAA"
      val enrolments = Enrolments(Set(
        Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, "123456")), "Activated"),
        Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, mtditid)), "Activated")
      ))
      lazy val result: Result = {
        (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
          .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
          .returning(Future.successful(enrolments and ConfidenceLevel.L250))

        await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
      }

      "returns an UNAUTHORIZED status" in {
        result.header.status shouldBe UNAUTHORIZED
      }
    }

    "the correct enrolment and nino exist but low CL" which {
      val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
      val mtditid = "AAAAAA"
      val enrolments = Enrolments(Set(
        Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated"),
        Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, mtditid)), "Activated")
      ))
      lazy val result: Result = {
        (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
          .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
          .returning(Future.successful(enrolments and ConfidenceLevel.L50))

        await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
      }

      "returns an UNAUTHORIZED status" in {
        result.header.status shouldBe UNAUTHORIZED
      }
    }

    "the correct enrolment exist but no nino" which {
      val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
      val mtditid = "AAAAAA"
      val enrolments = Enrolments(Set(Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, mtditid)), "Activated")))
      lazy val result: Result = {
        (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
          .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
          .returning(Future.successful(enrolments and ConfidenceLevel.L250))

        await(underTest.individualAuthentication(block, mtditid)(requestWithMtditid, emptyHeaderCarrier))
      }

      "returns an 401 status" in {
        result.header.status shouldBe UNAUTHORIZED
      }
    }

    "the correct nino exist but no enrolment" which {
      val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(request.user.mtditid))
      val id = "AAAAAA"
      val enrolments = Enrolments(Set(Enrolment(Nino.key, Seq(EnrolmentIdentifier(Nino.value, id)), "Activated")))
      lazy val result: Result = {
        (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
          .expects(*, Retrievals.allEnrolments and Retrievals.confidenceLevel, *, *)
          .returning(Future.successful(enrolments and ConfidenceLevel.L250))

        await(underTest.individualAuthentication(block, id)(requestWithMtditid, emptyHeaderCarrier))
      }

      "returns an 401 status" in {
        result.header.status shouldBe UNAUTHORIZED
      }
    }
  }

  ".agentAuthentication" should {
    val block: AuthorisationRequest[AnyContent] => Future[Result] = request => Future.successful(Ok(s"${request.user.mtditid} ${request.user.arn.get}"))

    "perform the block action" when {
      "the agent is authorised for the given user" which {
        val enrolments = Enrolments(Set(
          Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, "1234567890")), "Activated"),
          Enrolment(Agent.key, Seq(EnrolmentIdentifier(Agent.value, "0987654321")), "Activated")
        ))
        lazy val result = {
          (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
            .expects(*, Retrievals.allEnrolments, *, *)
            .returning(Future.successful(enrolments))

          await(underTest.agentAuthentication(block, "1234567890")(requestWithMtditid, emptyHeaderCarrier))
        }

        "has a status of OK" in {
          result.header.status shouldBe OK
        }

        "has the correct body" in {
          await(result.body.consumeData.map(_.utf8String)) shouldBe "1234567890 0987654321"
        }
      }
    }

    "return an Unauthorised" when {
      "the authorisation service returns an AuthorisationException exception" in {
        object AuthException extends AuthorisationException("some-exception-reason")

        lazy val result = {
          mockAuthReturnException(AuthException)
          underTest.agentAuthentication(block, "1234567890")(requestWithMtditid, emptyHeaderCarrier)
        }

        await(result).header.status shouldBe UNAUTHORIZED
      }
    }

    "return an Unauthorised" when {
      "the authorisation service returns a NoActiveSession exception" in {
        object NoActiveSession extends NoActiveSession("Some reason")

        lazy val result = {
          mockAuthReturnException(NoActiveSession)
          underTest.agentAuthentication(block, "1234567890")(requestWithMtditid, emptyHeaderCarrier)
        }

        await(result).header.status shouldBe UNAUTHORIZED
      }
    }

    "return a UNAUTHORIZED" when {
      "the user does not have an enrolment for the agent" in {
        val enrolments = Enrolments(Set(Enrolment(Individual.key, Seq(EnrolmentIdentifier(Individual.value, "1234567890")), "Activated")))
        lazy val result = {
          (mockAuthConnector.authorise(_: Predicate, _: Retrieval[_])(_: HeaderCarrier, _: ExecutionContext))
            .expects(*, Retrievals.allEnrolments, *, *)
            .returning(Future.successful(enrolments))

          underTest.agentAuthentication(block, "1234567890")(requestWithMtditid, emptyHeaderCarrier)
        }

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
