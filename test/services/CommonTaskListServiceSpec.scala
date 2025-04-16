/*
 * Copyright 2025 HM Revenue & Customs
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

import models.common._
import models.domain.JourneyAnswers
import models.errors.{ApiServiceError, DataNotFoundError}
import models.taskList._
import models.{BusinessDetailsResponse, PropertyDetails}
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper
import play.api.libs.json.JsObject
import uk.gov.hmrc.http.HeaderCarrier
import utils.UnitTest
import utils.mocks.{MockBusinessDetailsService, MockMongoJourneyAnswersRepository}
import utils.providers.AppConfigStubProvider

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

class CommonTaskListServiceSpec extends UnitTest
  with MockFactory
  with AppConfigStubProvider
  with MockBusinessDetailsService
  with MockMongoJourneyAnswersRepository
  with BeforeAndAfterEach {

  private trait Test {
    implicit val ec: ExecutionContext = ExecutionContext.global
    implicit val hc: HeaderCarrier = HeaderCarrier()

    val service: CommonTaskListService = new CommonTaskListService(
      appConfig = appConfigStub,
      service = mockIntegrationFrameworkService,
      repository = repository
    )

    val nino: String = "12345678"
    val taxYear: Int = 2025
    val mtdItId = "12345"

    val someProperty: Seq[PropertyDetails] = List(
      PropertyDetails(Some("uk-property"), None, None, "XYIS00000451267"),
      PropertyDetails(Some("foreign-property"), None, None, "XYIS00000451268")
    )
    val someUkProperty: Seq[PropertyDetails] = List(
      PropertyDetails(Some("uk-property"), None, None, "XYIS00000451267")
    )
    val someForeignProperty: Seq[PropertyDetails] = List(
      PropertyDetails(Some("foreign-property"), None, None, "XYIS00000451268")
    )

    def journeyResponse(incomeSourceId: IncomeSourceId, journeyName: JourneyName, status: JourneyStatus, mtditid: String = mtdItId, taxyear: Int = taxYear): JourneyAnswers = {
      JourneyAnswers(mtditid = Mtditid(mtditid),
        incomeSourceId = incomeSourceId,
        taxYear = TaxYear(taxyear),
        journey = journeyName,
        countryCode = None,
        status = status,
        data = JsObject.empty,
        createdAt = Instant.now(),
        updatedAt = Instant.now()
      )
    }

    def removeAll(collection: MongoCollection[_]): Future[Unit] =
      collection
        .deleteMany(Filters.empty())
        .toFuture()
        .map(_ => ())

    def testOnlyAdd(journeyAnswers: JourneyAnswers): Future[Unit] = {
      repository.collection.insertOne(journeyAnswers)
        .toFuture()
        .map(_ => ())
    }
  }

  val emptyTaskSections: List[TaskListSection] = List(
    TaskListSection(sectionTitle = SectionTitle.UkPropertyTitle, taskItems = None),
    TaskListSection(sectionTitle = SectionTitle.ForeignPropertyTitle, taskItems = None),
    TaskListSection(sectionTitle = SectionTitle.UkForeignPropertyTitle, taskItems = None)
  )

  val ukUrl = "http://localhost:TEST/update-and-submit-income-tax-return/property/2025/uk-property/about/start"
  val foreignUrl = "http://localhost:TEST/update-and-submit-income-tax-return/property/2025/foreign-property/about/start"
  val ukForeignUrl = s"http://localhost:TEST/update-and-submit-income-tax-return/property/2025/uk-foreign-property/about/start"

  def taskSections(status: JourneyStatus): List[TaskListSection] = List(
    TaskListSection(
      sectionTitle = SectionTitle.UkPropertyTitle,
      taskItems = Some(List(
        TaskListSectionItem(
          title = TaskTitle.UkProperty,
          status = status,
          href = Some(ukUrl)
        )
      ))
    ),
    TaskListSection(
      sectionTitle = SectionTitle.ForeignPropertyTitle,
      taskItems = Some(List(
        TaskListSectionItem(
          title = TaskTitle.ForeignProperty,
          status = status,
          href = Some(foreignUrl)
        )
      ))
    ),
    TaskListSection(
      sectionTitle = SectionTitle.UkForeignPropertyTitle,
      taskItems = Some(List(
        TaskListSectionItem(
          title = TaskTitle.UkForeignProperty,
          status = status,
          href = Some(ukForeignUrl)
        )
      ))
    )
  )

  val mixedTaskSections: List[TaskListSection] = List(
    TaskListSection(
      sectionTitle = SectionTitle.UkPropertyTitle,
      taskItems = Some(List(
        TaskListSectionItem(
          title = TaskTitle.UkProperty,
          status = JourneyStatus.Completed,
          href = Some(ukUrl)
        )
      ))
    ),
    TaskListSection(
      sectionTitle = SectionTitle.ForeignPropertyTitle,
      taskItems = Some(List(
        TaskListSectionItem(
          title = TaskTitle.ForeignProperty,
          status = JourneyStatus.CheckNow,
          href = Some(foreignUrl)
        )
      ))
    ),
    TaskListSection(
      sectionTitle = SectionTitle.UkForeignPropertyTitle,
      taskItems = Some(List(
        TaskListSectionItem(
          title = TaskTitle.UkForeignProperty,
          status = JourneyStatus.CheckNow,
          href = Some(ukForeignUrl)
        )
      ))
    )
  )

  def ukTaskSections(status: JourneyStatus): List[TaskListSection] = List(
    TaskListSection(
      sectionTitle = SectionTitle.UkPropertyTitle,
      taskItems = Some(List(
        TaskListSectionItem(
          title = TaskTitle.UkProperty,
          status = status,
          href = Some(ukUrl)
        )
      ))
    ),
    TaskListSection(sectionTitle = SectionTitle.ForeignPropertyTitle, taskItems = None),
    TaskListSection(sectionTitle = SectionTitle.UkForeignPropertyTitle, taskItems = None),
  )

  def foreignTaskSections(status: JourneyStatus): List[TaskListSection] = List(
    TaskListSection(sectionTitle = SectionTitle.UkPropertyTitle, taskItems = None),
    TaskListSection(
      sectionTitle = SectionTitle.ForeignPropertyTitle,
      taskItems = Some(List(
        TaskListSectionItem(
          title = TaskTitle.ForeignProperty,
          status = status,
          href = Some(foreignUrl)
        )
      ))
    ),
    TaskListSection(sectionTitle = SectionTitle.UkForeignPropertyTitle, taskItems = None)
  )

  "CommonTaskListService" when {
    "an error occurs while retrieving benefits data from IF" should {
      "return an empty task list when IF returns an API error" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Left(ApiServiceError(500)))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) mustBe emptyTaskSections
      }

      "handle appropriately when an exception occurs" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetailsException(nino, new RuntimeException("Dummy Exception"))

        await(removeAll(repository.collection))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        assertThrows[RuntimeException](await(underTest))
      }
    }

    "the call to IF for benefits data returns a successful, but empty, response" should {
      "return an empty task list if journeyAnswers also empty" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Left(DataNotFoundError))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe emptyTaskSections
      }
    }

    "Property repository contains no journeyAnswers" should {
      "return an empty task list when no Property data exists in IF" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Left(DataNotFoundError))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe emptyTaskSections
      }

      "return 'CheckNow' status for for all data when both Uk and Foreign data exists in IF" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Right(BusinessDetailsResponse(someProperty)))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe taskSections(JourneyStatus.CheckNow)
      }

      "return 'CheckNow' status for for just Uk data when only Uk data exists in IF" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Right(BusinessDetailsResponse(someUkProperty)))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe ukTaskSections(JourneyStatus.CheckNow)
      }

      "return 'CheckNow' status for for just Foreign data when only Foreign data exists in IF" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Right(BusinessDetailsResponse(someForeignProperty)))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe foreignTaskSections(JourneyStatus.CheckNow)
      }
    }

    "All property types have Journey Answers defined" should {
      "use Journey Answers when HMRC held data in IF" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Right(BusinessDetailsResponse(someProperty)))

        await(testOnlyAdd(journeyResponse(IncomeSourceId("123456"), JourneyName.About, JourneyStatus.Completed)))
        await(testOnlyAdd(journeyResponse(IncomeSourceId("123457"), JourneyName.ForeignPropertySelectCountry, JourneyStatus.Completed)))
        await(testOnlyAdd(journeyResponse(IncomeSourceId("123458"), JourneyName.UkAndForeignPropertyAbout, JourneyStatus.Completed)))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe taskSections(JourneyStatus.Completed)
      }

      "use Journey Answers when no HMRC held data in IF" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Left(DataNotFoundError))

        await(testOnlyAdd(journeyResponse(IncomeSourceId("123456"), JourneyName.About, JourneyStatus.Completed)))
        await(testOnlyAdd(journeyResponse(IncomeSourceId("123457"), JourneyName.ForeignPropertySelectCountry, JourneyStatus.Completed)))
        await(testOnlyAdd(journeyResponse(IncomeSourceId("123458"), JourneyName.UkAndForeignPropertyAbout, JourneyStatus.Completed)))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe taskSections(JourneyStatus.Completed)
      }
    }

    "Only UkProperty has Journey Answers defined" should {
      "use Journey Answers when HMRC held data in IF for Uk" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Right(BusinessDetailsResponse(someUkProperty)))

        await(testOnlyAdd(journeyResponse(IncomeSourceId("123456"), JourneyName.About, JourneyStatus.Completed)))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe ukTaskSections(JourneyStatus.Completed)
      }

      "use mix of both Journey and IF data when IF contains all data" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Right(BusinessDetailsResponse(someProperty)))

        await(testOnlyAdd(journeyResponse(IncomeSourceId("123456"), JourneyName.About, JourneyStatus.Completed)))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe mixedTaskSections
      }

      "use Journey Answers when no HMRC held data in IF" in new Test {
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Left(DataNotFoundError))

        await(testOnlyAdd(journeyResponse(IncomeSourceId("123456"), JourneyName.About, JourneyStatus.Completed)))

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe ukTaskSections(JourneyStatus.Completed)
      }
    }

    "Multiple users have data in journey answers" should {
      "return Journey Answers of user with the matching mtditid" in new Test{
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Left(DataNotFoundError))

        val otherUser: Future[Unit] = testOnlyAdd(journeyResponse(IncomeSourceId("123456"), JourneyName.About, JourneyStatus.Completed, mtditid = "12346"))
        val currentUser: Future[Unit] = testOnlyAdd(journeyResponse(IncomeSourceId("123456"), JourneyName.About, JourneyStatus.InProgress))

        await(otherUser)
        await(currentUser)

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe ukTaskSections(JourneyStatus.InProgress)
      }
    }

    "Same user has multiple entries for the same journey" should {
      "return Journey Answers of Journey with the matching taxYear" in new Test{
        await(removeAll(repository.collection))
        mockGetBusinessDetails(nino, Left(DataNotFoundError))

        val otherUser: Future[Unit] = testOnlyAdd(journeyResponse(IncomeSourceId("123456"), JourneyName.About, JourneyStatus.Completed, taxyear = 2023))
        val currentUser: Future[Unit] = testOnlyAdd(journeyResponse(IncomeSourceId("123456"), JourneyName.About, JourneyStatus.InProgress))

        await(otherUser)
        await(currentUser)

        val underTest: Future[Seq[TaskListSection]] = service.get(taxYear, nino, mtdItId)

        await(underTest) shouldBe ukTaskSections(JourneyStatus.InProgress)
      }
    }
  }
}
