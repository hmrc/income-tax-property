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

package controllers

import models.common.JourneyStatus
import models.taskList.{SectionTitle, TaskListSection, TaskListSectionItem, TaskTitle}
import org.scalamock.handlers.CallHandler5
import play.api.http.Status.OK
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers.{contentAsJson, status}
import services.CommonTaskListService
import uk.gov.hmrc.http.HeaderCarrier
import utils.mocks.MockAuthorisedAction
import utils.providers.FakeRequestProvider
import utils.{ControllerUnitTest, TaxYearUtils}

import scala.concurrent.{ExecutionContext, Future}

class CommonTaskListControllerSpec extends ControllerUnitTest with MockAuthorisedAction with FakeRequestProvider {
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
  val nino: String = "123456789"
  val taxYear: Int = TaxYearUtils.taxYear

  val commonTaskListService: CommonTaskListService = mock[CommonTaskListService]
  val controller = new CommonTaskListController(commonTaskListService, mockAuthorisedAction, cc)

  def taskListSection(title: SectionTitle, section: Option[Seq[TaskListSectionItem]]): TaskListSection = TaskListSection(title, section)

  val ukItem: Option[Seq[TaskListSectionItem]] = Some(Seq(TaskListSectionItem(TaskTitle.UkProperty, JourneyStatus.Completed, Some("TEST"))))
  val foreignItem: Option[Seq[TaskListSectionItem]] = Some(Seq(TaskListSectionItem(TaskTitle.ForeignProperty, JourneyStatus.Completed, Some("TEST"))))
  val ukForeignItem: Option[Seq[TaskListSectionItem]] = Some(Seq(TaskListSectionItem(TaskTitle.UkForeignProperty, JourneyStatus.Completed, Some("TEST"))))

  val returnJsonNone: JsValue = Json.parse(
    """
      |[
      |  {
      |    "sectionTitle": "UkProperty"
      |  },
      |  {
      |    "sectionTitle": "ForeignProperty"
      |  },
      |  {
      |    "sectionTitle": "UkForeignProperty"
      |  }
      |]
      |""".stripMargin
  )

  val returnJsonSome: JsValue = Json.parse(
    """
      |[
      |  {
      |    "sectionTitle": "UkProperty",
      |    "taskItems": [
      |      {
      |        "title": "UkPropertyTitle",
      |        "status": "completed",
      |        "href": "TEST"
      |      }
      |    ]
      |  },
      |  {
      |    "sectionTitle": "ForeignProperty",
      |    "taskItems": [
      |      {
      |        "title": "ForeignPropertyTitle",
      |        "status": "completed",
      |        "href": "TEST"
      |      }
      |    ]
      |  },
      |  {
      |    "sectionTitle": "UkForeignProperty",
      |    "taskItems": [
      |      {
      |        "title": "UkForeignPropertyTitle",
      |        "status": "completed",
      |        "href": "TEST"
      |      }
      |    ]
      |  }
      |]
      |""".stripMargin
  )

  def mockStateBenefitsService(taskListSection: Seq[TaskListSection]): CallHandler5[Int, String, String, ExecutionContext, HeaderCarrier, Future[Seq[TaskListSection]]] = {
    (commonTaskListService.get(_: Int, _: String, _: String)(_: ExecutionContext, _: HeaderCarrier))
      .expects(*, *, *, *, *)
      .returning(Future.successful(taskListSection))
  }

  ".getCommonTaskList" should {
    "return a task list section model for None returns" in {
      val result = {
        mockAuthorisation()
        mockStateBenefitsService(Seq(
          taskListSection(SectionTitle.UkPropertyTitle, None),
          taskListSection(SectionTitle.ForeignPropertyTitle, None),
          taskListSection(SectionTitle.UkForeignPropertyTitle, None)
        ))
        controller.getCommonTaskList(taxYear, nino)(fakeRequest)
      }

      status(result) shouldBe OK
      contentAsJson(result) shouldBe returnJsonNone
    }

    "return a task list section model for Some() returns" in {
      val result = {
        mockAuthorisation()
        mockStateBenefitsService(Seq(
          taskListSection(SectionTitle.UkPropertyTitle, ukItem),
          taskListSection(SectionTitle.ForeignPropertyTitle, foreignItem),
          taskListSection(SectionTitle.UkForeignPropertyTitle, ukForeignItem)
        ))
        controller.getCommonTaskList(taxYear, nino)(fakeRequest)
      }

      status(result) shouldBe OK
      contentAsJson(result) shouldBe returnJsonSome
    }
  }
}
