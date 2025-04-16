/*
 * Copyright 2024 HM Revenue & Customs
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

import cats.data.EitherT
import config.AppConfig
import models.BusinessDetailsResponse
import models.common.{IncomeSourceId, JourneyContext, JourneyName, Mtditid, TaxYear}
import models.domain.JourneyAnswers
import models.errors.ServiceError
import models.taskList.TaskStatus.{Completed, InProgress, NotStarted}
import models.taskList._
import play.api.Logging
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Instant
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CommonTaskListService @Inject()(appConfig: AppConfig,
                                      service: BusinessDetailsService,
                                      repository: MongoJourneyAnswersRepository) extends Logging {

  def get(taxYear: Int, nino: String, mtdItId: String, incomeSourceId: IncomeSourceId)
         (implicit ec: ExecutionContext,hc: HeaderCarrier): Future[Seq[TaskListSection]] = {

    val ukPropertyUrl: String = s"${appConfig.propertyFrontendUrl}/$taxYear/uk-property/about/start"
    val foreignPropertyUrl: String = s"${appConfig.propertyFrontendUrl}/$taxYear/foreign-property/about/start"
    val ukForeignPropertyUrl: String = s"${appConfig.propertyFrontendUrl}/$taxYear/uk-foreign-property/about/start"

    val ukPropertyCtx = JourneyContext(TaxYear(taxYear), incomeSourceId, Mtditid(mtdItId), JourneyName.About)
    val foreignPropertyCtx = JourneyContext(TaxYear(taxYear), incomeSourceId, Mtditid(mtdItId), JourneyName.ForeignPropertySelectCountry)
    val ukForeignPropertyCtx = JourneyContext(TaxYear(taxYear), incomeSourceId, Mtditid(mtdItId), JourneyName.UkAndForeignPropertyAbout)

    val emptyTaskList: Seq[TaskListSection] = Seq(
      TaskListSection(SectionTitle.UkPropertyTitle, None),
      TaskListSection(SectionTitle.ForeignPropertyTitle, None),
      TaskListSection(SectionTitle.UkForeignPropertyTitle, None)
    )

    //TODO Not quite liking this so far, might need to tweak the types we pass on to 'toTaskList()'
    def result: EitherT[Future, ServiceError, Seq[TaskListSection]] = for {
      allPropertyDataOpt <- EitherT(service.getBusinessDetails(nino))
      ukPropertyJourneyAnswersOpt <- EitherT.right(repository.fetch(ukPropertyCtx))
      foreignPropertyJourneyAnswersOpt <- EitherT.right(repository.fetch(foreignPropertyCtx))
      ukForeignPropertyJourneyAnswersOpt <- EitherT.right(repository.fetch(ukForeignPropertyCtx))
      taskList = toTaskList(allPropertyDataOpt, ukPropertyJourneyAnswersOpt, foreignPropertyJourneyAnswersOpt, ukForeignPropertyJourneyAnswersOpt)
    } yield taskList

    //TODO Refactor tasklist to function with property
    def toTaskList(allPropertyDataOpt: BusinessDetailsResponse,
                   ukPropertyJourneyAnswersOpt: Seq[JourneyAnswers],
                   foreignPropertyJourneyAnswersOpt: Seq[JourneyAnswers],
                   ukForeignPropertyJourneyAnswersOpt: Seq[JourneyAnswers]): Seq[TaskListSection] = {
      allPropertyDataOpt.fold(emptyTaskList)(allPropertyData => {
        val ukPropertyTask: Option[Seq[TaskListSectionItem]] = getStateBenefitTasks(
          hmrcAdded = allPropertyData.stateBenefitsData.flatMap(_.jobSeekersAllowances),
          customerAdded = allStateBenefitsData.customerAddedStateBenefitsData.flatMap(_.jobSeekersAllowances),
          taskTitle = TaskTitle.JSA,
          url = jsaUrl,
          journeyAnswers = jsaJourneyAnswersOpt
        )
        val foreignPropertyTask: Option[Seq[TaskListSectionItem]] = getStateBenefitTasks(
          hmrcAdded = allStateBenefitsData.stateBenefitsData.flatMap(_.employmentSupportAllowances),
          customerAdded = allStateBenefitsData.customerAddedStateBenefitsData.flatMap(_.employmentSupportAllowances),
          taskTitle = TaskTitle.ESA,
          url = esaUrl,
          journeyAnswers = esaJourneyAnswersOpt
        )
        val ukForeignPropertyTask: Option[Seq[TaskListSectionItem]] = getStateBenefitTasks(
          hmrcAdded = allStateBenefitsData.stateBenefitsData.flatMap(_.employmentSupportAllowances),
          customerAdded = allStateBenefitsData.customerAddedStateBenefitsData.flatMap(_.employmentSupportAllowances),
          taskTitle = TaskTitle.ESA,
          url = esaUrl,
          journeyAnswers = esaJourneyAnswersOpt
        )
        Seq(
          TaskListSection(SectionTitle.JsaTitle, jsaTask),
          TaskListSection(SectionTitle.EsaTitle, esaTask)
        )
      })
    }

    result.leftMap(_ => emptyTaskList).merge
  }

  //TODO: All of this will need refactoring to work for property
  private def getStateBenefitTasks(hmrcAdded: Option[Set[StateBenefit]],
                                   customerAdded: Option[Set[CustomerAddedStateBenefit]],
                                   taskTitle: TaskTitle,
                                   url: String,
                                   journeyAnswers: Option[JourneyAnswers]): Option[Seq[TaskListSectionItem]] = {
    val hmrcSubmittedOn: Instant =
      hmrcAdded.flatMap(
        _.headOption.flatMap(
          _.submittedOn
        )
      ).getOrElse(Instant.MIN)

    val customerSubmittedOn: Instant =
      customerAdded.flatMap(
        _.headOption.flatMap(
          _.submittedOn
        )
      ).getOrElse(Instant.MIN)

    val hmrcHeldDataNewer: Boolean = !hmrcSubmittedOn.isBefore(customerSubmittedOn)

    (hmrcAdded, customerAdded, journeyAnswers) match {
      case (Some(_), _, _) if hmrcHeldDataNewer =>
        Some(Seq(TaskListSectionItem(taskTitle, TaskStatus.CheckNow, Some(url))))
      case (_, _, Some(journeyAnswers)) =>
        val status: TaskStatus = journeyAnswers.data.value("status").validate[TaskStatus].asOpt match {
          case Some(TaskStatus.Completed) => Completed
          case Some(TaskStatus.InProgress) => InProgress
          case _ =>
            logger.info("[CommonTaskListService][getStatus] status stored in an invalid format, setting as 'Not yet started'.")
            NotStarted
        }
        Some(Seq(TaskListSectionItem(taskTitle, status, Some(url))))
      case (_, Some(_), _) =>
        Some(Seq(TaskListSectionItem(taskTitle, if(appConfig.sectionCompletedQuestionEnabled) TaskStatus.InProgress else TaskStatus.Completed, Some(url))))
      case (_, _, _) => None
    }
  }
}
