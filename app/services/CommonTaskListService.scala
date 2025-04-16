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

import config.AppConfig
import models.common.{JourneyName, JourneyStatus}
import models.domain.JourneyAnswers
import models.errors.DataNotFoundError
import models.prePopulation.PrePopulationResponse
import models.taskList._
import play.api.Logging
import repositories.MongoJourneyAnswersRepository
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CommonTaskListService @Inject()(appConfig: AppConfig,
                                      service: BusinessDetailsService,
                                      repository: MongoJourneyAnswersRepository) extends Logging {

  def get(taxYear: Int, nino: String, mtdItId: String)
         (implicit ec: ExecutionContext,hc: HeaderCarrier): Future[Seq[TaskListSection]] = {

    val ukPropertyUrl: String = s"${appConfig.propertyFrontendUrl}/$taxYear/uk-property/about/start"
    val foreignPropertyUrl: String = s"${appConfig.propertyFrontendUrl}/$taxYear/foreign-property/about/start"
    val ukForeignPropertyUrl: String = s"${appConfig.propertyFrontendUrl}/$taxYear/uk-foreign-property/about/start"

    def result(): Future[Seq[TaskListSection]] = {
      for {
        allBusinessDataOpt <- service.getBusinessDetails(nino).map {
          case Right(businessDetails)  => PrePopulationResponse.fromData(businessDetails)
          case Left(DataNotFoundError) => PrePopulationResponse.noPrePop
          case Left(err)               =>
            logger.warn(s"[CommonTaskListService][result] - An error occurred while checking the user's Property data for pre-pop $err")
            PrePopulationResponse.noPrePop
        }
        allJourneys <- repository.fetchAllJourneysUserTaxYear(taxYear, mtdItId)
        ukJourneyAnswersOpt = allJourneys.find(_.journey == JourneyName.About)
        foreignJourneyAnswersOpt = allJourneys.find(_.journey == JourneyName.ForeignPropertySelectCountry)
        ukAndForeignJourneyAnswersOpt = allJourneys.find(_.journey == JourneyName.UkAndForeignPropertyAbout)
        taskList = toTaskList(allBusinessDataOpt, ukJourneyAnswersOpt, foreignJourneyAnswersOpt, ukAndForeignJourneyAnswersOpt)
      } yield taskList
    }

    def toTaskList(prePopulationResponse: PrePopulationResponse,
                   ukPropertyJourneyAnswersOpt: Option[JourneyAnswers],
                   foreignPropertyJourneyAnswersOpt: Option[JourneyAnswers],
                   ukAndForeignPropertyJourneyAnswersOpt: Option[JourneyAnswers]): Seq[TaskListSection] = {
      val ukPropertyTask: Option[Seq[TaskListSectionItem]] = getPropertyTasks(
        ifPropertyExists = prePopulationResponse.hasUkPropertyPrePop,
        taskTitle = TaskTitle.UkProperty,
        url = ukPropertyUrl,
        journeyAnswers = ukPropertyJourneyAnswersOpt
      )
      val foreignPropertyTask: Option[Seq[TaskListSectionItem]] = getPropertyTasks(
        ifPropertyExists = prePopulationResponse.hasForeignPropertyPrePop,
        taskTitle = TaskTitle.ForeignProperty,
        url = foreignPropertyUrl,
        journeyAnswers = foreignPropertyJourneyAnswersOpt
      )
      val ukForeignPropertyTask: Option[Seq[TaskListSectionItem]] = getPropertyTasks(
        ifPropertyExists = prePopulationResponse.hasUkPropertyPrePop && prePopulationResponse.hasForeignPropertyPrePop,
        taskTitle = TaskTitle.UkForeignProperty,
        url = ukForeignPropertyUrl,
        journeyAnswers = ukAndForeignPropertyJourneyAnswersOpt
      )
      Seq(
        TaskListSection(SectionTitle.UkPropertyTitle, ukPropertyTask),
        TaskListSection(SectionTitle.ForeignPropertyTitle, foreignPropertyTask),
        TaskListSection(SectionTitle.UkForeignPropertyTitle, ukForeignPropertyTask)
      )
    }

    result()
  }

  private def getPropertyTasks(ifPropertyExists: Boolean,
                                   taskTitle: TaskTitle,
                                   url: String,
                                   journeyAnswers: Option[JourneyAnswers]): Option[Seq[TaskListSectionItem]] = {
    val loggingTitle = taskTitle.entryName.replace("Title","")
    (ifPropertyExists, journeyAnswers) match {
      case (_, Some(journeyAnswers)) =>
        logger.info(s"[CommonTaskListService][getPropertyTasks] - $loggingTitle - User has journey answers, setting status to: ${journeyAnswers.status}")
        Some(Seq(TaskListSectionItem(taskTitle, journeyAnswers.status, Some(url))))
      case (true, _) =>
        logger.info(s"[CommonTaskListService][getPropertyTasks] - $loggingTitle - User has no journey answers but does have PrePopData, setting status to ${JourneyStatus.CheckNow}")
        Some(Seq(TaskListSectionItem(taskTitle, JourneyStatus.CheckNow, Some(url))))
      case (_, _) =>
        logger.info(s"[CommonTaskListService][getPropertyTasks] - $loggingTitle - User has no journey or PrePopData, returning None")
        None
    }
  }
}
