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

import models.common._
import models.domain.{ForeignFetchedPropertyData, JourneyAnswers, JourneyWithStatus}
import models.repository.ForeignMerger._
import models.repository.Merger.GeneralMerger
import models.request.foreign.ForeignPropertyTax
import models.responses._

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class ForeignMergeService @Inject()(implicit
                                    ec: ExecutionContext
) {
  def mergeAll(
    resultFromAnnualDownstream: PropertyAnnualSubmission,
    resultFromPeriodicDownstreamMaybe: Option[PropertyPeriodicSubmission],
    foreignResultFromRepository: Map[String, Map[String, JourneyAnswers]]
  ): ForeignFetchedPropertyData = {

    val foreignPropertyTaxMaybe = mergeForeignPropertyTax(
      resultFromPeriodicDownstreamMaybe,
      foreignResultFromRepository.get(JourneyName.ForeignPropertyTax.entryName)
    )

    val foreignJourneyStatuses = foreignMergeStatuses(foreignResultFromRepository)

    val data = ForeignFetchedPropertyData(
      foreignPropertyTax = foreignPropertyTaxMaybe,
      foreignJourneyStatuses = foreignJourneyStatuses
    )

    data
  }

  def mergeForeignPropertyTax(
     resultFromDownstream: Option[PropertyPeriodicSubmission],
     foreignResultFromRepository: Option[Map[String, JourneyAnswers]]
 ): Option[Map[String, ForeignPropertyTax]] = {
    val foreignPropertyTaxStoreAnswers: Option[Map[String, ForeignPropertyTaxStoreAnswers]] =
      foreignResultFromRepository match {
        case Some(journeyAnswers) => Some(journeyAnswers.map {
        case (countryCode, storeAnswers) => countryCode -> storeAnswers.data.as[ForeignPropertyTaxStoreAnswers]
      })
      case None => None
    }

    val foreignPropertiesIncome: Option[Map[String, ForeignPropertyIncome]] =  for {
      rfd <- resultFromDownstream
      fp <- rfd.foreignProperty
    } yield Map(fp.flatMap(fp => fp.income.map(fpIncome => fp.countryCode -> fpIncome)):_*)

    foreignPropertyTaxStoreAnswers.merge(foreignPropertiesIncome)
  }

  def foreignMergeStatuses(foreignResultFromRepository: Map[String, Map[String, JourneyAnswers]]): Option[Map[String, List[JourneyWithStatus]]] = {
    val foreignJourneyStatusList: Seq[(String, JourneyWithStatus)] = foreignResultFromRepository.toList.flatMap {
      case (journeyName: String, foreignJourneyAnswers: Map[String, JourneyAnswers]) =>
        foreignJourneyAnswers.collect { case (countryCode, answers) =>
          countryCode -> JourneyWithStatus(journeyName, answers.status.entryName)
        }
    }

    val foreignJourneyStatusMap: Map[String, List[JourneyWithStatus]] = foreignJourneyStatusList.foldLeft(Map[String, List[JourneyWithStatus]]()) {
      case (acc, (countryCode: String, journey: JourneyWithStatus)) => acc.get(countryCode) match {
        case None => acc + (countryCode -> List(journey))
        case Some(journeys) => acc + (countryCode -> (journeys :+ journey))
      }
    }

    Option.when(foreignJourneyStatusMap.nonEmpty)(foreignJourneyStatusMap)
  }
}
