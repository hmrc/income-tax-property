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

import connectors.IntegrationFrameworkConnector
import models._
import models.common._
import models.domain.JourneyAnswers
import models.repository.Merger._
import models.request._
import models.request.esba.{EsbaInUpstream, EsbaInfo, EsbaInfoToSave}
import models.request.sba.{SbaInfo, SbaInfoToSave}
import models.request.ukrentaroom.RaRAdjustments
import models.responses._
import repositories.MongoJourneyAnswersRepository

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class MergeService @Inject() (connector: IntegrationFrameworkConnector, repository: MongoJourneyAnswersRepository)(
  implicit ec: ExecutionContext
) {
  def mergeAll(
    resultFromAnnualDownstream: PropertyAnnualSubmission,
    resultFromPeriodicDownstreamMaybe: Option[PropertyPeriodicSubmission],
    resultFromRepository: Map[String, JourneyAnswers]
  ): FetchedPropertyData = {
    val esbaInfoMaybe =
      mergeEsbaInfo(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalESBA.entryName))

    val sbaInfoMaybe =
      mergeSbaInfo(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalSBA.entryName))

    val propertyAboutMaybe = mergePropertyAbout(resultFromRepository.get(JourneyName.About.entryName))

    val propertyRentalsAboutMaybe = mergePropertyRentalsAbout(
      resultFromRepository.get(JourneyName.RentalAbout.entryName)
    )

    val adjustmentsMaybe =
      mergeAdjustments(
        resultFromAnnualDownstream,
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalAdjustments.entryName)
      )

    val allowancesMaybe =
      mergeAllowances(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalAllowances.entryName))

    val rentalsIncomeMaybe =
      mergeRentalsIncome(
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalIncome.entryName)
      )

    val rentalsExpensesMaybe =
      mergeRentalsExpenses(
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalExpenses.entryName)
      )

    val rarExpensesMaybe = mergeRaRExpenses(
      resultFromPeriodicDownstreamMaybe,
      resultFromRepository.get(JourneyName.RentARoomExpenses.entryName)
    )

    val raRAdjustmentsMaybe = mergeRaRAdjustments(
      resultFromAnnualDownstream,
      resultFromPeriodicDownstreamMaybe,
      resultFromRepository.get(JourneyName.RentARoomAdjustments.entryName)
    )

    val rentARoomAllowancesMaybe = mergeRaRAllowances(
      resultFromAnnualDownstream,
      resultFromRepository.get(JourneyName.RentARoomAllowances.entryName)
    )

    val raRAboutMaybe = mergeRaRAbout(
      resultFromAnnualDownstream,
      resultFromPeriodicDownstreamMaybe,
      resultFromRepository.get(JourneyName.RentARoomAbout.entryName)
    )

    val data = FetchedPropertyData(
      None,
      propertyAboutMaybe,
      propertyRentalsAboutMaybe,
      adjustmentsMaybe,
      allowancesMaybe,
      esbaInfoMaybe,
      sbaInfoMaybe,
      rentalsIncomeMaybe,
      rentalsExpensesMaybe,
      raRAbout = raRAboutMaybe,
      rarExpenses = rarExpensesMaybe,
      raRAdjustments = raRAdjustmentsMaybe,
      rentARoomAllowances = rentARoomAllowancesMaybe
    )

    data
  }

  def mergeRentalsIncome(
    resultFromDownstream: Option[PropertyPeriodicSubmission],
    resultFromRepository: Option[JourneyAnswers]
  ): Option[PropertyRentalsIncome] = {
    val rentalsIncomeStoreAnswers: Option[StoredIncome] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[StoredIncome])
      case None                 => None
    }
    val ukOtherPropertyIncome: Option[UkOtherPropertyIncome] =
      resultFromDownstream.flatMap(_.ukOtherProperty.flatMap(_.income))

    rentalsIncomeStoreAnswers.merge(ukOtherPropertyIncome)
  }

  def mergeRentalsExpenses(
    resultFromDownstream: Option[PropertyPeriodicSubmission],
    resultFromRepository: Option[JourneyAnswers]
  ): Option[PropertyRentalsExpense] = {
    val rentalsExpensesStoreAnswers: Option[ExpensesStoreAnswers] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[ExpensesStoreAnswers])
      case None                 => None
    }
    val ukOtherPropertyExpenses: Option[UkOtherPropertyExpenses] =
      resultFromDownstream.flatMap(_.ukOtherProperty.flatMap(_.expenses))

    rentalsExpensesStoreAnswers.merge(ukOtherPropertyExpenses)
  }

  def mergeRaRExpenses(
    resultFromDownstream: Option[PropertyPeriodicSubmission],
    resultFromRepository: Option[JourneyAnswers]
  ): Option[RentARoomExpenses] = {
    val raRExpensesStoreAnswers: Option[RentARoomExpensesStoreAnswers] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[RentARoomExpensesStoreAnswers])
      case None                 => None
    }
    val ukOtherPropertyExpenses: Option[UkOtherPropertyExpenses] =
      resultFromDownstream.flatMap(_.ukOtherProperty.flatMap(_.expenses))

    raRExpensesStoreAnswers.merge(ukOtherPropertyExpenses)
  }

  def mergeAllowances(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromRepository: Option[JourneyAnswers]
  ): Option[RentalAllowances] = {
    val allowancesStoreAnswers: Option[RentalAllowancesStoreAnswers] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[RentalAllowancesStoreAnswers])
      case None                 => None
    }
    val ukOtherPropertyAnnualAllowances: Option[UkOtherAllowances] =
      resultFromDownstream.ukOtherProperty.flatMap(_.ukOtherPropertyAnnualAllowances)
    allowancesStoreAnswers.merge(ukOtherPropertyAnnualAllowances)
  }

  def mergeRaRAllowances(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromRepository: Option[JourneyAnswers]
  ): Option[RentARoomAllowances] = {
    val rentARoomAllowancesStoreAnswers: Option[RentARoomAllowancesStoreAnswers] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[RentARoomAllowancesStoreAnswers])
      case None                 => None
    }
    val ukOtherPropertyAnnualAllowances: Option[UkOtherAllowances] =
      resultFromDownstream.ukOtherProperty.flatMap(_.ukOtherPropertyAnnualAllowances)
    rentARoomAllowancesStoreAnswers.merge(ukOtherPropertyAnnualAllowances)
  }

  def mergePropertyAbout(resultFromRepository: Option[JourneyAnswers]): Option[PropertyAbout] =
    resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[PropertyAbout])
      case None                 => None
    }

  def mergePropertyRentalsAbout(resultFromRepository: Option[JourneyAnswers]): Option[PropertyRentalsAbout] =
    resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[PropertyRentalsAbout])
      case None                 => None
    }

  def mergeRaRAbout(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromPeriodicDownstreamMaybe: Option[PropertyPeriodicSubmission],
    resultFromRepository: Option[JourneyAnswers]
  ): Option[RaRAbout] = {
    val rentARoomAllowancesStoreAnswers: Option[ClaimExpensesOrRRRYesNo] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[ClaimExpensesOrRRRYesNo])
      case None                 => None
    }
    val jointlyLet = for {
      uop    <- resultFromDownstream.ukOtherProperty
      ukopaa <- uop.ukOtherPropertyAnnualAdjustments
      ukorar <- ukopaa.ukOtherRentARoom
    } yield ukorar.jointlyLet

    val uKOtherPropertyMaybe: Option[UkOtherProperty] = for {
      resultFromPeriodicDownstream <- resultFromPeriodicDownstreamMaybe
      ukop                         <- resultFromPeriodicDownstream.ukOtherProperty
    } yield ukop

    rentARoomAllowancesStoreAnswers.merge((jointlyLet, uKOtherPropertyMaybe))
  }

  def mergeAdjustments(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromPeriodicDownstreamMaybe: Option[PropertyPeriodicSubmission],
    resultFromRepository: Option[JourneyAnswers]
  ): Option[PropertyRentalAdjustments] = {

    val adjustmentsAndPeriodicExpenses: Option[(UkOtherAdjustments, UkOtherPropertyExpenses)] = for {
      uopAnnual                    <- resultFromDownstream.ukOtherProperty
      resultFromDownstreamPeriodic <- resultFromPeriodicDownstreamMaybe // revisit. None Periodic what to do
      uopPeriodic                  <- resultFromDownstreamPeriodic.ukOtherProperty
      expensesPeriodic             <- uopPeriodic.expenses
      uopaa                        <- uopAnnual.ukOtherPropertyAnnualAdjustments
    } yield (uopaa, expensesPeriodic)

    val adjustmentStoreAnswers: Option[AdjustmentStoreAnswers] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[AdjustmentStoreAnswers])
      case None                 => None
    }

    adjustmentStoreAnswers.merge(adjustmentsAndPeriodicExpenses)
  }

  def mergeRaRAdjustments(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromPeriodicDownstreamMaybe: Option[PropertyPeriodicSubmission],
    resultFromRepository: Option[JourneyAnswers]
  ): Option[RaRAdjustments] = {

    val raRAdjustments: Option[(UkOtherAdjustments, UkOtherPropertyExpenses)] = for {
      uopAnnual                    <- resultFromDownstream.ukOtherProperty
      uopaa                        <- uopAnnual.ukOtherPropertyAnnualAdjustments
      resultFromPeriodicDownstream <- resultFromPeriodicDownstreamMaybe
      ukOtherProperty              <- resultFromPeriodicDownstream.ukOtherProperty
      ukOtherPropertyExpenses      <- ukOtherProperty.expenses
    } yield (uopaa, ukOtherPropertyExpenses)

    val raRAdjustmentStoreAnswers: Option[RaRBalancingChargeYesNo] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[RaRBalancingChargeYesNo])
      case None                 => None
    }

    raRAdjustmentStoreAnswers.merge(raRAdjustments)
  }

  def mergeEsbaInfo(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromRepository: Option[JourneyAnswers]
  ): Option[EsbaInfo] = {
    val esbasMaybe: Option[List[Esba]] = for {
      ukop   <- resultFromDownstream.ukOtherProperty
      ukopaa <- ukop.ukOtherPropertyAnnualAllowances
      esba   <- ukopaa.enhancedStructuredBuildingAllowance
    } yield esba.toList

    val esbasInRequestMaybe: Option[List[EsbaInUpstream]] = esbasMaybe.flatMap(
      EsbaInUpstream.fromEsbasToEsbasInUpstream
    )

    val esbaInfoSavedInRepositoryMaybe: Option[EsbaInfoToSave] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[EsbaInfoToSave])
      case None                 => None
    }

    esbaInfoSavedInRepositoryMaybe.merge(esbasInRequestMaybe)
  }

  def mergeSbaInfo(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromRepository: Option[JourneyAnswers]
  ): Option[SbaInfo] = {
    val sbasMaybe: Option[List[StructuredBuildingAllowance]] = for {
      ukop   <- resultFromDownstream.ukOtherProperty
      ukopaa <- ukop.ukOtherPropertyAnnualAllowances
      sba    <- ukopaa.structuredBuildingAllowance
    } yield sba.toList

    val sbaInfoSavedInRepositoryMaybe: Option[SbaInfoToSave] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[SbaInfoToSave])
      case None                 => None
    }

    sbaInfoSavedInRepositoryMaybe.merge(sbasMaybe)
  }

}
