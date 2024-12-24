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

import models._
import models.common._
import models.domain.{FetchedForeignPropertyData, FetchedPropertyData, FetchedUKPropertyData, JourneyAnswers, JourneyWithStatus}
import models.repository.ForeignMerger.ForeignPropertyTaxMerger
import models.repository.Merger._
import models.repository.ForeignMerger._
import models.request._
import models.request.esba.{EsbaInUpstream, EsbaInfo, EsbaInfoToSave}
import models.request.foreign._
import models.request.sba.{SbaInfo, SbaInfoToSave}
import models.request.ukrentaroom.RaRAdjustments
import models.responses._

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class MergeService @Inject() (implicit
  ec: ExecutionContext
) {
  def mergeAll(
    resultFromAnnualDownstream: PropertyAnnualSubmission,
    resultFromPeriodicDownstreamMaybe: Option[PropertyPeriodicSubmission],
    resultFromRepository: Map[String, JourneyAnswers],
    foreignResultFromRepository: Map[String, Map[String, JourneyAnswers]]
  ): FetchedPropertyData = {
    val esbaInfoMaybe =
      mergeEsbaInfo(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalESBA.entryName))

    val esbaInfoRentalsAndRaRMaybe =
      mergeEsbaInfo(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalsAndRaRESBA.entryName))

    val sbaInfoMaybe =
      mergeSbaInfo(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalSBA.entryName))

    val sbaInfoAndRaRMaybe =
      mergeSbaInfo(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalsAndRaRESBA.entryName))

    val propertyAboutMaybe = mergePropertyAbout(resultFromRepository.get(JourneyName.About.entryName))

    val foreignPropertySelectCountryMaybe = mergeForeignPropertySelectCountry(
      resultFromRepository.get(JourneyName.ForeignPropertySelectCountry.entryName)
    )

    val propertyRentalsAboutMaybe = mergePropertyRentalsAbout(
      resultFromRepository.get(JourneyName.RentalAbout.entryName)
    )

    val rentalsAndRaRAboutMaybe = mergeRentalsAndRaRAbout(
      resultFromAnnualDownstream,
      resultFromPeriodicDownstreamMaybe,
      resultFromRepository.get(JourneyName.RentalsAndRaRAbout.entryName)
    )

    val rentalsAdjustmentsMaybe =
      mergeAdjustments(
        resultFromAnnualDownstream,
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalAdjustments.entryName)
      )
    val adjustmentsMaybe = rentalsAdjustmentsMaybe.map(
      _.copy(
        propertyIncomeAllowance =
          Some(rentalsAdjustmentsMaybe.flatMap(_.propertyIncomeAllowance).getOrElse(0)), // Todo: Revisit
        unusedResidentialFinanceCost =
          Some(rentalsAdjustmentsMaybe.flatMap(_.unusedResidentialFinanceCost).getOrElse(0)) // Todo: Revisit
      )
    )
    val adjustmentsRentalsAndRaRMaybe =
      mergeAdjustments(
        resultFromAnnualDownstream,
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalsAndRaRAdjustments.entryName)
      )

    val allowancesMaybe =
      mergeAllowances(resultFromAnnualDownstream, resultFromRepository.get(JourneyName.RentalAllowances.entryName))

    val allowancesRentalsAndRaRMaybe =
      mergeAllowances(
        resultFromAnnualDownstream,
        resultFromRepository.get(JourneyName.RentalsAndRaRAllowances.entryName)
      )

    val rentalsIncomeMaybe =
      mergeRentalsIncome(
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalIncome.entryName)
      )

    val rentalsAndRaRIncomeMaybe =
      mergeRentalsAndRaRIncome(
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalsAndRaRIncome.entryName)
      )

    val rentalsExpensesMaybe =
      mergeRentalsExpenses(
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalExpenses.entryName)
      )

    val rentalsAndRaRExpensesMaybe =
      mergeRentalsExpenses(
        resultFromPeriodicDownstreamMaybe,
        resultFromRepository.get(JourneyName.RentalsAndRaRExpenses.entryName)
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

    val journeyStatuses = mergeStatuses(resultFromRepository)

    val foreignPropertyTaxMaybe = mergeForeignPropertyTax(
      resultFromPeriodicDownstreamMaybe,
      foreignResultFromRepository.get(JourneyName.ForeignPropertyTax.entryName)
    )

    val foreignPropertyIncomeMaybe = mergeForeignPropertyIncome(
      resultFromPeriodicDownstreamMaybe,
      foreignResultFromRepository.get(JourneyName.ForeignPropertyIncome.entryName)
    )

    val foreignJourneyStatuses = mergeForeignStatuses(foreignResultFromRepository)

    val fetchedUKPropertyData = FetchedUKPropertyData(
      None,
      propertyAboutMaybe,
      propertyRentalsAboutMaybe,
      rentalsAndRaRAboutMaybe,
      adjustmentsMaybe,
      rentalsAndRaRAdjustments = adjustmentsRentalsAndRaRMaybe,
      allowancesMaybe,
      rentalsAndRaRAllowances = allowancesRentalsAndRaRMaybe,
      esbaInfoMaybe,
      rentalsAndRaREsbasWithSupportingQuestions = esbaInfoRentalsAndRaRMaybe,
      sbasWithSupportingQuestions = sbaInfoMaybe,
      rentalsAndRaRSbasWithSupportingQuestions = sbaInfoAndRaRMaybe,
      rentalsIncomeMaybe,
      rentalsAndRaRIncomeMaybe,
      rentalsExpensesMaybe,
      rentalsAndRaRExpenses = rentalsAndRaRExpensesMaybe,
      raRAbout = raRAboutMaybe,
      rarExpenses = rarExpensesMaybe,
      raRAdjustments = raRAdjustmentsMaybe,
      rentARoomAllowances = rentARoomAllowancesMaybe,
      journeyStatuses = journeyStatuses,
      foreignPropertySelectCountry = foreignPropertySelectCountryMaybe
    )

    val fetchedForeignPropertyData = FetchedForeignPropertyData(
      foreignPropertyTax = foreignPropertyTaxMaybe,
      foreignPropertyIncome = foreignPropertyIncomeMaybe,
      foreignJourneyStatuses = foreignJourneyStatuses
    )

    FetchedPropertyData(
      ukPropertyData = fetchedUKPropertyData,
      foreignPropertyData = fetchedForeignPropertyData
    )
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

    val propertyRentalsIncome: Option[PropertyRentalsIncome] =
      rentalsIncomeStoreAnswers.merge(ukOtherPropertyIncome)(RentalsIncomeMerger)
    propertyRentalsIncome
  }

  def mergeRentalsAndRaRIncome(
    resultFromDownstream: Option[PropertyPeriodicSubmission],
    resultFromRepository: Option[JourneyAnswers]
  ): Option[RentalsAndRaRIncome] = {
    val rentalsIncomeStoreAnswers: Option[StoredIncome] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[StoredIncome])
      case None                 => None
    }
    val ukOtherPropertyIncome: Option[UkOtherPropertyIncome] =
      resultFromDownstream.flatMap(_.ukOtherProperty.flatMap(_.income))

    val rentalsAndRaRIncome: Option[RentalsAndRaRIncome] =
      rentalsIncomeStoreAnswers.merge(ukOtherPropertyIncome)(RentalsAndRaRIncomeMerger)
    rentalsAndRaRIncome
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

  def mergeForeignPropertySelectCountry(
    resultFromRepository: Option[JourneyAnswers]
  ): Option[ForeignPropertySelectCountry] =
    resultFromRepository match {
      case Some(journeyAnswers: JourneyAnswers) => Some(journeyAnswers.data.as[ForeignPropertySelectCountry])
      case None                                 => None
    }

  def mergePropertyRentalsAbout(resultFromRepository: Option[JourneyAnswers]): Option[PropertyRentalsAbout] =
    resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[PropertyRentalsAbout])
      case None                 => None
    }

  def mergeRentalsAndRaRAbout(
    resultFromDownstream: PropertyAnnualSubmission,
    resultFromPeriodicDownstreamMaybe: Option[PropertyPeriodicSubmission],
    resultFromRepository: Option[JourneyAnswers]
  ): Option[RentalsAndRaRAbout] = {
    val claimPropertyIncomeAllowanceYesOrNo: Option[ClaimPropertyIncomeAllowanceYesOrNo] = resultFromRepository match {
      case Some(journeyAnswers) => Some(journeyAnswers.data.as[ClaimPropertyIncomeAllowanceYesOrNo])
      case None                 => None
    }
    val claimExpensesOrRRRYesNo: Option[ClaimExpensesOrRRRYesNo] = resultFromRepository match {
      case Some(journeyAnswers) => journeyAnswers.data.asOpt[ClaimExpensesOrRRRYesNo]
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

    val rentalsAndRaRAboutStoreAnswers = (claimPropertyIncomeAllowanceYesOrNo, claimExpensesOrRRRYesNo)
    rentalsAndRaRAboutStoreAnswers.merge((jointlyLet, uKOtherPropertyMaybe))
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

  def mergeStatuses(resultFromRepository: Map[String, JourneyAnswers]): List[JourneyWithStatus] =
    JourneyName.values.toList.flatMap(journeyName =>
      resultFromRepository
        .get(journeyName.entryName)
        .map(journeyAnswers => JourneyWithStatus(journeyName.entryName, journeyAnswers.status.entryName))
    )

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

  def mergeForeignPropertyTax(
    resultFromDownstream: Option[PropertyPeriodicSubmission],
    foreignResultFromRepository: Option[Map[String, JourneyAnswers]]
  ): Option[Map[String, ForeignPropertyTax]] = {
    val foreignPropertyTaxStoreAnswers: Option[Map[String, ForeignPropertyTaxStoreAnswers]] =
      foreignResultFromRepository match {
        case Some(journeyAnswers) => Some(journeyAnswers.map {
          case (countryCode, storeAnswers) => countryCode -> storeAnswers.data.as[ForeignPropertyTaxStoreAnswers]
        })
        case _ => None
      }

    val foreignPropertiesIncome: Option[Map[String, ForeignPropertyIncome]] = for {
      rfd <- resultFromDownstream
      fp <- rfd.foreignProperty
    } yield Map(fp.flatMap(fp => fp.income.map(fpIncome => fp.countryCode -> fpIncome)):_*)

    foreignPropertyTaxStoreAnswers.merge(foreignPropertiesIncome)
  }

  def mergeForeignPropertyIncome(
    resultFromDownstream: Option[PropertyPeriodicSubmission],
    foreignResultFromRepository: Option[Map[String, JourneyAnswers]]
  ): Option[Map[String, ForeignIncomeAnswers]] = {
    val foreignPropertyIncomeStoreAnswers: Option[Map[String, ForeignIncomeStoreAnswers]] =
      foreignResultFromRepository match {
        case Some(journeyAnswers) => Some(journeyAnswers.map {
          case (countryCode, storeAnswers) => countryCode -> storeAnswers.data.as[ForeignIncomeStoreAnswers]
        })
        case _ => None
      }

    val foreignPropertiesIncome: Option[Map[String, ForeignPropertyIncome]] = for {
      rfd <- resultFromDownstream
      fp <- rfd.foreignProperty
    } yield Map(fp.flatMap(fp => fp.income.map(fpIncome => fp.countryCode -> fpIncome)):_*)

    foreignPropertyIncomeStoreAnswers.merge(foreignPropertiesIncome)
  }

  def mergeForeignStatuses(foreignResultFromRepository: Map[String, Map[String, JourneyAnswers]]): Option[Map[String, List[JourneyWithStatus]]] = {
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
