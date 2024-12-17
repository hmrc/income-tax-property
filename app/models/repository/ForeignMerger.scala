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

package models.repository

import models.request.foreign.{ForeignIncomeTax, ForeignPropertyTax}
import models.responses._

// T: to return (merge into)
// U: saved (extract from)
// X: from downstream


object ForeignMerger {

  implicit object ForeignPropertyTaxMerger
    extends Merger[Option[Map[String, ForeignPropertyTax]], Option[Map[String, ForeignPropertyTaxStoreAnswers]], Option[Map[String, ForeignPropertyIncome]]] {

    override def merge(
      extractedMaybe: Option[Map[String, ForeignPropertyTaxStoreAnswers]],
      fromDownstreamMaybe: Option[Map[String, ForeignPropertyIncome]]
    ): Option[Map[String, ForeignPropertyTax]] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (Some(extractedMap), Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignPropertyTax] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyIncome) => countryCode -> ForeignPropertyTax(
              foreignIncomeTax = Some(ForeignIncomeTax(
                foreignIncomeTaxYesNo = extractedMap.get(countryCode)
                  .flatMap(_.foreignIncomeTaxYesNo)
                  .getOrElse(foreignPropertyIncome.foreignTaxPaidOrDeducted.isDefined),
                foreignTaxPaidOrDeducted = foreignPropertyIncome.foreignTaxPaidOrDeducted
              )),
              foreignTaxCreditRelief = foreignPropertyIncome.foreignTaxCreditRelief
            )
          }
          Option.when(result.nonEmpty)(result)
        case (_, Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignPropertyTax] = fromDownstreamMap.map {
            case (countryCode, foreignPropertyIncome) => countryCode -> ForeignPropertyTax(
              foreignIncomeTax = Some(ForeignIncomeTax(
                foreignIncomeTaxYesNo = foreignPropertyIncome.foreignTaxPaidOrDeducted.isDefined,
                foreignTaxPaidOrDeducted = foreignPropertyIncome.foreignTaxPaidOrDeducted
              )),
              foreignTaxCreditRelief = foreignPropertyIncome.foreignTaxCreditRelief
            )
          }
          Option.when(result.nonEmpty)(result)
        case _ => None
      }
  }

}
