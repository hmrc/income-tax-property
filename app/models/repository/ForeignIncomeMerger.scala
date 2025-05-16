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

import models.ForeignIncomeDividendsStoreAnswers
import models.request.foreignincome.{ForeignDividendsAnswers, ForeignDividend}

// T: to return (merge into)
// U: saved (extract from)
// X: from downstream

object ForeignIncomeMerger {

  implicit object ForeignDividendsAnswersMerger
      extends Merger[Option[Map[String, ForeignDividendsAnswers]], Option[Map[String, ForeignIncomeDividendsStoreAnswers]
      ], Option[Map[String, ForeignDividend]]] {

    override def merge(
      extractedMaybe: Option[Map[String, ForeignIncomeDividendsStoreAnswers]],
      fromDownstreamMaybe: Option[Map[String, ForeignDividend]]
    ): Option[Map[String, ForeignDividendsAnswers]] =
      (extractedMaybe, fromDownstreamMaybe) match {
        case (_, Some(fromDownstreamMap)) =>
          val result: Map[String, ForeignDividendsAnswers] = fromDownstreamMap.map {
            case (countryCode, dividends) =>
              countryCode -> ForeignDividendsAnswers(
                amountBeforeTax = dividends.amountBeforeTax,
                taxTakenOff = dividends.taxTakenOff,
                specialWithholdingTax = dividends.specialWithholdingTax,
                foreignTaxCreditRelief = dividends.foreignTaxCreditRelief,
                taxableAmount = Some(dividends.taxableAmount)
              )
          }
          Option.when(result.nonEmpty)(result)
        case _ => None
      }
  }
  
}
