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

package models.prePopulation

import models.BusinessDetailsResponse
import play.api.libs.json.{Json, Writes}

case class PrePopulationResponse(hasUkPropertyPrePop: Boolean,
                                 hasForeignPropertyPrePop: Boolean
                                )

object PrePopulationResponse {
  implicit val writes: Writes[PrePopulationResponse] = Json.writes[PrePopulationResponse]

  private def checkProperty(propertySelector: String, data: BusinessDetailsResponse): Boolean = {
    data.propertyData.exists(_.incomeSourceType.getOrElse("") == propertySelector)
  }

  def fromData(data: BusinessDetailsResponse): PrePopulationResponse = {
    val ukPropertySelector :String = "uk-property"
    val foreignPropertySelector :String = "foreign-property"

    PrePopulationResponse(
      hasUkPropertyPrePop       = checkProperty(ukPropertySelector, data),
      hasForeignPropertyPrePop  = checkProperty(foreignPropertySelector, data)
    )
  }

  val noPrePop: PrePopulationResponse = PrePopulationResponse(
    hasUkPropertyPrePop       = false,
    hasForeignPropertyPrePop  = false
  )
}
