/*
 * Copyright 2023 HM Revenue & Customs
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

package models

import models.request.{ElectricChargePointAllowance, RentalAllowances}
import play.api.libs.json.{Json, OFormat}


case class RentalAllowancesStoreAnswers(annualInvestmentAllowance: Option[BigDecimal],
                             electricChargePointAllowance: ElectricChargePointAllowance,
                             zeroEmissionCarAllowance: Option[BigDecimal],
                             zeroEmissionGoodsVehicleAllowance: Option[BigDecimal],
                             businessPremisesRenovationAllowance: Option[BigDecimal],
                             replacementOfDomesticGoodsAllowance: Option[BigDecimal],
                             otherCapitalAllowance: Option[BigDecimal])

object RentalAllowancesStoreAnswers {
  implicit val formats: OFormat[RentalAllowancesStoreAnswers] = Json.format[RentalAllowancesStoreAnswers]

  def fromJourneyAnswers(answers: RentalAllowances): RentalAllowancesStoreAnswers =
    RentalAllowancesStoreAnswers(
      annualInvestmentAllowance = answers.annualInvestmentAllowance,
      electricChargePointAllowance = answers.electricChargePointAllowance,
      zeroEmissionCarAllowance = answers.zeroEmissionCarAllowance,
      zeroEmissionGoodsVehicleAllowance = answers.zeroEmissionGoodsVehicleAllowance,
      businessPremisesRenovationAllowance = answers.businessPremisesRenovationAllowance,
      replacementOfDomesticGoodsAllowance = answers.replacementOfDomesticGoodsAllowance,
      otherCapitalAllowance = answers.otherCapitalAllowance
    )
}
