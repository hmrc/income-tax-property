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

package models.request.foreign

import models.errors.ServiceError
import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.responses.ForeignPropertyAllowances
import monocle.Optional
import monocle.macros.GenLens
import play.api.libs.json._
import play.api.libs.ws.BodyWritable

import java.time.LocalDateTime

case class ForeignPropertyAdjustments(privateUseAdjustment: Option[BigDecimal], balancingCharge: Option[BigDecimal])

object ForeignPropertyAdjustments {
  implicit val format: OFormat[ForeignPropertyAdjustments] = Json.format[ForeignPropertyAdjustments]
}

case class AnnualForeignProperty(
  countryCode: String,
  adjustments: Option[ForeignPropertyAdjustments],
  allowances: Option[ForeignPropertyAllowances]
)

object AnnualForeignProperty {
  implicit val format: OFormat[AnnualForeignProperty] = Json.format[AnnualForeignProperty]
}

case class AnnualForeignPropertySubmission(
  submittedOn: Option[LocalDateTime],
  foreignProperty: Option[Seq[AnnualForeignProperty]]
)

object AnnualForeignPropertySubmission {
  implicit val format: OFormat[AnnualForeignPropertySubmission] = Json.format[AnnualForeignPropertySubmission]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)

  def fromForeignPropertyAllowances(
    mayBeAnnualForeignPropertySubmissionFromDownstream: Option[AnnualForeignPropertySubmission],
    foreignPropertyAllowancesWithCountryCode: ForeignPropertyAllowancesWithCountryCode
  ): Either[ServiceError, AnnualForeignPropertySubmission] = {

    val countryCode = foreignPropertyAllowancesWithCountryCode.countryCode
    val foreignPropertyLens = GenLens[AnnualForeignPropertySubmission](_.foreignProperty)
    val foreignPropertyAllowancesLens = GenLens[AnnualForeignProperty](_.allowances)

    val firstForeignPropertyAllowancesLens: Optional[AnnualForeignPropertySubmission, ForeignPropertyAllowances] =
      foreignPropertyLens.some.index(0).andThen(foreignPropertyAllowancesLens.some)

    val (mayBeAnnualForeignPropertySubmission, maybeForeignPropertyAdjustments, maybeForeignPropertyAllowances): (
      Option[AnnualForeignPropertySubmission],
      Option[ForeignPropertyAdjustments],
      Option[ForeignPropertyAllowances]
    ) =
      mayBeAnnualForeignPropertySubmissionFromDownstream match {
        case Some(
              fpas @ AnnualForeignPropertySubmission(
                _,
                Some(Seq(AnnualForeignProperty(_, Some(adjustments), Some(allowances))))
              )
            ) =>
          (Some(fpas), Some(adjustments), Some(allowances))
        case Some(pps) => (Some(pps), None, None)
        case _         => (None, None, None)
      }

    val newForeignPropertyAllowances = ForeignPropertyAllowances(
      zeroEmissionsCarAllowance = foreignPropertyAllowancesWithCountryCode.zeroEmissionsCarAllowance,
      zeroEmissionsGoodsVehicleAllowance = foreignPropertyAllowancesWithCountryCode.zeroEmissionsGoodsVehicleAllowance,
      costOfReplacingDomesticItems = foreignPropertyAllowancesWithCountryCode.costOfReplacingDomesticItems,
      otherCapitalAllowance = foreignPropertyAllowancesWithCountryCode.annualInvestmentAllowance,
      annualInvestmentAllowance = maybeForeignPropertyAllowances.flatMap(_.annualInvestmentAllowance),
      propertyAllowance = maybeForeignPropertyAllowances.flatMap(_.propertyAllowance),
      electricChargePointAllowance = maybeForeignPropertyAllowances.flatMap(_.electricChargePointAllowance),
      structuredBuildingAllowance = maybeForeignPropertyAllowances.flatMap(_.structuredBuildingAllowance)
    )

    val annualForeignPropertySubmissionRetainingAdjustments = AnnualForeignPropertySubmission(
      submittedOn = mayBeAnnualForeignPropertySubmission.map(_.submittedOn).getOrElse(Some(LocalDateTime.now())),
      foreignProperty = Some(
        Seq(
          AnnualForeignProperty(
            countryCode = countryCode,
            adjustments = maybeForeignPropertyAdjustments,
            allowances = Some(ForeignPropertyAllowances(None, None, None, None, None, None, None, None))
          )
        )
      )
    )

    val annualForeignPropertySubmissionWithNewAllowances =
      firstForeignPropertyAllowancesLens.replace(newForeignPropertyAllowances)(
        annualForeignPropertySubmissionRetainingAdjustments
      )

    Right(annualForeignPropertySubmissionWithNewAllowances)
  }

}