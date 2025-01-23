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
import models.request.foreign.adjustments.ForeignPropertyAdjustmentsWithCountryCode
import models.request.foreign.allowances.ForeignPropertyAllowancesWithCountryCode
import models.responses.{StructuredBuildingAllowance, StructuredBuildingAllowanceBuilding, StructuredBuildingAllowanceDate}
import models.request.foreign.sba.ForeignPropertySbaWithCountryCode
import monocle.Optional
import monocle.macros.GenLens
import play.api.libs.json._
import play.api.libs.ws.BodyWritable

case class ForeignPropertyAdjustments(privateUseAdjustment: Option[BigDecimal], balancingCharge: Option[BigDecimal])

object ForeignPropertyAdjustments {
  implicit val format: OFormat[ForeignPropertyAdjustments] = Json.format[ForeignPropertyAdjustments]
}

case class ForeignPropertyAllowances(
  zeroEmissionsCarAllowance: Option[BigDecimal],
  zeroEmissionsGoodsVehicleAllowance: Option[BigDecimal],
  costOfReplacingDomesticItems: Option[BigDecimal],
  otherCapitalAllowance: Option[BigDecimal],
  annualInvestmentAllowance: Option[BigDecimal],
  propertyAllowance: Option[BigDecimal],
  electricChargePointAllowance: Option[BigDecimal],
  structuredBuildingAllowance: Option[Seq[StructuredBuildingAllowance]]
)

object ForeignPropertyAllowances {
  implicit val format: OFormat[ForeignPropertyAllowances] =
    Json.format[ForeignPropertyAllowances]
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

    val (maybeForeignPropertyAdjustments, maybeForeignPropertyAllowances): (
      Option[ForeignPropertyAdjustments],
      Option[ForeignPropertyAllowances]
    ) =
      mayBeAnnualForeignPropertySubmissionFromDownstream match {
        case Some(
              AnnualForeignPropertySubmission(Some(Seq(AnnualForeignProperty(_, Some(adjustments), Some(allowances)))))
            ) =>
          (Some(adjustments), Some(allowances))
        case Some(_) => (None, None)
        case _       => (None, None)
      }

    val newForeignPropertyAllowances = ForeignPropertyAllowances(
      zeroEmissionsCarAllowance = foreignPropertyAllowancesWithCountryCode.zeroEmissionsCarAllowance,
      zeroEmissionsGoodsVehicleAllowance = foreignPropertyAllowancesWithCountryCode.zeroEmissionsGoodsVehicleAllowance,
      costOfReplacingDomesticItems = foreignPropertyAllowancesWithCountryCode.costOfReplacingDomesticItems,
      otherCapitalAllowance = foreignPropertyAllowancesWithCountryCode.otherCapitalAllowance,
      annualInvestmentAllowance = maybeForeignPropertyAllowances.flatMap(_.annualInvestmentAllowance),
      propertyAllowance = maybeForeignPropertyAllowances.flatMap(_.propertyAllowance),
      electricChargePointAllowance = maybeForeignPropertyAllowances.flatMap(_.electricChargePointAllowance),
      structuredBuildingAllowance = maybeForeignPropertyAllowances.flatMap(_.structuredBuildingAllowance)
    )

    val annualForeignPropertySubmissionRetainingAdjustments = AnnualForeignPropertySubmission(
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

  def fromForeignPropertySbas(
     mayBeAnnualForeignPropertySubmissionFromDownstream: Option[AnnualForeignPropertySubmission],
    foreignPropertySbaWithCountryCode: ForeignPropertySbaWithCountryCode
  ): Either[ServiceError, AnnualForeignPropertySubmission] = {
    val targetCountryCode = foreignPropertySbaWithCountryCode.countryCode
    val foreignPropertyLens = GenLens[AnnualForeignPropertySubmission](_.foreignProperty)
    val foreignPropertyAllowancesLens = GenLens[AnnualForeignProperty](_.allowances)
    val firstForeignPropertyAllowancesLens: Optional[AnnualForeignPropertySubmission, ForeignPropertyAllowances] =
      foreignPropertyLens.some.index(0).andThen(foreignPropertyAllowancesLens.some)

    val (maybeForeignPropertyAdjustments, maybeForeignPropertyAllowances): (
      Option[ForeignPropertyAdjustments], Option[ForeignPropertyAllowances]) =
      mayBeAnnualForeignPropertySubmissionFromDownstream match {
        case Some(annualForeignPropertySubmission) =>
          annualForeignPropertySubmission.foreignProperty.flatMap(_.find(_.countryCode == targetCountryCode)) match {
            case Some(foreignPropertyForTheCountryCode) =>
              (foreignPropertyForTheCountryCode.adjustments, foreignPropertyForTheCountryCode.allowances)
            case _ => (None, None)
          }
        case _ => (None, None)
      }

    val sbaSeq: Seq[StructuredBuildingAllowance] = foreignPropertySbaWithCountryCode.allowances.map { allowances =>
      allowances.toSeq.map { foreignSba =>
        StructuredBuildingAllowance(
          amount = foreignSba.foreignStructureBuildingAllowanceClaim,
          firstYear = Some(StructuredBuildingAllowanceDate(
            qualifyingDate = foreignSba.foreignStructureBuildingQualifyingDate,
            qualifyingAmountExpenditure = foreignSba.foreignStructureBuildingQualifyingAmount
          )),
          building = StructuredBuildingAllowanceBuilding(
            name = Some(foreignSba.foreignStructureBuildingAddress.name),
            number = Some(foreignSba.foreignStructureBuildingAddress.number),
            postCode = foreignSba.foreignStructureBuildingAddress.postCode
          ))
      }
    }.getOrElse(Seq.empty[StructuredBuildingAllowance])

    val newForeignPropertyAllowances = ForeignPropertyAllowances(
      zeroEmissionsCarAllowance = maybeForeignPropertyAllowances.flatMap(_.zeroEmissionsCarAllowance),
      zeroEmissionsGoodsVehicleAllowance = maybeForeignPropertyAllowances.flatMap(_.zeroEmissionsGoodsVehicleAllowance),
      costOfReplacingDomesticItems = maybeForeignPropertyAllowances.flatMap(_.costOfReplacingDomesticItems),
      otherCapitalAllowance = maybeForeignPropertyAllowances.flatMap(_.otherCapitalAllowance),
      annualInvestmentAllowance = maybeForeignPropertyAllowances.flatMap(_.annualInvestmentAllowance),
      propertyAllowance = None,
      electricChargePointAllowance = maybeForeignPropertyAllowances.flatMap(_.electricChargePointAllowance),
      structuredBuildingAllowance = Option.when(sbaSeq.nonEmpty)(sbaSeq)
    )

    val annualForeignPropertySubmissionRetainingAdjustments = AnnualForeignPropertySubmission(
      foreignProperty = Some(
        Seq(
          AnnualForeignProperty(
            countryCode = targetCountryCode,
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

  def fromForeignPropertyAdjustments(
    mayBeAnnualForeignPropertySubmissionFromDownstream: Option[AnnualForeignPropertySubmission],
    foreignPropertyAdjustmentsWithCountryCode: ForeignPropertyAdjustmentsWithCountryCode
  ): Either[ServiceError, AnnualForeignPropertySubmission] = {
    val targetCountryCode = foreignPropertyAdjustmentsWithCountryCode.countryCode
    val foreignPropertyLens = GenLens[AnnualForeignPropertySubmission](_.foreignProperty)
    val foreignPropertyAdjustmentsLens = GenLens[AnnualForeignProperty](_.adjustments)
    val firstForeignPropertyAdjustmentsLens: Optional[AnnualForeignPropertySubmission, ForeignPropertyAdjustments] =
      foreignPropertyLens.some.index(0).andThen(foreignPropertyAdjustmentsLens.some)
    val foreignPropertyAllowancesLens = GenLens[AnnualForeignProperty](_.allowances)
    val firstForeignPropertyAllowancesLens: Optional[AnnualForeignPropertySubmission, ForeignPropertyAllowances] =
      foreignPropertyLens.some.index(0).andThen(foreignPropertyAllowancesLens.some)

    val newForeignPropertyAdjustments = ForeignPropertyAdjustments(
      privateUseAdjustment = Some(foreignPropertyAdjustmentsWithCountryCode.privateUseAdjustment),
      balancingCharge = foreignPropertyAdjustmentsWithCountryCode.balancingCharge.balancingChargeAmount
    )

    val newForeignPropertyAllowances = ForeignPropertyAllowances(
      propertyAllowance = foreignPropertyAdjustmentsWithCountryCode.propertyIncomeAllowanceClaim,
      zeroEmissionsCarAllowance = None,
      zeroEmissionsGoodsVehicleAllowance = None,
      costOfReplacingDomesticItems = None,
      otherCapitalAllowance = None,
      annualInvestmentAllowance = None,
      electricChargePointAllowance = None,
      structuredBuildingAllowance = None
    )

    val annualForeignPropertySubmissionRetainingAllowances = AnnualForeignPropertySubmission(
      foreignProperty = Some(
        Seq(
          AnnualForeignProperty(
            countryCode = targetCountryCode,
            adjustments = Some(ForeignPropertyAdjustments(None, None)),
            allowances = Some(ForeignPropertyAllowances(None, None, None, None, None, None, None, None))
          )
        )
      )
    )

    val annualForeignPropertySubmissionWithNewAdjustments =
      firstForeignPropertyAdjustmentsLens.replace(newForeignPropertyAdjustments)(
        annualForeignPropertySubmissionRetainingAllowances
      )

    val annualForeignPropertySubmissionWithBoth =
      firstForeignPropertyAllowancesLens.replace(newForeignPropertyAllowances)(
        annualForeignPropertySubmissionWithNewAdjustments
      )

    Right(annualForeignPropertySubmissionWithBoth)
  }

}
