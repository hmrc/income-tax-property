package models.responses

import models.Enumerable
import models.request.foreign.WithName
import play.api.libs.json.{Format, Json}

import java.time.LocalDate

case class HipBusinessDataModel (
  incomeSourceId: String,
  incomeSource: Option[String],
  accPeriodSDate: LocalDate,
  accPeriodEDate: LocalDate,
  tradingName: Option[String],
  businessAddressDetails: Option[BusinessAddressDetails],
  businessContactDetails: Option[BusinessContactDetails],
  tradingSDate: Option[LocalDate],
  contextualTaxYear: Option[String], // Format YYYY
  cashOrAccrualsFlag: Boolean,
  seasonalFlag: Option[Boolean],
  cessationDate: Option[LocalDate],
  paperLessFlag: Option[Boolean],
  incomeSourceStartDate: Option[LocalDate],
  firstAccountingPeriodStartDate: Option[LocalDate],
  firstAccountingPeriodEndDate: Option[LocalDate],
  latencyDetails: Option[LatencyDetails],
  quarterTypeElection: Option[QuarterTypeElection]
)

object HipBusinessDataModel {
  implicit val format: Format[HipBusinessDataModel] = Json.format[HipBusinessDataModel]

  def toBusinessDetailsModel(hipBusinessDataModel: HipBusinessDataModel): BusinessDetailsModel = {
    BusinessDetailsModel(
      incomeSourceId = hipBusinessDataModel.incomeSourceId,
      accountingPeriodStartDate = hipBusinessDataModel.accPeriodSDate,
      accountingPeriodEndDate = hipBusinessDataModel.accPeriodEDate,
      tradingName = hipBusinessDataModel.tradingName,
      businessAddressDetails = hipBusinessDataModel.businessAddressDetails,
      businessContactDetails = hipBusinessDataModel.businessContactDetails,
      tradingStartDate = hipBusinessDataModel.tradingSDate,
      latencyDetails = hipBusinessDataModel.latencyDetails,
      cashOrAccruals = Some(hipBusinessDataModel.cashOrAccrualsFlag),
      seasonal = hipBusinessDataModel.seasonalFlag,
      cessationDate = hipBusinessDataModel.cessationDate,
      paperless = hipBusinessDataModel.paperLessFlag,
      firstAccountingPeriodStartDate = hipBusinessDataModel.firstAccountingPeriodStartDate,
      firstAccountingPeriodEndDate = hipBusinessDataModel.firstAccountingPeriodEndDate
    )
  }
}


case class QuarterTypeElection(
                                quarterReportingType: QuarterReportingType,
                                taxYearofElection: String //Format: YYYY
                              )
object QuarterTypeElection{
  implicit val format: Format[QuarterTypeElection] = Json.format[QuarterTypeElection]
}


sealed trait QuarterReportingType
object QuarterReportingType extends Enumerable.Implicits {
  case object STANDARD extends WithName("STANDARD") with QuarterReportingType
  case object CALENDAR extends WithName("CALENDAR") with QuarterReportingType

  val values: Seq[QuarterReportingType] = Seq(
    STANDARD,
    CALENDAR
  )

  implicit val enumerable: Enumerable[QuarterReportingType] =
    Enumerable(values.map(v => v.toString -> v): _*)
}