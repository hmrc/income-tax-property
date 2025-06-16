package models.responses

import models.request.foreign.WithName
import models.{Enumerable, IncomeSourceType}
import play.api.libs.json.{Format, Json}

import java.time.LocalDate

case class HipPropertyDataModel(
                                    incomeSourceType: Option[IncomeSourceType],
                                    incomeSourceId: String,
                                    accPeriodSDate: LocalDate,
                                    accPeriodEDate: LocalDate,
                                    tradingSDate: Option[LocalDate],
                                    contextualTaxYear: Option[String], // Format YYYY
                                    cashOrAccrualsFlag: Boolean,
                                    numPropRented: Option[String],
                                    numPropRentedUK: Option[String],
                                    numPropRentedEEA: Option[String],
                                    numPropRentedNONEEA: Option[String],
                                    email: Option[String],
                                    cessationDate: Option[LocalDate],
                                    paperLessFlag: Option[Boolean],
                                    incomeSourceStartDate: Option[LocalDate],
                                    firstAccountingPeriodStartDate: Option[LocalDate],
                                    firstAccountingPeriodEndDate: Option[LocalDate],
                                    latencyDetails: Option[LatencyDetails],
                                    quarterTypeElection: Option[QuarterTypeElection]
)

object HipPropertyDataModel {
  implicit val format: Format[HipPropertyDataModel] = Json.format[HipPropertyDataModel]

  def toPropertyDetailsModel(hipPropertyDataModel: HipPropertyDataModel): PropertyDetailsModel = {
    PropertyDetailsModel(
      incomeSourceType = hipPropertyDataModel.incomeSourceType.map(_.toString),
      incomeSourceId = hipPropertyDataModel.incomeSourceId,
      accountingPeriodStartDate = hipPropertyDataModel.accPeriodSDate,
      accountingPeriodEndDate = hipPropertyDataModel.accPeriodEDate,
      tradingStartDate = hipPropertyDataModel.tradingSDate,
      cashOrAccruals = Some(hipPropertyDataModel.cashOrAccrualsFlag),
      numPropRented = hipPropertyDataModel.numPropRented.map(_.toInt),
      numPropRentedUK = hipPropertyDataModel.numPropRentedUK.map(_.toInt),
      numPropRentedEEA = hipPropertyDataModel.numPropRentedEEA.map(_.toInt),
      numPropRentedNONEEA = hipPropertyDataModel.numPropRentedNONEEA.map(_.toInt),
      email = hipPropertyDataModel.email,
      cessationDate = hipPropertyDataModel.cessationDate,
      paperLess = hipPropertyDataModel.paperLessFlag,
      incomeSourceStartDate = hipPropertyDataModel.incomeSourceStartDate,
      firstAccountingPeriodStartDate = hipPropertyDataModel.firstAccountingPeriodStartDate,
      firstAccountingPeriodEndDate = hipPropertyDataModel.firstAccountingPeriodStartDate,
      latencyDetails = hipPropertyDataModel.latencyDetails
    )
  }
}