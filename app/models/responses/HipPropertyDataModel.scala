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
                                    paperLessFlag: Boolean,
                                    incomeSourceStartDate: LocalDate,
                                    firstAccountingPeriodStartDate: LocalDate,
                                    firstAccountingPeriodEndDate: LocalDate,
                                    latencyDetails: Option[LatencyDetails],
                                    quarterTypeElection: Option[QuarterTypeElection]
)

object HipPropertyDataModel {
  implicit val format: Format[HipPropertyDataModel] = Json.format[HipPropertyDataModel]
}