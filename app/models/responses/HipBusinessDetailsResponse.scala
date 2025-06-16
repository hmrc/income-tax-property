
package models.responses

import models.responses.HipBusinessDataModel.toBusinessDetailsModel
import models.responses.HipBusinessDetailsResponse.TaxPayerDisplayResponse
import models.responses.HipBusinessDetailsResponse.TaxPayerDisplayResponse.toIncomeSourceTPDR
import models.responses.HipPropertyDataModel.toPropertyDetailsModel
import play.api.libs.json.{Format, Json}

import java.time.LocalDateTime

case class HipBusinessDetailsResponse (
                                        processingDate: LocalDateTime,
                                        taxPayerDisplayResponse: TaxPayerDisplayResponse
                                      )

object HipBusinessDetailsResponse {
  implicit val format: Format[HipBusinessDetailsResponse] = Json.format[HipBusinessDetailsResponse]

  case class TaxPayerDisplayResponse (
                                       safeId: String,
                                       nino: String,
                                       mtdId: String,
                                       yearOfMigration: Option[String], // Format YYYY
                                       propertyIncomeFlag: Boolean,
                                       businessData: Option[Seq[HipBusinessDataModel]],
                                       propertyData: Option[Seq[HipPropertyDataModel]]
                                     )

  object TaxPayerDisplayResponse {
    implicit val format: Format[TaxPayerDisplayResponse] = Json.format[TaxPayerDisplayResponse]

    def toIncomeSourceTPDR(taxPayerDisplayResponse: TaxPayerDisplayResponse): IncomeSourceDetailsModel.TaxPayerDisplayResponse = {
      IncomeSourceDetailsModel.TaxPayerDisplayResponse(
        safeId = taxPayerDisplayResponse.safeId,
        nino = taxPayerDisplayResponse.nino,
        mtdId = taxPayerDisplayResponse.mtdId,
        yearOfMigration = taxPayerDisplayResponse.yearOfMigration,
        propertyIncome = taxPayerDisplayResponse.propertyIncomeFlag,
        businessData = taxPayerDisplayResponse.businessData.map(_.map(toBusinessDetailsModel)),
        propertyData = taxPayerDisplayResponse.propertyData.map(_.map(toPropertyDetailsModel))
      )
    }
  }

  def toIncomeSourceDetailsModel(hipBusinessDetailsResponse: HipBusinessDetailsResponse): IncomeSourceDetailsModel = {
    IncomeSourceDetailsModel(
      processingDate = hipBusinessDetailsResponse.processingDate,
      taxPayerDisplayResponse = toIncomeSourceTPDR(hipBusinessDetailsResponse.taxPayerDisplayResponse)
    )
  }
}


