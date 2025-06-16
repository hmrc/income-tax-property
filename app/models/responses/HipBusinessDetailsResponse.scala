package models.responses

import play.api.libs.json.{Format, Json}

import java.time.LocalDateTime

case class HipBusinessDetailsResponse (
                                        processingDate: LocalDateTime,
                                        taxPayerDisplayResponse: TaxPayerDisplayResponse
                                      )

object HipBusinessDetailsResponse {
  implicit val format: Format[HipBusinessDetailsResponse] = Json.format[HipBusinessDetailsResponse]

}


case class TaxPayerDisplayResponse (
                                     safeId: String,
                                     nino: String,
                                     mtdId: String,
                                     yearOfMigration: Option[String], // Format YYYY
                                     propertyIncomeFlag: Boolean,
                                     businessData: Option[HipBusinessDataModel],
                                     propertyData: Option[HipPropertyDataModel]
                                   )

object TaxPayerDisplayResponse {
  implicit val format: Format[TaxPayerDisplayResponse] = Json.format[TaxPayerDisplayResponse]
}