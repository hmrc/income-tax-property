package models.responses

import java.time.LocalDateTime

case class HipBusinessDetailsResponse (
                                        processingDate: LocalDateTime,
                                        taxPayerDisplayResponse: TaxPayerDisplayResponse
                                      )


case class TaxPayerDisplayResponse (
                                     safeId: String,
                                     nino: String,
                                     mtdId: String,
                                     yearOfMigration: Option[String], // Format YYYY
                                     propertyIncomeFlag: Boolean,
                                     businessData: Option[HipBusinessDataModel],
                                     propertyData: Option[HipPropertyDataModel]
                                   )