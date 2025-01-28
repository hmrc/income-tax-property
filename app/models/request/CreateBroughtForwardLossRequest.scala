package models.request

import models.common.IncomeSourceId
import play.api.libs.json.{Format, JsValue, Json, Writes}
import play.api.libs.ws.BodyWritable

case class CreateBroughtForwardLossRequest(
  taxYearBroughtForwardFrom: String,
  typeOfLoss: LossType,
  businessId: IncomeSourceId,
  lossAmount: BigDecimal
)

object CreateBroughtForwardLossRequest {
  implicit val format: Format[CreateBroughtForwardLossRequest] = Json.format[CreateBroughtForwardLossRequest]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)
}
