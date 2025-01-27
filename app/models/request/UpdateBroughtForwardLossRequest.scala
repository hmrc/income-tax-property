package models.request

import play.api.libs.json.{Format, JsValue, Json, Writes}
import play.api.libs.ws.BodyWritable

case class UpdateBroughtForwardLossRequest(
  updatedBroughtForwardLossAmount: BigDecimal
)

object UpdateBroughtForwardLossRequest {
  implicit val format: Format[UpdateBroughtForwardLossRequest] = Json.format[UpdateBroughtForwardLossRequest]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)
}