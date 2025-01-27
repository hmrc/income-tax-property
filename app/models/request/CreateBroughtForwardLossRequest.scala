package models.request

import models.Enumerable
import models.common.{IncomeSourceId, TaxYear}
import models.request.foreign.WithName
import play.api.libs.json.{Format, JsValue, Json, Writes}
import play.api.libs.ws.BodyWritable

case class CreateBroughtForwardLossRequest(
  incomeSourceId: IncomeSourceId,
  incomeSourceType: IncomeSourceType,
  broughtForwardLossAmount: BigDecimal,
  taxYearBroughtForwardFrom: TaxYear
)

object CreateBroughtForwardLossRequest {
  implicit val format: Format[CreateBroughtForwardLossRequest] = Json.format[CreateBroughtForwardLossRequest]

  implicit def jsonBodyWritable[T](implicit
    writes: Writes[T],
    jsValueBodyWritable: BodyWritable[JsValue]
  ): BodyWritable[T] = jsValueBodyWritable.map(writes.writes)
}

sealed trait IncomeSourceType

object IncomeSourceType extends Enumerable.Implicits {
  case object ForeignProperty extends WithName("15") with IncomeSourceType
  case object UKProperty extends WithName("02") with IncomeSourceType
}