package models.request

import models.Enumerable
import models.request.foreign.WithName

sealed trait LossType

object LossType extends Enumerable.Implicits {
  case object UKProperty            extends WithName("uk-property") with LossType
  case object UKPropertyFHL         extends WithName("uk-property-fhl") with LossType
  case object ForeignProperty       extends WithName("foreign-property") with LossType
  case object ForeignPropertyFHLEEA extends WithName("foreign-property-fhl-eea") with LossType
  case object SelfEmployment        extends WithName("self-employment") with LossType
  case object SelfEmploymentClass4  extends WithName("self-employment-class4") with LossType

  val values: Seq[LossType] = Seq(
    UKProperty,
    UKPropertyFHL,
    ForeignProperty,
    ForeignPropertyFHLEEA,
    SelfEmployment,
    SelfEmploymentClass4
  )

  implicit val enumerable: Enumerable[LossType] = Enumerable(values.map(v => v.toString -> v): _*)
}
