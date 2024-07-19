package models

import models.responses.{UkOtherProperty, UkOtherPropertyExpenses, UkOtherPropertyIncome}
import utils.UnitTest

class UkOtherPropertySpec extends UnitTest {
  "convert income having all None values to None" in {

    val ukOtherProperty: UkOtherProperty = UkOtherProperty(
      Some(UkOtherPropertyIncome(None, None, None, None, None, None)),
      Some(UkOtherPropertyExpenses(Some(22), None, None, None, None, None, None, None, None, None, None))
    )
    UkOtherProperty.convertToNoneIfAllFieldsNone(ukOtherProperty) shouldBe Some(ukOtherProperty.copy(income = None))
  }

  "convert expense having all None values to None" in {

    val ukOtherProperty: UkOtherProperty = UkOtherProperty(
      Some(UkOtherPropertyIncome(Some(12.34), None, None, None, None, None)),
      Some(UkOtherPropertyExpenses(None, None, None, None, None, None, None, None, None, None, None))
    )
    UkOtherProperty.convertToNoneIfAllFieldsNone(ukOtherProperty) shouldBe Some(ukOtherProperty.copy(expenses = None))
  }
}
