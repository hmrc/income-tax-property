/*
 * Copyright 2024 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
