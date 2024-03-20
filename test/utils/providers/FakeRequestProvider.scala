/*
 * Copyright 2023 HM Revenue & Customs
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

package utils.providers

import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest

trait FakeRequestProvider {

  protected val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest().withHeaders("MTDITID" -> "1234567890")
  protected val fakeGetRequest: FakeRequest[AnyContentAsEmpty.type] = fakeRequest.withMethod(newMethod = "GET")
  protected val fakePostRequest: FakeRequest[AnyContentAsEmpty.type] = fakeRequest.withMethod(newMethod = "POST")
  protected val fakePutRequest: FakeRequest[AnyContentAsEmpty.type] = fakeRequest.withMethod(newMethod = "PUT")
  protected val fakeDeleteRequest: FakeRequest[AnyContentAsEmpty.type] = fakeRequest.withMethod(newMethod = "DELETE")
}
