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

package uk.gov.hmrc.incometaxproperty

import stubs.WireMockStubs
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.test.{DefaultAwaitTimeout, FutureAwaits}
import providers.AppConfigStubProvider
import support.helpers.IncomeTaxWireMockServer
import uk.gov.hmrc.http.test.HttpClientSupport

trait ConnectorSpec extends AnyWordSpec with Matchers
  with FutureAwaits with DefaultAwaitTimeout
  with HttpClientSupport
  with AppConfigStubProvider
  with IncomeTaxWireMockServer
  with WireMockStubs
  with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    super.beforeAll()
    startWiremock()
  }

  override def afterAll(): Unit = {
    stopWiremock()
    super.afterAll()
  }
}