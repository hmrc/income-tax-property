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

package connectors

import com.github.tomakehurst.wiremock.client.MappingBuilder
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.http.HttpHeader
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import play.api.http.ContentTypes.JSON
import play.api.test.Helpers.CONTENT_TYPE
import uk.gov.hmrc.http.HttpResponse

trait WireMockStubs {

  def stubGetHttpClientCall(url: String,
                            httpResponse: HttpResponse,
                            requestHeaders: Seq[HttpHeader] = Seq.empty): StubMapping = {
    val mappingBuilder = get(urlMatching(url))
    stubMapping(httpResponse, requestHeaders, mappingBuilder)
  }

  def stubPostHttpClientCall(url: String,
                             httpRequestBodyJson: String,
                             httpResponse: HttpResponse,
                             requestHeaders: Seq[HttpHeader] = Seq.empty): StubMapping = {
    val mappingBuilder = post(urlMatching(url)).withRequestBody(equalToJson(httpRequestBodyJson))
    stubMapping(httpResponse, requestHeaders, mappingBuilder)
  }

  def stubPutHttpClientCall(url: String,
                            httpRequestBodyJson: String,
                            httpResponse: HttpResponse,
                            requestHeaders: Seq[HttpHeader] = Seq.empty): StubMapping = {
    val mappingBuilder = put(urlMatching(url)).withRequestBody(equalToJson(httpRequestBodyJson))
    stubMapping(httpResponse, requestHeaders, mappingBuilder)
  }

  def stubDeleteHttpClientCall(url: String,
                               httpResponse: HttpResponse,
                               requestHeaders: Seq[HttpHeader] = Seq.empty): StubMapping = {
    val mappingBuilder = delete(urlMatching(url))
    stubMapping(httpResponse, requestHeaders, mappingBuilder)
  }

  private def stubMapping(httpResponse: HttpResponse,
                          requestHeaders: Seq[HttpHeader],
                          mappingBuilder: MappingBuilder): StubMapping = {
    val responseBuilder = aResponse()
      .withStatus(httpResponse.status)
      .withBody(httpResponse.body)
      .withHeader(CONTENT_TYPE, JSON)

    val mappingBuilderWithHeaders: MappingBuilder = requestHeaders
      .foldLeft(mappingBuilder)((result, nxt) => result.withHeader(nxt.key(), equalTo(nxt.firstValue())))

    stubFor(mappingBuilderWithHeaders.willReturn(responseBuilder))
  }
}
