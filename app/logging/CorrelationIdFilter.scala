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

package logging

import com.google.inject.Inject
import org.apache.pekko.stream.Materializer
import org.slf4j.MDC
import play.api.mvc.{Filter, RequestHeader, Result}

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

class CorrelationIdFilter @Inject() (implicit val mat: Materializer, val ec: ExecutionContext) extends Filter {

  private val CorrelationIdHeaderKey = "CorrelationId"

  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val correlationId = requestHeader.headers.get(CorrelationIdHeaderKey).getOrElse(UUID.randomUUID().toString)
    MDC.put(CorrelationIdHeaderKey, correlationId)

    val eventualResult = if (requestHeader.headers.get(CorrelationIdHeaderKey).isEmpty) {
      val newRequest = requestHeader.withHeaders(requestHeader.headers.add(CorrelationIdHeaderKey -> correlationId))
      nextFilter(newRequest).map { result =>
        result.withHeaders(CorrelationIdHeaderKey -> correlationId)
      }
    } else {
      nextFilter(requestHeader)
    }
    eventualResult.andThen { case _ =>
      MDC.remove(CorrelationIdHeaderKey)
    }
  }
}
