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

package models.domain

import models.common._
import models.errors.InvalidJsonFormatError
import play.api.libs.json._
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats

import java.time._
import scala.reflect.ClassTag

case class JourneyAnswers(mtditid: Mtditid,
                          businessId: BusinessId,
                          taxYear: TaxYear,
                          journey: JourneyName,
                          status: JourneyStatus,
                          data: JsObject,
                          expireAt: Instant,
                          createdAt: Instant,
                          updatedAt: Instant) {

  def validatedAs[A: Reads](implicit ct: ClassTag[A]): Either[InvalidJsonFormatError, A] = jsonAs[A](data)
}

object JourneyAnswers extends MongoJavatimeFormats.Implicits {
  private val reads: Reads[JourneyAnswers] = Json
    .reads[JourneyAnswers]
    .orElse(
      Reads[JourneyAnswers] { json =>
        Json.reads[JourneyAnswers].reads(json.as[JsObject] + ("data" -> JsObject.empty))
      }
    )
  private val writes: OWrites[JourneyAnswers] = Json.writes[JourneyAnswers]

  implicit val formats: OFormat[JourneyAnswers] = OFormat(reads, writes)
}
