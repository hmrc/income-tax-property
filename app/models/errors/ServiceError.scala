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

package models.errors

import play.api.libs.json.{JsPath, JsonValidationError}

trait ServiceError {
  val message: String
}

case class ApiServiceError(status: Int) extends ServiceError {
  override val message: String = s"API exception occurred. Exception: $status"
}

case object DataNotFoundError extends ServiceError {
  override val message: String = "User data could not be found"
}

case object RepositoryError extends ServiceError {
  override val message: String = "Received unsuccessful persistence response"
}

case class InvalidJsonFormatError(expectedCaseClassName: String,
                                  rawJson: String, error: List[(JsPath, scala.collection.Seq[JsonValidationError])]) extends ServiceError {
  val message: String = s"Cannot convert JSON to a case class: $expectedCaseClassName. Error: ${error.toString}. JSON:\n$rawJson"
}

final case class CannotReadJsonError(details: List[(JsPath, scala.collection.Seq[JsonValidationError])]) extends ServiceError {
  val message: String = s"Cannot read JSON: ${details.toString}"
}

final case class CannotParseJsonError(details: Throwable) extends ServiceError {
  val message: String = s"Cannot parse JSON: ${details.getMessage}"
}

final case class InternalError(description: String) extends ServiceError {
  override val message: String = description
}