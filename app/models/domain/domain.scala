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

package models

import cats.data.EitherT
import models.errors.{InvalidJsonFormatError, ServiceError}
import play.api.libs.json.{JsObject, Reads}

import scala.concurrent.Future
import scala.reflect.ClassTag

package object domain {
  type ApiResultT[A] = EitherT[Future, ServiceError, A]

  def jsonAs[A: Reads](jsObj: JsObject)(implicit ct: ClassTag[A]): Either[InvalidJsonFormatError, A] =
    jsObj
      .validate[A]
      .asEither
      .fold(
        err => Left(InvalidJsonFormatError(ct.runtimeClass.getName, jsObj.toString(), err.toList)),
        answers => Right(answers)
      )
}

