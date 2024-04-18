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

import cats.data.EitherT
import cats.syntax.either._
import models.errors.ServiceError

import scala.concurrent.Future

object ITPEnvelope {
  type ITPEnvelope[T] = EitherT[Future, ServiceError, T]

  def liftFuture[T](value: Future[Either[ServiceError, T]]): ITPEnvelope[T] =
    EitherT(value)

  def liftEither[T](value: Either[ServiceError, T]): ITPEnvelope[T] =
    EitherT(Future.successful(value))

  def liftPure[T](value: T): ITPEnvelope[T] = liftEither(value.asRight[ServiceError])
}
