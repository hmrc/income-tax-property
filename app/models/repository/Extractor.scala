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

package models.repository

import models.request.esba.{EsbaInfo, EsbaInfoToSave}


trait Extractor[T, U] {
  def extractToSavePart(withAll: T): U

}

object Extractor {
  implicit object EsbaExtractor extends Extractor[EsbaInfo, EsbaInfoToSave] {
    override def extractToSavePart(withAll: EsbaInfo): EsbaInfoToSave = {
      EsbaInfoToSave(withAll.claimEnhancedStructureBuildingAllowance, withAll.esbaClaims)
    }
  }

  implicit class GeneralExtractor[T, U](value: T) {

    def extractToSavePart()(implicit extractor: Extractor[T, U]): U = {
      extractor.extractToSavePart(value)
    }
  }

}
