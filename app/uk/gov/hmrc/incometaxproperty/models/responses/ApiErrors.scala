
package uk.gov.hmrc.incometaxproperty.models.responses

import play.api.libs.json.{JsValue, Json, OFormat}

sealed trait ErrorBody

case class ErrorModel(status: Int, body: ErrorBody){
  def toJson: JsValue ={
    body match {
      case error: ErrorBodyModel => Json.toJson(error)
      case errors: ErrorsBodyModel => Json.toJson(errors)
    }
  }
}

/** Single Error **/
case class ErrorBodyModel(code: String, reason: String) extends ErrorBody

object ErrorBodyModel {
  implicit val formats: OFormat[ErrorBodyModel] = Json.format[ErrorBodyModel]
  def parsingError(apiNumber: String = ""): ErrorBodyModel = ErrorBodyModel("PARSING_ERROR", s"Error parsing response from API${apiNumber.trim}")
}

/** Multiple Errors **/
case class ErrorsBodyModel(failures: Seq[ErrorBodyModel]) extends ErrorBody

object ErrorsBodyModel {
  implicit val formats: OFormat[ErrorsBodyModel] = Json.format[ErrorsBodyModel]
}
