package connectors.response

import connectors.Parser
import models.errors.ApiError
import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class UpdateBroughtForwardLossResponse(httpResponse: HttpResponse, result: Either[ApiError, Unit])


object UpdateBroughtForwardLossResponse {

  implicit val updateBroughtForwardLossResponse: HttpReads[UpdateBroughtForwardLossResponse] = new HttpReads[UpdateBroughtForwardLossResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): UpdateBroughtForwardLossResponse = response.status match {
      case OK =>
        UpdateBroughtForwardLossResponse(httpResponse = response, result = Right(None))
      case NOT_FOUND => UpdateBroughtForwardLossResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST => UpdateBroughtForwardLossResponse(response, handleError(response, response.status))
      case _ => UpdateBroughtForwardLossResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }
  }
}
