package connectors.response

import connectors.Parser
import models.errors.ApiError
import models.responses.HipBusinessDetailsResponse
import play.api.http.Status._
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

case class GetHipBusinessDetailsResponse(httpResponse: HttpResponse, result: Either[ApiError, HipBusinessDetailsResponse])

object GetHipBusinessDetailsResponse {
  implicit val getHipBusinessDetailsResponse: HttpReads[GetHipBusinessDetailsResponse] = new HttpReads[GetHipBusinessDetailsResponse] with Parser {

    override protected val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): GetHipBusinessDetailsResponse = response.status match {
      case OK => GetHipBusinessDetailsResponse(response, extractResult(response))
      case NOT_FOUND => GetHipBusinessDetailsResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST | UNPROCESSABLE_ENTITY | INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE =>
        GetHipBusinessDetailsResponse(response, handleError(response, response.status))
      case _ => GetHipBusinessDetailsResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }

    private def extractResult(response: HttpResponse): Either[ApiError, HipBusinessDetailsResponse] = {
      val json = response.json
      json.validate[HipBusinessDetailsResponse]
        .fold[Either[ApiError, HipBusinessDetailsResponse]](_ => badSuccessJsonResponse, parsedModel => Right(parsedModel))
    }
  }
}