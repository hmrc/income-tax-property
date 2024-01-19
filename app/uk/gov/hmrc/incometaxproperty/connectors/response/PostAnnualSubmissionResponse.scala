package uk.gov.hmrc.incometaxproperty.connectors.response

import play.api.Logging
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR, NOT_FOUND, NO_CONTENT, SERVICE_UNAVAILABLE, UNPROCESSABLE_ENTITY}
import uk.gov.hmrc.http.{HttpReads, HttpResponse}
import uk.gov.hmrc.incometaxproperty.connectors.Parser
import uk.gov.hmrc.incometaxproperty.models.errors.ApiError

case class PostAnnualSubmissionResponse(httpResponse: HttpResponse, result: Either[ApiError, Option[String]])

object PostAnnualSubmissionResponse extends Logging {

  implicit val postPeriodicSubmission: HttpReads[PostAnnualSubmissionResponse] = new HttpReads[PostAnnualSubmissionResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName

    override def read(method: String, url: String, response: HttpResponse): PostAnnualSubmissionResponse = response.status match {
      case NO_CONTENT => PostAnnualSubmissionResponse(response, Right(None))
      case NOT_FOUND => PostAnnualSubmissionResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST | UNPROCESSABLE_ENTITY | INTERNAL_SERVER_ERROR | SERVICE_UNAVAILABLE =>
        PostAnnualSubmissionResponse(response, handleError(response, response.status))
      case _ => PostAnnualSubmissionResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }
  }
}
