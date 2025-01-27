package connectors.response

import connectors.Parser
import models.errors.ApiError
import play.api.http.Status._
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.http.{HttpReads, HttpResponse}

import java.time.LocalDate

case class GetBroughtForwardLossResponse(httpResponse: HttpResponse, result: Either[ApiError, BroughtForwardLossResponse])

case class BroughtForwardLossResponse(
  incomeSourceId: String,
  incomeSourceType: String,
  broughtForwardLossAmount: BigDecimal,
  taxYear: Int,
  lossId: String,
  submissionDate: LocalDate
)

object BroughtForwardLossResponse {
  implicit val format: Format[BroughtForwardLossResponse] = Json.format[BroughtForwardLossResponse]
}

object GetBroughtForwardLossResponse {
  implicit val getBroughtForwardLossResponse: HttpReads[GetBroughtForwardLossResponse] = new HttpReads[GetBroughtForwardLossResponse] with Parser {

    override protected[connectors] val parserName: String = this.getClass.getSimpleName


    override def read(method: String, url: String, response: HttpResponse): GetBroughtForwardLossResponse = response.status match {
      case OK =>
        val result = response.json.validate[BroughtForwardLossResponse].fold[Either[ApiError, BroughtForwardLossResponse]](
          _ => badSuccessJsonResponse, parsedModel => Right(parsedModel)
        )
        GetBroughtForwardLossResponse(response, result)

      case NOT_FOUND => GetBroughtForwardLossResponse(response, handleError(response, NOT_FOUND))
      case BAD_REQUEST => GetBroughtForwardLossResponse(response, handleError(response, response.status))
      case _ => GetBroughtForwardLossResponse(response, handleError(response, INTERNAL_SERVER_ERROR))
    }
  }
}