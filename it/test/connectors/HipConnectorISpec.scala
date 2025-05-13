

package connectors

import models.common.{IncomeSourceId, Nino}
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.{HeaderCarrier, SessionId}
import scala.concurrent.ExecutionContext.Implicits.global

class HipConnectorISpec extends ConnectorIntegrationSpec with MockFactory {

  private val nino = Nino("some-nino")
  private val incomeSourceId = IncomeSourceId("some-income-source-id")
  private val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("sessionIdValue")))

  private val underTest = new HipConnector(httpClientV2, appConfigStub)

  ".createPropertyBroughtForwardLoss" should {
    ???
  }
}
