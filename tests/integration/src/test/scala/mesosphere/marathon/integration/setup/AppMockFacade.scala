package mesosphere.marathon
package integration.setup

import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.client.RequestBuilding
import akka.http.scaladsl.client.RequestBuilding.Get
import akka.http.scaladsl.model.HttpResponse
import akka.stream.Materializer
import com.mesosphere.utils.http.RestResult
import com.typesafe.scalalogging.StrictLogging
import mesosphere.marathon.util.Retry

import scala.concurrent.Future
import scala.concurrent.duration._

class AppMockFacade(https: Boolean = false)(implicit system: ActorSystem, mat: Materializer, waitTime: FiniteDuration = 30.seconds) extends StrictLogging {
  import com.mesosphere.utils.http.AkkaHttpResponse._
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val scheduler: Scheduler = system.scheduler

  def ping(host: String, port: Int): Future[RestResult[HttpResponse]] = custom("/ping")(host, port)

  val scheme: String = if (https) "https" else "http"

  def custom(uri: String, method: RequestBuilding.RequestBuilder = Get)(host: String, port: Int): Future[RestResult[HttpResponse]] = {
    val url = s"$scheme://$host:$port$uri"
    Retry(s"query: $url", Int.MaxValue, maxDuration = waitTime) {
      request(method(url))
    }
  }
}
