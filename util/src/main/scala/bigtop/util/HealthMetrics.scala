package bigtop
package util

import scala.collection.JavaConversions._
import com.yammer.metrics.scala.Instrumented
import akka.dispatch.{ Future, Promise }
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.service._
import java.util.concurrent.TimeUnit

trait HealthMetrics extends Instrumented {
  lazy val responseTimeUnit = TimeUnit.MILLISECONDS
  lazy val responseCountTimeUnit = TimeUnit.SECONDS

  lazy val successes    = metrics.meter("successes",     "successes",     null, responseCountTimeUnit)
  lazy val warnings     = metrics.meter("warnings",      "warnings",      null, responseCountTimeUnit)
  lazy val clientErrors = metrics.meter("client-errors", "client-errors", null, responseCountTimeUnit)
  lazy val serverErrors = metrics.meter("server-errors", "server-errors", null, responseCountTimeUnit)

  // We want the Java timer with the stop() method here, not the Scala wrapper with a time(foo) method:
  lazy val responseTimes = metrics.metricsRegistry.newTimer(
    getClass,
    "response-times",
    responseTimeUnit,
    responseCountTimeUnit
  )

  def healthMetrics[T,S](monitored: => ServiceDescriptorFactory[T,S]): ServiceDescriptorFactory[T,S] = {
    (context: ServiceContext) => {
      val underlying = monitored(context)

      ServiceDescriptor[T,S](
        startup = underlying.startup,

        request = (config: S) => {
          val inner = underlying.request(config)

          new AsyncCustomHttpService[T] {
            def metadata = None
            def service = {
              (req: HttpRequest[T]) =>
                inner.service(req) map {
                  futureResult =>
                    val responseTimer = responseTimes.time()
                  futureResult.map {
                    result: HttpResponse[T] =>
                      result.status.code match {
                        case _ : HttpSuccess => successes.mark()
                        case _ : HttpWarning => warnings.mark()
                        case _ : ClientError => clientErrors.mark()
                        case _ : ServerError => serverErrors.mark()
                      }
                    responseTimer.stop()
                    result
                  }
                }
            }
          }
        },

        shutdown = (config: S) => {
          for(name <- metrics.metricsRegistry.allMetrics.keySet) {
            metrics.metricsRegistry.removeMetric(name)
          }
          metrics.metricsRegistry.shutdown()

          underlying.shutdown(config)
        }
      )
    }
  }
}
