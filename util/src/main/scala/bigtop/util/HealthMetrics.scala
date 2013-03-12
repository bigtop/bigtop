package bigtop
package util

import scala.collection.JavaConversions._
import com.yammer.metrics.Metrics
import com.yammer.metrics.core.{MetricName, MetricsRegistry, Timer}
import com.yammer.metrics.scala.Meter
import scala.concurrent.{ Future, Promise }
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.service._
import java.util.concurrent.TimeUnit
import org.streum.configrity.Configuration

trait HealthMetrics {
  lazy val responseTimeUnit = TimeUnit.MILLISECONDS
  lazy val responseCountTimeUnit = TimeUnit.SECONDS

  lazy val registry = Metrics.defaultRegistry()

  def prefix(config: Configuration): String =
    config[String]("metrics.prefix")

  def meter(prefix: String, name: String, eventType: String, unit: TimeUnit): Meter = {
    val metricName = new MetricName(prefix, eventType, name)
    new Meter(registry.newMeter(metricName, eventType, unit))
  }

  // We want the Java timer with the stop() method here, not the Scala wrapper with a time(foo) method:
  def timer(prefix: String, eventType: String, name: String): Timer = {
    val metricName = new MetricName(prefix, eventType, name)
    registry.newTimer(metricName, responseTimeUnit, responseCountTimeUnit)
  }


  def healthMetrics[T,S](monitored: => ServiceDescriptorFactory[T,S]): ServiceDescriptorFactory[T,S] = {
    (context: ServiceContext) => {
      val loggingPrefix = prefix(context.config)

      val responseTimes = timer(loggingPrefix, "response-times", "response-times")
      val successes     = meter(loggingPrefix, "successes",      "successes",     responseCountTimeUnit)
      val warnings      = meter(loggingPrefix, "warnings",       "warnings",      responseCountTimeUnit)
      val clientErrors  = meter(loggingPrefix, "client-errors",  "client-errors", responseCountTimeUnit)
      val serverErrors  = meter(loggingPrefix, "server-errors",  "server-errors", responseCountTimeUnit)


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
          for(name <- registry.allMetrics.keySet) {
            registry.removeMetric(name)
          }
          registry.shutdown()

          underlying.shutdown(config)
        }
      )
    }
  }
}
