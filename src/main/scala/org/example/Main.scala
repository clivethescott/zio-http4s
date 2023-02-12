package org.example

import com.comcast.ip4s._
import io.circe.Encoder
import io.circe.generic.semiauto._
import io.circe.literal._
import org.http4s.EntityEncoder
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server._
import zio._
import zio.clock.Clock
import zio.interop.catz._

import java.time.ZoneId
import java.time.ZonedDateTime

case class Message(greeting: String, generatedAt: ZonedDateTime)
object Message {
  implicit val enc: Encoder[Message] = deriveEncoder
  implicit val entityEnc: EntityEncoder[Task, Message] = jsonEncoderOf
}

object Greeter {
  trait Service {
    def greet(name: String): Task[Message]
  }
  private class Impl(val clock: Clock.Service) extends Service {
    override def greet(name: String): Task[Message] = for {
      localTime <- clock.localDateTime.orDie
      zdt = localTime.atZone(ZoneId.systemDefault())
    } yield Message(greeting = s"Hello there, $name", generatedAt = zdt)
  }

  val live: ZLayer[Clock, Nothing, Greeter] =
    ZLayer.fromService[Clock.Service, Greeter.Service] { clock =>
      new Impl(clock)
    }

  def greet(name: String): ZIO[Greeter, Throwable, Message] =
    ZIO.accessM[Greeter](_.get.greet(name))
}

final class Api(greeterService: Greeter.Service) {
  private val dsl = Http4sDsl[Task]
  import dsl._

  val greetRoutes = HttpRoutes.of[Task] { case GET -> Root / "hello" / name =>
    greeterService.greet(name).foldM(_ => InternalServerError(), Ok(_))
  }
}

object Main extends zio.App {
  override def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val program = for {
      greeterService <- ZIO.service[Greeter.Service]
      httpApp = new Api(greeterService).greetRoutes.orNotFound
      server <- ZIO
        .runtime[ZEnv]
        .flatMap { implicit runtime =>
          EmberServerBuilder
            .default[Task]
            .withHost(ipv4"0.0.0.0")
            .withPort(port"8080")
            .withHttpApp(httpApp)
            .build
            .toManagedZIO
            .useForever
            .exitCode
        }
    } yield server

    program
      .provideCustomLayer(Greeter.live)
  }

}
