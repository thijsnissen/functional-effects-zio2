package net.degoes.zio

import zio.*
import zio.metrics.Metric

object SimpleLogging extends ZIOAppDefault:

  /**
   * EXERCISE
   *
   * Add logging using `ZIO.log` around each update of the ref.
   */
  val program: ZIO[Any, Nothing, Int] =
    for
      ref   <- Ref.make(0)
      _     <- ZIO.foreachParDiscard(0 to 10)(i => ZIO.log(s"Logged $i") *> ref.update(_ + i))
      value <- ref.get
    yield value

  /**
   * EXERCISE
   *
   * Surround `program` in `LogLevel.Error` to change its log level.
   */
  val program2: ZIO[Any, Nothing, Int] = ZIO.logLevel(LogLevel.Error)(program)

  val run: ZIO[Any, Nothing, Int] = program *> program2

object LogSpan extends ZIOAppDefault:

  /**
   * EXERCISE
   *
   * Add a log span of "createUser" to the whole function.
   */
  def createUser(userName: String, passHash: String, salt: Long): ZIO[Any, Nothing, Unit] =
    ZIO.logSpan("createUser"):
      for
        _ <- ZIO.log(s"Creating user $userName")
      yield ()

  val run: ZIO[Any, Nothing, Unit] =
    createUser("sherlockholmes", "jkdf67sf6", 21381L)

object CounterExample extends ZIOAppDefault:
  final case class Request(body: String)
  final case class Response(body: String)

  /**
   * EXERCISE
   *
   * Use the constructors in `Metric` to make a counter metric that accepts
   * integers as input.
   */
  lazy val requestCounter: Metric.Counter[Int] =
    Metric.counterInt("requestCounter")

  /**
   * EXERCISE
   *
   * Use methods on the counter to increment the counter on every request.
   */
  def processRequest(request: Request): Task[Response] =
    requestCounter.increment *> ZIO.succeed(Response("OK"))

  /**
   * EXERCISE
   *
   * Use methods on the counter to print out its value.
   *
   * NOTE: In real applications you don't need to poll metrics because they
   * will be exported to monitoring systems.
   *
   */
  lazy val printCounter: ZIO[Any, Nothing, Unit] =
    requestCounter.value.flatMap:
      (c: zio.metrics.MetricState.Counter) =>
        ZIO.logAnnotate("exercise", "PrintCounter"):
          ZIO.log(s"Counted: ${c.count}")

  lazy val run: ZIO[Any, Throwable, Response | Long] =
    val processor = processRequest(Request("input")).repeatN(99)
    val printer   = printCounter.schedule(Schedule.fixed(100.millis))

    processor.race(printer)
