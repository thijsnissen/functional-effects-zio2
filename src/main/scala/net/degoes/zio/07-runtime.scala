package net.degoes.zio

import zio.*
import scala.concurrent.ExecutionContext
import java.io.IOException

object CustomRuntime:
  val defaultEnvironment: ZEnvironment[Any] = ZEnvironment.empty
  val defaultRuntimeFlags: RuntimeFlags     = RuntimeFlags.default
  val defaultFiberRefs: FiberRefs           = FiberRefs.empty

  final case class AppConfig(name: String)

  /**
   * EXERCISE
   *
   * Create a custom runtime that bundles a value of type `AppConfig` into the
   * environment.
   */
  lazy val customRuntime: Runtime[Any with AppConfig] =
    Runtime(defaultEnvironment.add(AppConfig("My App")), defaultFiberRefs, defaultRuntimeFlags)

  val program: ZIO[AppConfig, IOException, Unit] =
    for
      appConfig <- ZIO.service[AppConfig]
      _ <- Console.printLine(s"Application name is ${appConfig.name}")
      _ <- Console.printLine("What is your name?")
      name <- Console.readLine
      _ <- Console.printLine(s"Hello, $name!")
    yield ()

  /**
   * EXERCISE
   *
   * Using the `run` method of the custom runtime you created,
   * execute the `program` effect above.
   *
   * NOTE: You will have to use `Unsafe.unsafe { implicit u => ... }`
   * or `Unsafe.unsafe { ... }` (Scala 3) in order to call `run`.
   */
  def main(args: Array[String]): Unit =
    Unsafe.unsafe:
      implicit u => customRuntime.unsafe.run(program)

object ThreadPool extends ZIOAppDefault:

  lazy val dbPool: Executor = Executor.fromExecutionContext(ExecutionContext.global)

  /**
   * EXERCISE
   *
   * Using `ZIO#onExecutor`, write an `onDatabase` combinator that runs the
   * specified effect on the database thread pool.
   */
  def onDatabase[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.onExecutor(dbPool)

  /**
   * EXERCISE
   *
   * Implement a combinator to print out thread information before and after
   * executing the specified effect.
   */
  def threadLogged[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    val log = ZIO.succeed:
      val thread = Thread.currentThread()

      val id        = thread.threadId
      val name      = thread.getName
      val groupName = thread.getThreadGroup.getName

      println(s"Thread($id, $name, $groupName)")

    log *> zio

  /**
   * EXERCISE
   *
   * Use the `threadLogged` combinator around different effects below to
   * determine which threads are executing which effects.
   */
  val run: ZIO[Any, IOException, Unit] =
    threadLogged(Console.printLine("Main")) *>
    threadLogged(onDatabase(Console.printLine("Database") *> ZIO.blocking(Console.printLine("Blocking")) *> Console.printLine("Database"))) *>
    threadLogged(Console.printLine("Main"))

object CustomLogger extends ZIOAppDefault:

  /**
   * EXERCISE
   *
   * Using `ZLogger.simple`, create a logger that dumps text strings to the console
   * using `println`.
   */
  lazy val simpleLogger: ZLogger[String, Unit] =
    ZLogger.simple((s: String) => println(s"MyLogger says: $s"))

  /**
   * EXERCISE
   *
   * Create a layer that will install your simple logger using Runtime.addLogger.
   */
  lazy val withCustomLogger: ZLayer[Any, Nothing, Unit] =
    Runtime.addLogger(simpleLogger)

  /**
   * EXERCISE
   *
   * Using `ZIO#provide`, inject the custom logger into the following effect
   * and verify your logger is being used.
   */
  val run: ZIO[Any, Nothing, Unit] =
    ZIO
      .log("Hello World!")
      .provide(withCustomLogger)
