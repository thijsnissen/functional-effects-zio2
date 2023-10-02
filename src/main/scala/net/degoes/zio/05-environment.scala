package net.degoes.zio

import zio.*

import java.io.IOException
import java.sql.ResultSet

/**
 * ZIO environment is a type-indexed map that allows you to store a number of
 * objects of different types. ZIO calls these objects "services", because
 * they contain bundles of functionality consumed your application.
 */
object TypeIndeedMap extends ZIOAppDefault:
  trait Logging
  object Logging extends Logging

  trait Database
  object Database extends Database

  trait Cache
  object Cache extends Cache

  val envLogging: ZEnvironment[Logging] = ZEnvironment(Logging: Logging)

  val envDatabase: ZEnvironment[Database] = ZEnvironment(Database: Database)

  val envCache: ZEnvironment[Cache] = ZEnvironment(Cache: Cache)

  /**
   * EXERCISE
   *
   * Using the `++` operator on `ZEnvironment`, combine the three maps
   * (`envLogging`, `envDatabase`, and `envCache`) into a single map that
   * has all three objects.
   */
  val allThree: ZEnvironment[Database with Cache with Logging] =
    envLogging ++ envDatabase ++ envCache

  /**
   * EXERCISE
   *
   * Using `ZEnvironment#get`, which can retrieve an object stored in
   * the map, retrieve the logging, database, and cache objects from
   * `allThree`. Note that you will have to specify the type parameter,
   * as it cannot be inferred (the map needs to know which of the objects
   * you want to retrieve, and that can be specified only by type).
   */
  lazy val logging: Logging   = allThree.get[Logging]
  lazy val database: Database = allThree.get[Database]
  lazy val cache: Cache       = allThree.get[Cache]

  val run: IO[IOException, Unit] =
    Console.printLine("Completed!")

object AccessEnvironment extends ZIOAppDefault:

  final case class Config(host: String, port: Int)

  /**
   * EXERCISE
   *
   * Using `ZIO.service`, access a `Config` service from the environment, and
   * extract the `host` field from it.
   */
  val accessHost: ZIO[Config, Nothing, String] =
    //ZIO.service[Config].map(_.host)
    ZIO.serviceWith[Config](_.host)

  /**
   * EXERCISE
   *
   * Using `ZIO.service`, access a `Config` service from the environment, and
   * extract the `port` field from it.
   */
  val accessPort: ZIO[Config, Nothing, Int] =
    //ZIO.service[Config].map(_.port)
    ZIO.serviceWith[Config](_.port)

  val run: IO[IOException, Unit] =
    val config = Config("localhost", 7878)

    (for
      host <- accessHost
      port <- accessPort
      _    <- Console.printLine(s"Configuration: $host:$port")
    yield ()).provideEnvironment(ZEnvironment(config))

object ProvideEnvironment extends ZIOAppDefault:

  final case class Config(server: String, port: Int)

  final case class DatabaseConnection():
    def query(query: String): Task[Int] = ZIO.attempt(42)

  val getServer: ZIO[Config, Nothing, String] =
    ZIO.serviceWith[Config](_.server)

  val useDatabaseConnection: ZIO[DatabaseConnection, Throwable, Int] =
    ZIO.serviceWithZIO[DatabaseConnection](_.query("SELECT * FROM USERS"))

  /**
   * EXERCISE
   *
   * Compose both the `getServer` and `useDatabaseConnection` effects together
   * and run them.
   * In order to do this successfully, you will have to use
   * `ZIO#provideEnvironment` to give them the environment that they need in
   * order to run.
   */
  val run: IO[Throwable, RuntimeFlags] =
    val config = Config("localhost", 7878)

    (getServer *> useDatabaseConnection)
      .provideEnvironment(ZEnvironment(config, DatabaseConnection()))

/**
 * In ZIO, layers are values that contain construction logic for services in
 * your  application. Services provide functionality like persistence or
 * logging or authentication, and they are used by business logic.
 *
 * A layer is a lot like a constructor, but may have complex initialization
 * or finalization, or may produce more than one service.
 *
 * ZIO has compile-time, type-safe wiring up of layers, which allows you to
 * optionally use ZIO for dependency-injection. The wire-up of layers
 * is done in a resource-safe, failure-aware way, with maximum parallelism
 * to decrease application startup time.
 *
 * Layers bring more power and compositionality to constructors. Although you
 * don't have to make your own layers to benefit from ZIO, layers can make
 * it easier and safer to assemble applications out of modules.
 */
object LayerEnvironment extends ZIOAppDefault:

  import java.io.IOException

  type LoggingWithFiles = Logging with Files

  trait Files:
    def read(file: String): IO[IOException, String]

  object Files:

    /**
     * EXERCISE
     *
     * Using `ZLayer.succeed`, create a layer that implements the `Files`
     * service.
     */
    val live: ZLayer[Any, Nothing, Files] =
      val files = new Files:
        def read(file: String): IO[IOException, String] =
          ZIO.attemptBlockingIO(scala.io.Source.fromFile(file)).map(_.mkString)

      ZLayer.succeed(files)

  trait Logging:
    def log(line: String): UIO[Unit]

  object Logging:
    /**
     * EXERCISE
     *
     * Using `ZLayer.fromFunction`, create a layer that requires `Console`
     * and uses the console to provide a logging service.
     */
    case class LoggingLive(console: Console) extends Logging:
      def log(line: String): UIO[Unit] =
        console.printLine(line).ignore

    val live: ZLayer[Console, Nothing, Logging] =
      ZLayer.fromFunction(LoggingLive.apply)

  /**
   * EXERCISE
   *
   * Discover the inferred type of `effect`, and write it out explicitly.
   */
  val effect: ZIO[LoggingWithFiles, IOException, Unit] =
    for
      files   <- ZIO.service[Files]
      logging <- ZIO.service[Logging]
      file    <- files.read("build.sbt")
      _       <- logging.log(file)
    yield ()

  val run: ZIO[Any, IOException, Unit] =

    /**
     * EXERCISE
     *
     * Create a layer using `ZLayer.make` and specifying all the pieces that go into the layer.
     */
    val fullLayer: ZLayer[Any, Nothing, LoggingWithFiles] =
      ZLayer.make[Files with Logging](Logging.live, Files.live, ZLayer.succeed(Console.ConsoleLive))

    /**
     * EXERCISE
     *
     * Using `ZIO#provide`, provide the full layer into the effect to remove its dependencies.
     */
    val effect: ZIO[Any, IOException, Unit] =
      LayerEnvironment.effect.provide(fullLayer)

    effect

object Example:
  // JAVA oop to Functional Scala OOP: Interfaces teturn ZIO instances.

  // ZLayer is the answer to how do you wire up your applications.
  // ZLayer[In, Error, Out] is a constructor that:
  // - has async / concurrent crating & finalization
  // - can fail in a type-safe way
  // - can take a long to to initialize
  // - can produce more than one thing
  // - can compose (when using Has/service as input / output)
  // - Every implementation in your application should have a ZLayer associated
  //   with it that describes how to build such a thing
  //   (i.e. a ZLayer for each case class seen above).

  // Build your apps against interfaces as well as implementations of those
  // interfaces for different configurations like test and production.
  // Coding to an interface is a good idea.

  object OOP:
    trait User
    trait UserID
    trait UserProfile
    trait DbConfig

    trait Database:
      def query(q: String): Task[java.sql.ResultSet]

    trait UserRepo:
      def getUserById(id: UserID): Task[User]
      def getUserProfileById(id: UserID): Task[UserProfile]

    trait TracingService:
      def getTraceId: Task[String]

    trait ProfileCache:
      def lookupCache(userId: UserID): Task[Option[UserProfile]]

    trait EmailService:
      def sendEmail(subject: String, body: String, to: String, from: String): Task[Unit]

    final case class UserRepoLive(db: Database) extends UserRepo:
      def getUserById(id: UserID): Task[User] = ???

      def getUserProfileById(id: UserID): Task[UserProfile] = ???

    final case class DatabaseLive(dbConfig: DbConfig) extends Database:
      def query(q: String): Task[ResultSet] = ???

      def initalize: Task[Unit] = ???
      def destroy: Task[Unit] = ???

    // Traditional way:
    val userRepoOld: ZIO[Scope, Throwable, UserRepo] =
      ZIO.scoped:
        val dbConfig: DbConfig =
          new DbConfig:
            val user: String = "myDbUser"
            val pass: String = "myDbPass"
            val host: String = "myDbHost"
            val port: Int    = 1234

        val db = DatabaseLive(dbConfig)

        ZIO.addFinalizer(db.destroy.ignore) *> db.initalize as UserRepoLive(db)

    val userRepoLive: ZLayer[Database, Nothing, UserRepo] =
      ZLayer.fromZIO:
        for
          db <- ZIO.service[Database]
        yield
          UserRepoLive(db)

    def databaseLive: ZLayer[DbConfig, Throwable, Database] =
      ZLayer.scoped:
        for
          dbConfig <- ZIO.service[DbConfig]
          dbLive   <- ZIO.succeed(DatabaseLive(dbConfig))
          _        <- dbLive.initalize
          _        <- ZIO.addFinalizer(dbLive.destroy.ignore)
        yield dbLive

    def dbConfigLayer: ZLayer[Any, Throwable, DbConfig] =
      ZLayer.succeed:
        new DbConfig:
          val user: String = "myDbUser"
          val pass: String = "myDbPass"
          val host: String = "myDbHost"
          val port: Int    = 1234

    val userRepo: ZLayer[Any, Throwable, UserRepo] =
      // dbConfigLayer >>> databaseLive >>> userRepoLive
      ZLayer.make[UserRepo](userRepoLive, databaseLive, dbConfigLayer)

    def myExampleApp(effect: ZIO[UserRepo, Throwable, Unit]): ZIO[Any, Throwable, Unit] =
      effect.provideLayer(userRepo)

  end OOP
