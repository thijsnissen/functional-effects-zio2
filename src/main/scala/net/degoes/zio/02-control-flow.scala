package net.degoes.zio

import zio.*
import scala.collection.immutable.Nil
import scala.annotation.tailrec
import java.io.IOException

object Looping extends ZIOAppDefault:

  /**
   * EXERCISE
   *
   * Implement a `repeat` combinator using `flatMap` (or `zipRight`) and recursion.
   */
  def repeat[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    if (n <= 0) ZIO.succeed(Chunk.empty)
    else repeat(n - 1)(effect).flatMap: chunk =>
      effect.map(a => chunk :+ a)

    @annotation.tailrec
    def loop(i: Int, z: ZIO[R, E, Chunk[A]]): ZIO[R, E, Chunk[A]] =
      if i <= 0 then z
      else loop(i - 1, z.flatMap(chunk => effect.map(a => chunk :+ a)))

    loop(n, ZIO.succeed(Chunk.empty[A]))

  val run: ZIO[Any, IOException, Chunk[Unit]] =
    repeat(100)(Console.printLine("All work and no play makes Jack a dull boy"))

object Interview extends ZIOAppDefault:
  import java.io.IOException

  val questions: List[String] =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `getAllAnswers` function in such a fashion that it will ask
   * the user each question and collect them all into a list.
   */
  def getAllAnswers(questions: List[String]): ZIO[Any, IOException, List[String]] =
    questions match
      case Nil     => ZIO.succeed(Nil)
      case q :: qs =>
        for
          _ <- Console.printLine(q)
          a <- Console.readLine
          b <- getAllAnswers(qs)
        yield a :: b

  /**
   * EXERCISE
   *
   * Use the preceding `getAllAnswers` function, together with the predefined
   * `questions`, to ask the user a bunch of questions, and print the answers.
   */
  val run: ZIO[Any, IOException, Unit] =
    for
      l <- getAllAnswers(questions)
      _ <- ZIO.foldLeft(l)(()):
        (_, a) => Console.printLine(a)
    yield ()

object InterviewGeneric extends ZIOAppDefault:
  import java.io.IOException

  val questions: List[String] =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `iterateAndCollect` function.
   */
  def iterateAndCollect[R, E, A, B](as: List[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    as match
      case Nil     => ZIO.succeed(Nil)
      case a :: as =>
        for
          b <- f(a)
          c <- iterateAndCollect(as)(f)
        yield b :: c

  val run: ZIO[Any, IOException, List[String]] =
    iterateAndCollect(questions): q =>
      for
        _ <- Console.printLine(q)
        a <- Console.readLine
      yield a

object InterviewForeach extends ZIOAppDefault:

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Using `ZIO.foreach`, iterate over each question in `questions`, print the
   * question to the user (`Console.printLine`), read the answer from the user
   * (`Console.readLine`), and collect all answers into a collection. Finally, print
   * out the contents of the collection.
   */
  val run: ZIO[Any, IOException, Unit] =
    ZIO
      .foreach(questions): q =>
        for
          _ <- Console.printLine(q)
          a <- Console.readLine
        yield a
      .flatMap(Console.printLine(_))

object WhileLoop extends ZIOAppDefault:

  /**
   * EXERCISE
   *
   * Implement the functional effect version of a while loop so the
   * application runs correctly.
   */
  def whileLoop[R, E, A](cond: UIO[Boolean])(zio: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    for
      b <- cond
      c <- if b then zio.map(Chunk(_)).zipWith(whileLoop(cond)(zio))(_ ++ _)
           else ZIO.succeed(Chunk.empty)
    yield c

  val run: ZIO[Any, IOException, RuntimeFlags] =
    def loop(variable: Ref[Int]) =
      whileLoop(variable.get.map(_ < 100))
        for
          value <- variable.get
          _     <- Console.printLine(s"At iteration: $value")
          _     <- variable.update(_ + 1)
        yield ()

    for
      variable <- Ref.make(0)
      _        <- loop(variable)
    yield 0

object Iterate extends ZIOAppDefault:

  /**
   * EXERCISE
   *
   * Implement the `iterate` function such that it iterates until the condition
   * evaluates to false, returning the "last" value of type `A`.
   */
  def iterate[R, E, A](start: A)(cond: A => Boolean)(f: A => ZIO[R, E, A]): ZIO[R, E, A] =
    if cond(start) then f(start).flatMap(s => iterate(s)(cond)(f)) else ZIO.succeed(start)

  val run: ZIO[Any, IOException, Int] =
    iterate(0)(_ < 100): i =>
      Console.printLine(s"At iteration: $i").as(i + 1)

object TailRecursive extends ZIOAppDefault:

  trait Response

  trait Request:
    def returnResponse(response: Response): Task[Unit]

  lazy val acceptRequest: Task[Request] = ZIO.attempt:
    new Request:
      def returnResponse(response: Response): Task[Unit] =
        ZIO.attempt(println(s"Returning response $response"))

  def handleRequest(request: Request): Task[Response] = ZIO.attempt:
    println(s"Handling request $request")
    new Response {}

  /**
   * EXERCISE
   *
   * Make this infinite loop (which represents a webserver) effectfully tail
   * recursive.
   */
  lazy val webserver: Task[Nothing] =
    for
      request  <- acceptRequest
      response <- handleRequest(request)
      _        <- request.returnResponse(response)
      nothing  <- webserver
    yield nothing

  lazy val webserver2: Task[Nothing] =
    (for
      request <- acceptRequest
      response <- handleRequest(request)
      _ <- request.returnResponse(response)
    yield ()).forever

  lazy val webserver3: Task[Nothing] =
    acceptRequest.flatMap:
      request => handleRequest(request).flatMap:
        response => request.returnResponse(response) *> webserver

  val run: ZIO[Any, Nothing, Unit] =
    for
      fiber <- webserver3.fork
      _     <- ZIO.sleep(100.millis)
      _     <- fiber.interrupt
    yield ()
