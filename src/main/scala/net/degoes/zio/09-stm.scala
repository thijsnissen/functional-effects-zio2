package net.degoes.zio

import net.degoes.zio.StmLunchTime.Attendee.State.Starving
import zio.*

import java.io.IOException

object StmSwap extends ZIOAppDefault:
  /**
   * EXERCISE
   *
   * Demonstrate the following code does not reliably swap two values in the
   * presence of concurrency.
   */
  def exampleRef: UIO[Int] =
    def swap[A](ref1: Ref[A], ref2: Ref[A]): UIO[Unit] =
      for
        v1 <- ref1.get
        v2 <- ref2.get
        _  <- ref2.set(v1)
        _  <- ref1.set(v2)
      yield ()

    for
      ref1   <- Ref.make(100)
      ref2   <- Ref.make(0)
      fiber1 <- swap(ref1, ref2).repeatN(100).fork
      fiber2 <- swap(ref2, ref1).repeatN(100).fork
      _      <- (fiber1 zip fiber2).join
      value  <- (ref1.get zipWith ref2.get)(_ + _)
    yield value

  /**
   * EXERCISE
   *
   * Using `STM`, implement a safe version of the swap function.
   */
  def exampleStm: UIO[Int] =
    // ZSTM[R, E, A]
    //  STM[E, A]    = ZSTM[Any, E, A]
    // USTM[A]       = ZSTM[Any, Nothing, A]
    import zio.stm.*

    def swap[A](ref1: TRef[A], ref2: TRef[A]): UIO[Unit] =
      (for
        v1 <- ref1.get
        v2 <- ref2.get
        _ <- ref2.set(v1)
        _ <- ref1.set(v2)
      yield ()).commit

    for
      ref1   <- TRef.make(100).commit
      ref2   <- TRef.make(0).commit
      fiber1 <- swap(ref1, ref2).repeatN(100).fork
      fiber2 <- swap(ref2, ref1).repeatN(100).fork
      _      <- (fiber1 zip fiber2).join
      value  <- (ref1.get zipWith ref2.get)(_ + _).commit
    yield value

  val run: ZIO[Any, IOException, Unit] =
    exampleRef.map(_.toString).flatMap(Console.printLine(_))

object StmLock extends ZIOAppDefault:

  import zio.stm.*

  /**
   * EXERCISE
   *
   * Using STM, implement a simple binary lock by implementing the creation,
   * acquisition, and release methods.
   */
  class Lock private (tref: TRef[Boolean]):
    def acquire: UIO[Unit] =
      (for
        acquired <- tref.get
        _        <- if acquired then STM.retry else tref.set(true)
      yield ()).commit

    def release: UIO[Unit] =
      tref.set(false).commit

  object Lock:
    def make: UIO[Lock] =
      TRef.make(false).commit.map(tref => new Lock(tref))

  val run: ZIO[Any, IOException, Unit] =
    for
      lock <- Lock.make
      fiber1 <- ZIO
                 .acquireReleaseWith(lock.acquire)(_ => lock.release)(_ => Console.printLine("Bob  : I have the lock!"))
                 .repeat(Schedule.recurs(10))
                 .fork
      fiber2 <- ZIO
                 .acquireReleaseWith(lock.acquire)(_ => lock.release)(_ => Console.printLine("Sarah: I have the lock!"))
                 .repeat(Schedule.recurs(10))
                 .fork
      _ <- (fiber1 zip fiber2).join
    yield ()

object StmQueue extends ZIOAppDefault:

  import zio.stm.*
  import scala.collection.immutable.Queue as ScalaQueue

  /**
   * EXERCISE
   *
   * Using STM, implement a async queue with double back-pressuring.
   */
  class Queue[A] private (capacity: Int, queue: TRef[ScalaQueue[A]]):
    def take: UIO[A] =
      (for
        option <- queue.get.map(_.dequeueOption)
        a      <- option match
                    case Some(h, t) => queue.set(t).as(h)
                    case None       => STM.retry
      yield a).commit

    def offer(a: A): UIO[Unit] =
      (for
        size <- queue.get.map(_.size)
        _    <- if size < capacity then queue.update(_.enqueue(a)) else STM.retry
      yield ()).commit

  object Queue:
    def bounded[A](capacity: Int): UIO[Queue[A]] =
      TRef
        .make(ScalaQueue.empty[A])
        .commit
        .map(scalaQueue => new Queue(capacity, scalaQueue))

  val run: ZIO[Any, IOException, Unit] =
    for
      queue <- Queue.bounded[Int](10)
      _     <- ZIO.foreach(0 to 100)(i => queue.offer(i)).fork
      _     <- ZIO.foreachDiscard(0 to 100):
                 _ => queue.take.flatMap(i => Console.printLine(s"Got: $i"))
    yield ()

object StmLunchTime extends ZIOAppDefault:

  import zio.stm.*

  /**
   * EXERCISE
   *
   * Using STM, implement the missing methods of Attendee.
   */
  final case class Attendee(state: TRef[Attendee.State]):
    import Attendee.State.*

    def isStarving: STM[Nothing, Boolean] =
      for
        a <- state.get
        b <- a match
          case Starving => STM.succeed(true)
          case Full     => STM.succeed(false)
      yield b

    def feed: STM[Nothing, Unit] =
      for
        a    <- state.get
        bool <- isStarving
        _    <- if bool then state.set(Full) else STM.unit
      yield ()

  object Attendee:
    enum State:
      case Starving
      case Full

  /**
   * EXERCISE
   *
   * Using STM, implement the missing methods of Table.
   */
  final case class Table(seats: TArray[Boolean]):
    def findEmptySeat: STM[Nothing, Option[Int]] =
      seats
        .fold[(Int, Option[Int])]((0, None)):
          case ((index, z @ Some(_)), _) => (index + 1, z)
          case ((index, None), taken) =>
            (index + 1, if (taken) None else Some(index))
        .map(_._2)

    def takeSeat(index: Int): STM[Nothing, Unit] =
      seats.update(index, _ => true)

    def vacateSeat(index: Int): STM[Nothing, Unit] =
      seats.update(index, _ => false)

  /**
   * EXERCISE
   *
   * Using STM, implement a method that feeds a single attendee.
   */
  def feedAttendee(t: Table, a: Attendee): STM[Nothing, Unit] =
    for
      seat <- t.findEmptySeat
      no   <- seat match
        case Some(i) => STM.succeed(i)
        case None    => STM.retry
      _    <- t.takeSeat(no)
      _    <- a.feed
      _    <- t.vacateSeat(no)
    yield ()

  /**
   * EXERCISE
   *
   * Using STM, implement a method that feeds only the starving attendees.
   */
  def feedStarving(table: Table, attendees: Iterable[Attendee]): UIO[Unit] =
    for
      at <- ZIO.filter(attendees)(a => ZIO.succeed(a == Starving))
      _  <- ZIO.foreachDiscard(at)(a => feedAttendee(table, a).commit)
    yield ()

  val run: ZIO[Any, Any, ExitCode] =
    val Attendees = 100
    val TableSize = 5

    for
      attendees <-
        ZIO.foreach(0 to Attendees):
          i =>
            TRef
              .make[Attendee.State](Attendee.State.Starving)
              .map(Attendee(_))
              .commit
      table <-
        TArray
          .fromIterable(List.fill(TableSize)(false))
          .map(a => Table(a))
          .commit
      _ <- feedStarving(table, attendees)
    yield ExitCode.success

// TODO
object StmPriorityQueue extends ZIOAppDefault:
  import zio.stm.*

  /**
   * EXERCISE
   *
   * Using STM, design a priority queue, where smaller integers are assumed
   * to have higher priority than greater integers.
   */
  class PriorityQueue[A] private (minLevel: TRef[Option[Int]], map: TMap[Int, TQueue[A]]):
    def offer(a: A, priority: Int): STM[Nothing, Unit] =
      ???

    def take: STM[Nothing, A] =
      ???

  object PriorityQueue:
    def make[A]: STM[Nothing, PriorityQueue[A]] =
      ???

  val run: ZIO[Any, IOException, Int] =
    for
      _     <- Console.printLine("Enter any key to exit...")
      queue <- PriorityQueue.make[String].commit

      lowPriority = ZIO.foreach(0 to 100):
        i =>
          queue
            .offer(s"Offer: $i with priority 3", 3)
            .commit.delay(1.millis)

      highPriority = ZIO.foreach(0 to 100):
        i =>
          queue
            .offer(s"Offer: $i with priority 0", 0)
            .commit.delay(2.millis)

      _ <- ZIO.forkAll(List(lowPriority, highPriority)) *> queue.take.commit
            .flatMap(Console.printLine(_))
            .forever
            .fork *>
            Console.readLine
    yield 0

// TODO
object StmReentrantLock extends ZIOAppDefault {

  import zio.stm._

  private final case class WriteLock(
    writeCount: Int,
    readCount: Int,
    fiberId: FiberId
  )
  private final class ReadLock private (readers: Map[FiberId, Int]) {
    def total: Int = readers.values.sum

    def noOtherHolder(fiberId: FiberId): Boolean =
      readers.size == 0 || (readers.size == 1 && readers.contains(fiberId))

    def readLocks(fiberId: FiberId): Int =
      readers.get(fiberId).fold(0)(identity)

    def adjust(fiberId: FiberId, adjust: Int): ReadLock = {
      val total = readLocks(fiberId)

      val newTotal = total + adjust

      new ReadLock(
        readers =
          if (newTotal == 0) readers - fiberId
          else readers.updated(fiberId, newTotal)
      )
    }
  }
  private object ReadLock {
    val empty: ReadLock = new ReadLock(Map())

    def apply(fiberId: FiberId, count: Int): ReadLock =
      if (count <= 0) empty else new ReadLock(Map(fiberId -> count))
  }

  /**
   * EXERCISE
   *
   * Using STM, implement a reentrant read/write lock.
   */
  private class ReentrantReadWriteLock(data: TRef[Either[ReadLock, WriteLock]]) {
    def writeLocks: UIO[Int] = ???

    def writeLocked: UIO[Boolean] = ???

    def readLocks: UIO[Int] = ???

    def readLocked: UIO[Boolean] = ???

    val read: ZIO[Scope, Nothing, Int] = ???

    val write: ZIO[Scope, Nothing, Int] = ???
  }
  object ReentrantReadWriteLock {
    private def make: UIO[ReentrantReadWriteLock] = ???
  }

  val run = ???
}

object StmDiningPhilosophers extends ZIOAppDefault:

  import zio.stm._
  import java.io.IOException

  sealed trait Fork
  val Fork: Fork = new Fork {}

  final case class Placement(
    left: TRef[Option[Fork]],
    right: TRef[Option[Fork]]
  )

  final case class Roundtable(seats: Vector[Placement])

  /**
   * EXERCISE
   *
   * Using STM, implement the logic of a philosopher to take not one fork, but
   * both forks when they are both available.
   */
  def takeForks(
    left: TRef[Option[Fork]],
    right: TRef[Option[Fork]]
  ): STM[Nothing, (Fork, Fork)] =
    for
      leftFork <- left.get.collect:
        case Some(fork) => fork
      rightFork <- right.get.collect:
        case Some(fork) => fork
    yield (leftFork, rightFork)

  /**
   * EXERCISE
   *
   * Using STM, implement the logic of a philosopher to release both forks.
   */
  def putForks(
    left: TRef[Option[Fork]],
    right: TRef[Option[Fork]]
  )(
    tuple: (Fork, Fork)
  ): STM[Nothing, Unit] =
    val (leftFork, rightFork) = tuple

    left.set(Some(leftFork)) *> right.set(Some(rightFork))

  def setupTable(size: Int): ZIO[Any, Nothing, Roundtable] =
    val makeFork = TRef.make[Option[Fork]](Some(Fork))

    (for
      allForks0 <- STM.foreach(0 to size):
        _ => makeFork
      allForks = allForks0 ++ List(allForks0.head)
      placements = (allForks zip allForks.drop(1)).map:
        case (l, r) => Placement(l, r)
    yield Roundtable(placements.toVector)).commit

  def eat(
    philosopher: Int,
    roundtable: Roundtable
  ): ZIO[Any, IOException, Unit] =
    val placement = roundtable.seats(philosopher)

    val left  = placement.left
    val right = placement.right

    for
      forks <- takeForks(left, right).commit
      _     <- Console.printLine(s"Philosopher $philosopher eating...")
      _     <- putForks(left, right)(forks).commit
      _     <- Console.printLine(s"Philosopher $philosopher is done eating")
    yield ()

  val run: ZIO[Any, IOException, Unit] =
    val count = 10

    def eaters(table: Roundtable): Iterable[ZIO[Any, IOException, Unit]] =
      (0 to count).map:
        index => eat(index, table)

    for
      table <- setupTable(count)
      fiber <- ZIO.forkAll(eaters(table))
      _     <- fiber.join
      _     <- Console.printLine("All philosophers have eaten!")
    yield ()
