package net.degoes.zio

import zio.*
import zio.Schedule.WithState

import java.io.IOException

object Retry:

  /**
   * EXERCISE
   *
   * Using `Schedule.recurs`, create a schedule that recurs 5 times.
   */
  val fiveTimes: Schedule[Any, Any, Int] = Schedule.recurs(5).map(_.toInt)

  /**
   * EXERCISE
   *
   * Using the `ZIO.repeat`, repeat printing "Hello World" five times to the
   * console.
   */
  val repeated1: ZIO[Any, IOException, Unit] =
    Console.printLine("Hello, World!").repeat(fiveTimes).unit

  /**
   * EXERCISE
   *
   * Using `Schedule.spaced`, create a schedule that recurs forever every 1 second.
   */
  val everySecond: Schedule[Any, Any, Int] = Schedule.spaced(1.second).map(_.toInt)

  /**
   * EXERCISE
   *
   * Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
   * and the `everySecond` schedule, create a schedule that repeats fives times,
   * evey second.
   */
  val fiveTimesEverySecond: Schedule[Any, Any, (Int, Int)] = fiveTimes && everySecond

  /**
   * EXERCISE
   *
   * Using the `ZIO#repeat`, repeat the action Console.printLine("Hi hi") using
   * `fiveTimesEverySecond`.
   */
  val repeated2: ZIO[Any, IOException, Unit] =
    Console
      .printLine("Hi hi")
      .repeat(fiveTimesEverySecond)
      .unit

  /**
   * EXERCISE
   *
   * Using `Schedule#andThen` the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats fives times rapidly, and then
   * repeats every second forever.
   */
  val fiveTimesThenEverySecond: Schedule[Any, Any, Int] =
    fiveTimes andThen everySecond

  /**
   * EXERCISE
   *
   * Using `ZIO#retry`, retry the following error a total of five times.
   */
  val error1: ZIO[Any, String, Nothing]   = ZIO.fail("Uh oh!")
  val retried5: ZIO[Any, String, Nothing] = error1.retry(fiveTimes)

  /**
   * EXERCISE
   *
   * Using the `Schedule#||`, the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats the minimum of five times and
   * every second.
   */
  val fiveTimesOrEverySecond: Schedule[Any, Any, (Int, Int)] =
    fiveTimes || everySecond

  /**
   * EXERCISE
   *
   * Using `Schedule.exponential`, create an exponential schedule that starts
   * from 10 milliseconds.
   */
  val exponentialSchedule: Schedule[Any, Any, zio.Duration] =
    Schedule.exponential(10.millis)

  /**
   * EXERCISE
   *
   * Using `Schedule.jittered` produced a jittered version of `exponentialSchedule`.
   */
  val jitteredExponential: Schedule[Any, Any, zio.Duration] =
    exponentialSchedule.jittered

  /**
   * EXERCISE
   *
   * Using `Schedule.whileOutput`, produce a filtered schedule from `Schedule.forever`
   * that will halt when the number of recurrences exceeds 100.
   */
  val oneHundred: Schedule[Any, Any, Long] =
    Schedule.forever.whileOutput(_ <= 100)

  /**
   * EXERCISE
   *
   * Using `Schedule.identity`, produce a schedule that recurs forever, without delay,
   * returning its inputs.
   */
  def inputs[A]: Schedule[Any, A, A] = Schedule.identity[A]

  /**
   * EXERCISE
   *
   * Using `Schedule#collect`, produce a schedule that recurs forever, collecting its
   * inputs into a list.
   */
  def collectedInputs[A]: Schedule[Any, A, List[A]] =
    Schedule.forever.collectAll.map(_.toList.map(_.asInstanceOf[A]))

  /**
   * EXERCISE
   *
   * Using  `*>` (`zipRight`), combine `fiveTimes` and `everySecond` but return
   * the output of `everySecond`.
   */
  val fiveTimesEverySecondR: Schedule[Any, Any, Int] =
    fiveTimes *> everySecond

  /**
   * EXERCISE
   *
   * Produce a jittered schedule that first does exponential spacing (starting
   * from 10 milliseconds), but then after the spacing reaches 60 seconds,
   * switches over to fixed spacing of 60 seconds between recurrences, but will
   * only do that for up to 100 times, and produce a list of the inputs to
   * the schedule.
   */
  import Schedule.{ collectAll, exponential, fixed, recurs }

  def mySchedule[A]: Schedule[Any, A, List[A]] =
    (jitteredExponential.whileOutput(_ <= 60.seconds) *> (fixed(60.seconds) || recurs(100)))
      .collectAll.map(_.toList.map(_.asInstanceOf[A]))
