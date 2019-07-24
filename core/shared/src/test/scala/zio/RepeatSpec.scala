package zio

import utest._
import zio.clock.Clock

object RepeatSpec extends BaseCrossPlatformSpec2 with UtestScalacheckExtension {

  override def tests: Tests = Tests {
    test("Repeat on success according to a provided strategy") {
      test("for 'recurs(a negative number)' repeats 0 additional time") - unsafeRun(repeatNeg)
      test("for 'recurs(0)' does repeats 0 additional time") - unsafeRun(repeat0)
      test("for 'recurs(1)' does repeats 1 additional time") - unsafeRun(repeat1)
      test("for 'once' does repeats 1 additional time") - unsafeRun(once)
      test("for 'recurs(a positive given number)' repeats that additional number of time") - unsafeRun(repeatN)
    }
    test("Repeat on failure does not actually repeat") - unsafeRun(repeatFail)
    test("Repeat a scheduled repeat repeats the whole number") - unsafeRun(repeatRepeat)
    test("Repeat an action 2 times and call `ensuring` should") {
      test("run the specified finalizer as soon as the schedule is complete") - unsafeRun(ensuring)
    }
  }

  val repeat: Int => ZIO[Clock, Nothing, Int] = (n: Int) =>
    for {
      ref <- Ref.make(0)
      s   <- ref.update(_ + 1).repeat(Schedule.recurs(n))
    } yield s

  /*
   * A repeat with a negative number of times should not repeat the action at all
   */
  def repeatNeg() =
    repeat(-5).map(x => x must_=== 1)

  /*
   * A repeat with 0 number of times should not repeat the action at all
   */
  def repeat0() =
    repeat(0).map(x => x must_=== 1)

  def never() =
    for {
      ref <- Ref.make(0)
      _   <- ref.update(_ + 1).repeat(Schedule.never)
      res <- ref.get
    } yield res must_=== 1

  def repeat1() =
    repeat(1).map(x => x must_=== 2)

  def once() =
    for {
      ref <- Ref.make(0)
      _   <- ref.update(_ + 1).repeat(Schedule.once)
      res <- ref.get
    } yield res must_=== 2

  def repeatN() =
    repeat(42).map(x => x must_=== 42 + 1)

  def repeatRepeat() = {
    val n = 42
    for {
      ref <- Ref.make(0)
      io  = ref.update(_ + 1).repeat(Schedule.recurs(n))
      _   <- io.repeat(Schedule.recurs(1))
      res <- ref.get
    } yield res must_=== (n + 1) * 2
  }

  def repeatFail() = {
    // a method that increment ref and fail with the incremented value in error message
    def incr(ref: Ref[Int]): IO[String, Int] =
      for {
        i <- ref.update(_ + 1)
        _ <- IO.fail(s"Error: $i")
      } yield i

    val repeated =
      (for {
        ref <- Ref.make(0)
        _   <- incr(ref).repeat(Schedule.recurs(42))
      } yield ()).foldM(
        err => IO.succeed(err),
        _ => IO.succeed("it should not be a success at all")
      )

    repeated.map(x => x must_=== "Error: 1")
  }

  def ensuring() =
    for {
      p          <- Promise.make[Nothing, Unit]
      r          <- Ref.make(0)
      _          <- r.update(_ + 2).repeat(Schedule.recurs(2)).ensuring(p.succeed(()))
      v          <- r.get
      finalizerV <- p.poll
    } yield assert(v == 6, finalizerV.isDefined)
}
