package zio
import utest._

object PromiseSpec extends BaseCrossPlatformSpec2 {

  override def tests: Tests = Tests {
    test("Make a promise and retrieve its value correctly after complete it with:") {
      test("`complete` to complete that promise with a specified value.") - e1
      test("`done` to complete that promise with a completed result.") - e2
    }
    test("Make a promise and retrieve its fail value after complete it with:") {
      test("`error` to fail that promise with a specified error.") - e3
      test("`done` to complete that promise with a failed result.") - e4
    }
    test("Given a completed promise `done` returns false and get should return the first completed value.") - e5

    test("Make a promise and retrieve its Throwable value after interruption calling:") {
      test("`done` to complete that promise with a terminated result.") - e6
      test("`interrupt` and interrupt all other fibers.") - e7
    }
    test("poll retrieves the final status immediately") {
      test("it `fails' with Unit ` if the promise is not done yet.") - e8
      test("Otherwise, it returns the `Exit`, whether") {
        test("`succeeded`") - e9
        test("`failed`") - e10
        test("`interrupted`") - e11
      }
    }
    test("Make a Promise and expect it's `isDone` value to") {
      test("be `false` before it is completed") - e12
      test("be `true` after it has been completed") {
        test("with a value") - e13
        test("with a failure") - e14
      }
    }
  }

  def e1() =
    unsafeRun(
      for {
        p <- Promise.make[Nothing, Int]
        s <- p.succeed(32)
        v <- p.await
      } yield assert(s, v == 32)
    )

  def e2() =
    unsafeRun(
      for {
        p <- Promise.make[Nothing, Int]
        s <- p.done(IO.succeed(14))
        v <- p.await
      } yield assert(s, v == 14)
    )

  def e3() =
    unsafeRun(
      for {
        p <- Promise.make[String, Int]
        s <- p.fail("error in e3")
        v <- p.await.either
      } yield assert(s, v == Left("error in e3"))
    )

  def e4() =
    unsafeRun(
      for {
        p <- Promise.make[String, Int]
        s <- p.done(IO.fail("error in e4"))
        v <- p.await.either
      } yield assert(s, v == Left("error in e4"))
    )

  def e5() =
    unsafeRun(
      for {
        p <- Promise.make[Nothing, Int]
        _ <- p.succeed(1)
        s <- p.done(IO.succeed(9))
        v <- p.await
      } yield assert(!s, v == 1)
    )

  def e6() =
    unsafeRun(
      for {
        p <- Promise.make[Exception, Int]
        s <- p.interrupt
      } yield assert(s)
    )
  def e7() =
    unsafeRun(
      for {
        p <- Promise.make[Exception, Int]
        s <- p.interrupt
      } yield assert(s)
    )

  def e8() =
    unsafeRun(
      for {
        p       <- Promise.make[String, Int]
        attempt <- p.poll.get.either
      } yield assert(attempt.isLeft)
    )

  def e9() =
    unsafeRun {
      for {
        p      <- Promise.make[String, Int]
        _      <- p.succeed(12)
        result <- p.poll.get.flatMap(_.run)
      } yield assert(result == Exit.succeed(12))
    }

  def e10() =
    unsafeRun {
      for {
        p      <- Promise.make[String, Int]
        _      <- p.fail("failure")
        result <- p.poll.get.flatMap(_.run)
      } yield assert(result == Exit.fail("failure"))
    }

  def e11() =
    unsafeRun {
      for {
        p             <- Promise.make[String, Int]
        _             <- p.interrupt
        attemptResult <- p.poll.get.flatMap(_.run)
      } yield assert(attemptResult == Exit.interrupt)
    }

  def e12() =
    unsafeRun(
      for {
        p <- Promise.make[String, Int]
        d <- p.isDone
      } yield assert(!d)
    )

  def e13() =
    unsafeRun(
      for {
        p <- Promise.make[String, Int]
        _ <- p.succeed(0)
        d <- p.isDone
      } yield assert(d)
    )

  def e14() =
    unsafeRun(
      for {
        p <- Promise.make[String, Int]
        _ <- p.fail("failure")
        d <- p.isDone
      } yield assert(d)
    )

}
