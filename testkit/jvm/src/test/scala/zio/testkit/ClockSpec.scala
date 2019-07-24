package zio.testkit

import java.util.concurrent.TimeUnit

import utest._
import zio._
import zio.duration._

object ClockSpec extends TestRuntime {
  override def tests: Tests = Tests {
    test("ClockSpec") {
      test("Sleep does sleep instantly") - {
        unsafeRun(
          for {
            ref       <- Ref.make(TestClock.Zero)
            testClock = TestClock(ref)
            result    <- testClock.sleep(10.hours).timeout(100.milliseconds)
          } yield assert(result.nonEmpty)
        )

      }
      test("Sleep passes nanotime correctly") - {
        unsafeRun(
          for {
            ref       <- Ref.make(TestClock.Zero)
            testClock = TestClock(ref)
            time1     <- testClock.nanoTime
            _         <- testClock.sleep(1.millis)
            time2     <- testClock.nanoTime
          } yield assert((time2 - time1) == 1000000L)
        )

      }
      test("Sleep passes currentTime correctly") - {
        unsafeRun(
          for {
            ref       <- Ref.make(TestClock.Zero)
            testClock = TestClock(ref)
            time1     <- testClock.currentTime(TimeUnit.MILLISECONDS)
            _         <- testClock.sleep(1.millis)
            time2     <- testClock.currentTime(TimeUnit.MILLISECONDS)
          } yield assert((time2 - time1) == 1L)
        )

      }
      test("Sleep passes currentDateTime correctly") - {
        unsafeRun(
          for {
            ref       <- Ref.make(TestClock.Zero)
            testClock = TestClock(ref)
            time1     <- testClock.currentDateTime
            _         <- testClock.sleep(1.millis)
            time2     <- testClock.currentDateTime
          } yield assert((time2.toInstant.toEpochMilli - time1.toInstant.toEpochMilli) == 1L)
        )

      }
      test("Sleep correctly records sleeps") - {
        unsafeRun(
          for {
            ref       <- Ref.make(TestClock.Zero)
            testClock = TestClock(ref)
            _         <- testClock.sleep(1.millis)
            sleeps    <- testClock.sleeps
          } yield assert(sleeps == List(1.milliseconds))
        )

      }
      test("Adjust correctly advances nanotime") - {
        unsafeRun(
          for {
            ref       <- Ref.make(TestClock.Zero)
            testClock = TestClock(ref)
            time1     <- testClock.nanoTime
            _         <- testClock.adjust(1.millis)
            time2     <- testClock.nanoTime
          } yield assert((time2 - time1) == 1000000L)
        )

      }
      test("Adjust correctly advances currentTime") - {
        unsafeRun(
          for {
            ref       <- Ref.make(TestClock.Zero)
            testClock = TestClock(ref)
            time1     <- testClock.currentTime(TimeUnit.MILLISECONDS)
            _         <- testClock.adjust(1.millis)
            time2     <- testClock.currentTime(TimeUnit.MILLISECONDS)
          } yield assert((time2 - time1) == 1L)
        )

      }
      test("Adjust correctly advances currentDateTime") - {
        unsafeRun(
          for {
            ref       <- Ref.make(TestClock.Zero)
            testClock = TestClock(ref)
            time1     <- testClock.currentDateTime
            _         <- testClock.adjust(1.millis)
            time2     <- testClock.currentDateTime
          } yield assert((time2.toInstant.toEpochMilli - time1.toInstant.toEpochMilli) == 1L)
        )

      }
      test("Adjust does not produce sleeps") - {
        unsafeRun(
          for {
            ref       <- Ref.make(TestClock.Zero)
            testClock = TestClock(ref)
            _         <- testClock.adjust(1.millis)
            sleeps    <- testClock.sleeps
          } yield assert(sleeps == Nil)
        )
      }
    }
  }
}
