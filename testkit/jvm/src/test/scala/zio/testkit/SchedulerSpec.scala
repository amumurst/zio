package zio.testkit

import zio._
import utest._
import zio.clock.Clock
import zio.duration._

object SchedulerSpec extends TestRuntime {
  def mkScheduler(runtime: Runtime[Clock]): UIO[(TestClock, TestScheduler)] =
    for {
      clockData <- Ref.make(TestClock.Zero)
      clock     = TestClock(clockData)
      scheduler = TestScheduler(clockData, runtime)
    } yield (clock, scheduler)

  override def tests: Tests = Tests {
    test("Scheduled tasks get executed") - {
      unsafeRun(
        for {
          res       <- mkScheduler(this)
          clock     = res._1
          scheduler <- res._2.scheduler
          promise   <- Promise.make[Nothing, Unit]
          _ <- ZIO.effectTotal(scheduler.schedule(new Runnable {
                override def run(): Unit = unsafeRun(promise.done(ZIO.unit).unit)
              }, 10.seconds))
          _        <- clock.sleep(10.seconds)
          _        <- scheduler.safeShutdown()
          executed <- promise.poll.map(_.nonEmpty)
        } yield assert(executed)
      )
    }
    test("Scheduled tasks only get executed when time has passed") - {
      unsafeRun(
        for {
          res       <- mkScheduler(this)
          clock     = res._1
          scheduler <- res._2.scheduler
          promise   <- Promise.make[Nothing, Unit]
          _ <- ZIO.effectTotal(scheduler.schedule(new Runnable {
                override def run(): Unit = unsafeRun(promise.done(ZIO.unit).unit)
              }, 10.seconds + 1.nanosecond))
          _        <- clock.sleep(10.seconds)
          _        <- scheduler.safeShutdown()
          executed <- promise.poll.map(_.nonEmpty)
        } yield assert(!executed)
      )
    }
    test("Scheduled tasks can be canceled") - {
      unsafeRun(
        for {
          res       <- mkScheduler(this)
          clock     = res._1
          scheduler <- res._2.scheduler
          promise   <- Promise.make[Nothing, Unit]
          cancel <- ZIO.effectTotal(scheduler.schedule(new Runnable {
                     override def run(): Unit = unsafeRun(promise.done(ZIO.unit).unit)
                   }, 10.seconds))
          canceled <- ZIO.effectTotal(cancel())
          _        <- clock.sleep(10.seconds)
          _        <- scheduler.safeShutdown()
          executed <- promise.poll.map(_.nonEmpty)
        } yield assert(!executed && canceled)
      )
    }
    test("Tasks that are cancelled after completion are not reported as interrupted") - {
      unsafeRun(
        for {
          res       <- mkScheduler(this)
          clock     = res._1
          scheduler <- res._2.scheduler
          promise   <- Promise.make[Nothing, Unit]
          cancel <- ZIO.effectTotal(scheduler.schedule(new Runnable {
                     override def run(): Unit = unsafeRun(promise.done(ZIO.unit).unit)
                   }, 10.seconds))
          _        <- clock.sleep(10.seconds)
          _        <- scheduler.safeShutdown()
          canceled <- ZIO.effectTotal(cancel())
          executed <- promise.poll.map(_.nonEmpty)
        } yield assert(executed && !canceled)
      )

    }
  }
}
