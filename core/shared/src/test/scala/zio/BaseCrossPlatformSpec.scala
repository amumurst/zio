package zio

import java.util.Timer

import utest.TestSuite
import zio.internal.PlatformLive

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{ Duration, _ }

abstract class BaseCrossPlatformSpec2 extends TestSuite with DefaultRuntime {
  override val Platform = PlatformLive.makeDefault().withReportFailure(_ => ())

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  val DefaultTimeout: Duration = 60.seconds
  val timer                    = new Timer()

  def exactlyOnce[R, A, A1](value: A)(func: UIO[A] => ZIO[R, String, A1]): ZIO[R, String, A1] =
    Ref.make(0).flatMap { ref =>
      for {
        res   <- func(ref.update(_ + 1) *> ZIO.succeed(value))
        count <- ref.get
        _ <- if (count != 1) {
              ZIO.fail("Accessed more than once")
            } else {
              ZIO.succeed(())
            }
      } yield res
    }

  def withLatch[R, E, A](f: UIO[Unit] => ZIO[R, E, A]): ZIO[R, E, A] =
    Promise.make[Nothing, Unit] >>= (latch => f(latch.succeed(()).unit) <* latch.await)

  def withLatch[R, E, A](f: (UIO[Unit], UIO[Unit]) => ZIO[R, E, A]): ZIO[R, E, A] =
    for {
      ref   <- Ref.make(true)
      latch <- Promise.make[Nothing, Unit]
      a <- f(latch.succeed(()).unit, ZIO.uninterruptibleMask { restore =>
            ref.set(false) *> restore(latch.await)
          })
      _ <- UIO.whenM(ref.get)(latch.await)
    } yield a
  implicit class ZIOMustExpectable[R, E, A](zio: ZIO[R, E, A]) {
    def mustFailBecauseOf(cause: Cause[E]): ZIO[R, A, Unit] =
      zio.sandbox.flip.map(error => assert(error == cause))

  }
}
