package zio.blocking

import java.util.concurrent.atomic.AtomicBoolean

import utest._
import zio.duration.Duration
import zio.{ TestRuntime, UIO }

object BlockingSpec extends TestRuntime {
  override def tests: Tests = Tests {
    test("Make a Blocking Service and verify that") {
      test("`effectBlocking` completes successfully") - {
        unsafeRun(effectBlocking(()))
      }
      test("`effectBlockingCancelable` completes successfully") - {
        unsafeRun(effectBlockingCancelable(())(UIO.unit))
      }
      test("`effectBlocking` can be interrupted") - {
        val res = unsafeRun(effectBlocking(Thread.sleep(50000)).timeout(Duration.Zero))
        assert(res.isEmpty)
      }

      def blocking(released: AtomicBoolean) =
        while (!released.get()) {
          try {
            Thread.sleep(10L)
          } catch {
            case _: InterruptedException => ()
          }
        }

      test("`effectBlockingCancelable` can be interrupted") - {
        val release = new AtomicBoolean(false)
        val cancel  = UIO.effectTotal(release.set(true))
        val res     = unsafeRun(effectBlockingCancelable(blocking(release))(cancel).timeout(Duration.Zero))
        assert(res.isEmpty)
      }
    }
  }
}
