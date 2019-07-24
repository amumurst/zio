package zio.internal

import java.util.concurrent.RejectedExecutionException

import scala.concurrent.ExecutionContext
import utest._
import zio.UtestScalacheckExtension

final class TestExecutor(val submitResult: Boolean) extends Executor {
  val here: Boolean                       = true
  def shutdown(): Unit                    = ()
  def submit(runnable: Runnable): Boolean = submitResult
  def yieldOpCount: Int                   = 1
  def metrics: None.type                  = None
}

final class CheckPrintThrowable extends Throwable {
  var printed = false

  override def printStackTrace(): Unit = printed = true
}

object TestExecutor {
  val failing = new TestExecutor(false)
  val y       = new TestExecutor(true)
  val u       = new TestExecutor(true)

  val badEC = new ExecutionContext {
    override def execute(r: Runnable): Unit            = throw new RejectedExecutionException("Rejected: " + r.toString)
    override def reportFailure(cause: Throwable): Unit = ()
  }

  val ec = new ExecutionContext {
    override def execute(r: Runnable): Unit            = ()
    override def reportFailure(cause: Throwable): Unit = ()
  }
}

object ExecutorSpec extends TestSuite with UtestScalacheckExtension {
  override def tests: Tests = Tests {
    test("Create the default unyielding executor and check that") {
      test("When converted to an EC, it reports Throwables to stdout") - {
        val t = new CheckPrintThrowable
        TestExecutor.failing.asEC.reportFailure(t)
        assert(t.printed)
      }
    }
    test("Create an executor that cannot have tasks submitted to and check that") {
      test("It throws an exception upon submission") - {
        intercept[RejectedExecutionException](TestExecutor.failing.submitOrThrow(() => ()))
      }
      test("When converted to an ExecutionContext, it throws an exception") - {
        intercept[RejectedExecutionException](TestExecutor.failing.asEC.execute(() => ()))
      }
      test("When created from an EC, throw when fed an effect") - {
        intercept[RejectedExecutionException](
          Executor
            .fromExecutionContext(1)(TestExecutor.badEC)
            .submitOrThrow(() => ())
        )
      }
    }

    test("Create a yielding executor and check that") {
      test("Runnables can be submitted") - {
        interceptNot[RejectedExecutionException](TestExecutor.y.submitOrThrow(() => ()))
      }
      test("When converted to an ExecutionContext, it accepts Runnables") - {
        interceptNot[RejectedExecutionException](TestExecutor.y.asEC.execute(() => ()))
      }
      test("When created from an EC, must not throw when fed an effect") - {
        assert(Executor.fromExecutionContext(1)(TestExecutor.ec).submit(() => ()))
      }
    }
    test("Create an unyielding executor and check that") {
      test("Runnables can be submitted") - {
        interceptNot[RejectedExecutionException](TestExecutor.u.submitOrThrow(() => ()))
      }
      test("When converted to an ExecutionContext, it accepts Runnables") - {
        interceptNot[RejectedExecutionException](TestExecutor.u.asEC.execute(() => ()))
      }
    }
  }
}
