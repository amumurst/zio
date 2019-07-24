package zio

import utest._
import zio.duration._
import zio.internal.stacktracer.ZTraceElement
import zio.internal.stacktracer.ZTraceElement.SourceLocation

object StacktracesSpec extends TestRuntime {

  // set to true to print traces
  private val debug = false

  override def tests: Tests = Tests {
    test("basic test") - basicTest

    test("foreach") - foreachTrace
    test("foreach fail") - foreachFail
    test("foreachPar fail") - foreachParFail
    test("foreachParN fail") - foreachParNFail

    test("left-associative fold") - leftAssociativeFold
    test("nested left binds") - nestedLeftBinds

    test("fiber ancestry") - fiberAncestry
    test("fiber ancestry example with uploads") - fiberAncestryUploadExample
    test("fiber ancestry has a limited size") - fiberAncestryIsLimited

    test("blocking trace") - blockingTrace

    test("tracing regions") - tracingRegions
    test("tracing region is inherited on fork") - tracingRegionsInheritance

    test("execution trace example with conditional") - executionTraceConditionalExample

    test("mapError fully preserves previous stack trace") - mapErrorPreservesTrace

    test("catchSome with optimized effect path") - catchSomeWithOptimizedEffect
    test("catchAll with optimized effect path") - catchAllWithOptimizedEffect
    test("foldM with optimized effect path") - foldMWithOptimizedEffect

    test("single effect for-comprehension") - singleEffectForComp
    test("single effectTotal for-comprehension") - singleEffectTotalForComp
    test("single suspendWith for-comprehension") - singleSuspendWithForComp
  }

  private def show(trace: ZTrace): Unit   = if (debug) println(trace.prettyPrint)
  private def show(cause: Cause[_]): Unit = if (debug) println(cause.prettyPrint)

  private def mentionsMethod(method: String, trace: List[ZTraceElement]): Boolean =
    trace.exists {
      case s: SourceLocation => s.method contains method
      case _                 => false
    }
  private def allMentionsMethod(method: String, trace: List[ZTraceElement]): Boolean =
    trace.forall {
      case s: SourceLocation => s.method contains method
      case _                 => false
    }

  private implicit final class CauseMust[R >: Environment](io: ZIO[R, _, _]) {
    def causeMust(check: Cause[_] => Unit): Unit =
      unsafeRunSync(io).fold[Unit](
        cause => {
          show(cause)
          check(cause)
        },
        _ => assert(false)
      )
  }

  def basicTest() = {
    val io = for {
      _     <- ZIO.unit
      trace <- ZIO.trace
    } yield trace

    val trace: ZTrace = unsafeRun(io)
    show(trace)

    assert(trace.executionTrace.size == 1)
    assert(mentionsMethod("basicTest", trace.executionTrace))
    assert(trace.stackTrace.size == 2)
    assert(mentionsMethod("basicTest", trace.stackTrace))
  }

  def foreachTrace() = {
    import foreachTraceFixture._

    val io = for {
      _     <- effectTotal
      _     <- ZIO.foreach_(1 to 10)(_ => ZIO.unit *> ZIO.trace)
      trace <- ZIO.trace
    } yield trace

    val trace: ZTrace = unsafeRun(io)
    show(trace)

    assert(trace.stackTrace.size == 2)
    assert(mentionsMethod("foreachTrace", trace.stackTrace))
    assert(mentionsMethod("foreachTrace", trace.executionTrace))
    assert(mentionsMethod("foreach_", trace.executionTrace))
    assert(mentionsMethod("effectTotal", trace.executionTrace))
  }

  object foreachTraceFixture {
    def effectTotal() = ZIO.effectTotal(())
  }

  def foreachFail() = {
    val io = for {
      t1 <- ZIO
             .foreach_(1 to 10) { i =>
               if (i == 7)
                 ZIO.unit *>
                   ZIO.fail("Dummy error!")
               else
                 ZIO.unit *>
                   ZIO.trace
             }
             .foldCauseM(e => IO(e.traces.head), _ => ZIO.dieMessage("can't be!"))
      t2 <- ZIO.trace
    } yield (t1, t2)

    val (trace1, trace2): (ZTrace, ZTrace) = unsafeRun(io)

    assert(mentionsMethod("foreach_", trace1.stackTrace))
    assert(mentionsMethod("foreachFail", trace1.stackTrace))
    assert(mentionsMethod("foreach_", trace1.executionTrace))
    assert(mentionsMethod("foreachFail", trace1.executionTrace))
    assert(trace2.stackTrace.size == 2)
    assert(mentionsMethod("foreachFail", trace2.stackTrace))
    assert(mentionsMethod("foreach_", trace2.executionTrace))
    assert(mentionsMethod("foreachFail", trace2.executionTrace))
  }

  def foreachParFail() = {
    val io = for {
      _ <- ZIO.foreachPar(1 to 10) { i =>
            ZIO.sleep(1.second) *> (if (i >= 7) UIO(i / 0) else UIO(i / 10))
          }
    } yield ()

    io causeMust { z =>
      assert(
        z.traces.head.stackTrace.size == 2,
        mentionsMethod("foreachParFail", z.traces.head.stackTrace)
      )
    }
  }

  def foreachParNFail() = {
    val io = for {
      _ <- ZIO.foreachParN(4)(1 to 10) { i =>
            ZIO.sleep(1.second) *> (if (i >= 7) UIO(i / 0) else UIO(i / 10))
          }
    } yield ()

    io causeMust { z =>
      assert(
        z.traces.head.stackTrace.size == 2,
        mentionsMethod("foreachParNFail", z.traces.head.stackTrace)
      )
    }
  }

  def leftAssociativeFold() = {
    val io: ZIO[Any, Nothing, ZTrace] =
      (1 to 10)
        .foldLeft(ZIO.unit *> ZIO.unit) { (acc, _) =>
          acc *> UIO(())
        } *>
        ZIO.unit *>
        ZIO.unit *>
        ZIO.unit *>
        ZIO.trace

    val trace: ZTrace = unsafeRun(io)
    show(trace)

    assert(trace.stackTrace.size == 1)
    assert(allMentionsMethod("leftAssociativeFold", trace.executionTrace))
  }

  def nestedLeftBinds() = {
    import nestedLeftBindsFixture._

    val (trace1, trace2): (ZTrace, ZTrace) = unsafeRun(io)
    show(trace1)
    show(trace2)

    assert(trace1.executionTrace.isEmpty)
    assert(trace1.stackTrace.size == 5)
    assert(mentionsMethod("method2", trace1.stackTrace))
    assert(mentionsMethod("method1", trace1.stackTrace))
    assert(mentionsMethod("io", trace1.stackTrace))

    assert(trace2.stackTrace.size == 2)
    assert(mentionsMethod("tuple", trace2.stackTrace))
    assert(mentionsMethod("method2", trace2.executionTrace))
    assert(mentionsMethod("method1", trace2.executionTrace))
    assert(mentionsMethod("io", trace2.executionTrace))
  }

  object nestedLeftBindsFixture {
    def method2() =
      for {
        trace <- ZIO.trace
        _     <- ZIO.unit
        _     <- ZIO.unit
        _     <- ZIO.unit
        _     <- UIO(())
      } yield trace

    def method1() =
      for {
        t <- method2
        _ <- ZIO.unit
        _ <- ZIO.unit
        _ <- ZIO.unit
      } yield t

    def tuple(t: ZTrace): ZTrace => (ZTrace, ZTrace) = t2 => (t, t2)

    val io =
      (for {
        t <- method1
        _ <- ZIO.unit
        _ <- ZIO.unit
        _ <- ZIO.unit
      } yield t)
        .foldM(
          failure = _ => IO.fail(()),
          success = t =>
            IO.trace
              .map(tuple(t))
        )
  }

  def fiberAncestry() = {

    def fiber0() =
      for {
        f1 <- fiber1.fork
        _  <- f1.join
      } yield ()

    def fiber1() =
      for {
        _  <- ZIO.unit
        _  <- ZIO.unit
        f2 <- fiber2.fork
        _  <- ZIO.unit
        _  <- f2.join
      } yield ()

    def fiber2() =
      for {
        _ <- UIO { throw new Exception() }
      } yield ()

    fiber0 causeMust { cause =>
      assert(
        cause.traces.nonEmpty,
        cause.traces.head.parentTrace.nonEmpty,
        cause.traces.head.parentTrace.get.parentTrace.nonEmpty,
        cause.traces.head.parentTrace.get.parentTrace.get.parentTrace.isEmpty
      )
    }
  }

  def fiberAncestryUploadExample() = {
    import fiberAncestryUploadExampleFixture._

    uploadUsers(List(new User)) causeMust { cause =>
      assert(
        cause.traces.head.stackTrace.size == 2,
        mentionsMethod("uploadUsers", cause.traces.head.stackTrace.headOption.toList),
        cause.traces(1).stackTrace.isEmpty,
        cause.traces(1).executionTrace.size == 1,
        mentionsMethod("uploadTo", cause.traces(1).executionTrace.headOption.toList),
        cause.traces(1).parentTrace.nonEmpty,
        mentionsMethod("uploadUsers", cause.traces(1).parentTrace.get.stackTrace)
      )
    }
  }

  object fiberAncestryUploadExampleFixture {
    sealed trait JSON
    final class User extends JSON
    final class URL
    def userDestination(): URL = new URL

    def uploadUsers(users: List[User]): Task[Unit] =
      for {
        _ <- IO.foreachPar(users)(uploadTo(userDestination))
      } yield ()

    def uploadTo(destination: URL)(json: JSON): Task[Unit] = {
      val _ = (destination, json)
      Task(throw new Exception("Expired credentials"))
    }
  }

  def fiberAncestryIsLimited() = {
    import fiberAncestryIsLimitedFixture._

    recursiveFork(10000) causeMust { cause =>
      assert(
        cause.traces.size == 1,
        cause.traces.head.parents.size == 10,
        mentionsMethod("recursiveFork", cause.traces.head.executionTrace),
        mentionsMethod("recursiveFork", cause.traces.head.parents.head.stackTrace)
      )
    }
  }

  object fiberAncestryIsLimitedFixture {
    def recursiveFork(i: Int): UIO[Unit] =
      i match {
        case 0 =>
          UIO(throw new Exception("oops!"))
        case _ =>
          ZIO.suspend(recursiveFork(i - 1)).fork.flatMap(_.join)
      }
  }

  def blockingTrace() = {
    val io = for {
      _ <- blocking.effectBlocking { throw new Exception() }
    } yield ()

    io causeMust { cause =>
      val trace = cause.traces.head

      // the bottom items on exec trace and stack trace refer to this line
      assert(
        mentionsMethod("blockingTrace", trace.stackTrace),
        mentionsMethod("blockingTrace", trace.executionTrace.lastOption.toList)
      )
    }

  }

  def tracingRegions() = {
    import tracingRegionsFixture._

    val io = (for {
      _ <- ZIO.unit
      _ <- ZIO.unit
      _ <- ZIO.effect(traceThis()).traced.traced.traced
      _ <- ZIO.unit
      _ <- ZIO.unit
      _ <- ZIO.fail("end")
    } yield ()).untraced

    io causeMust { cause =>
      assert(
        cause.traces.size == 1,
        mentionsMethod("traceThis", cause.traces.head.executionTrace),
        !mentionsMethod("tracingRegions", cause.traces.head.executionTrace),
        cause.traces.head.stackTrace.size == 1
      )
    }
  }

  object tracingRegionsFixture {
    val traceThis: () => String = () => "trace this!"
  }

  def tracingRegionsInheritance() = {
    val io: ZIO[Any, Nothing, Unit] = for {
      _ <- ZIO.unit
      _ <- ZIO.unit
      untraceableFiber <- (ZIO.unit *> (ZIO.unit *> ZIO.unit *> ZIO.dieMessage("error!") *> ZIO.checkTraced(
                           ZIO.succeed
                         )).fork).untraced
      tracingStatus <- untraceableFiber.join
      _             <- ZIO.when(tracingStatus.isTraced) { ZIO.dieMessage("Expected disabled tracing") }
    } yield ()

    io causeMust { cause =>
      assert(
        cause.traces.size == 1,
        cause.traces.head.executionTrace.isEmpty,
        cause.traces.head.stackTrace.isEmpty,
        cause.traces.head.parentTrace.isEmpty
      )
    }
  }

  def executionTraceConditionalExample() = {
    import executionTraceConditionalExampleFixture._

    val io = doWork(true)

    io causeMust { cause =>
      val trace = cause.traces.head

      assert(
        mentionsMethod("doSideWork", trace.executionTrace.lastOption.toList),
        mentionsMethod("doMainWork", trace.executionTrace),
        mentionsMethod("doWork", trace.stackTrace.headOption.toList)
      )
    }
  }

  object executionTraceConditionalExampleFixture {
    def doWork(condition: Boolean) =
      for {
        _ <- IO.when(condition)(doSideWork)
        _ <- doMainWork
      } yield ()

    def doSideWork() = Task(())
    def doMainWork() = Task(throw new Exception("Worker failed!"))
  }

  def singleEffectForComp() = {
    import singleTaskForCompFixture._

    selectHumans causeMust { cause =>
      assert(
        cause.traces.size == 1,
        cause.traces.head.stackTrace.size == 2,
        mentionsMethod("selectHumans", cause.traces.head.stackTrace.headOption.toList)
      )
    }
  }

  object singleTaskForCompFixture {
    def asyncDbCall(): Task[Unit] =
      Task(throw new Exception)

    val selectHumans: Task[Unit] = for {
      _ <- asyncDbCall()
    } yield ()
  }

  def singleEffectTotalForComp() = {
    import singleUIOForCompFixture._

    selectHumans causeMust { cause =>
      assert(
        cause.traces.size == 1,
        cause.traces.head.stackTrace.size == 2,
        mentionsMethod("selectHumans", cause.traces.head.stackTrace)
      )
    }
  }

  object singleUIOForCompFixture {
    def asyncDbCall(): Task[Unit] =
      UIO(throw new Exception)

    val selectHumans: Task[Unit] = for {
      _ <- asyncDbCall()
    } yield ()
  }

  def singleSuspendWithForComp() = {
    import singleEffectTotalWithForCompFixture._

    selectHumans causeMust { cause =>
      assert(
        cause.traces.size == 1,
        cause.traces.head.stackTrace.size == 2,
        mentionsMethod("selectHumans", cause.traces.head.stackTrace)
      )
    }
  }

  object singleEffectTotalWithForCompFixture {
    def asyncDbCall(): Task[Unit] =
      UIO.suspendWith(_ => throw new Exception)

    val selectHumans: Task[Unit] = for {
      _ <- asyncDbCall()
    } yield ()
  }

  def catchSomeWithOptimizedEffect() = {
    import catchSomeWithOptimizedEffectFixture._

    val io = for {
      t <- Task(fail())
            .flatMap(badMethod)
            .catchSome {
              case _: ArithmeticException => ZIO.fail("impossible match!")
            }
    } yield t

    io causeMust { cause =>
      assert(
        cause.traces.size == 1,
        cause.traces.head.executionTrace.size == 1,
        mentionsMethod("fail", cause.traces.head.executionTrace),
        cause.traces.head.stackTrace.size == 4,
        mentionsMethod("badMethod", cause.traces.head.stackTrace.headOption.toList),
        mentionsMethod("apply", cause.traces.head.stackTrace.drop(1).headOption.toList),
        mentionsMethod("catchSomeWithOptimizedEffect", cause.traces.head.stackTrace.drop(2).headOption.toList)
      )
    }
  }

  object catchSomeWithOptimizedEffectFixture {
    val fail      = () => throw new Exception("error!")
    val badMethod = ZIO.succeed(_: ZTrace)
  }

  def catchAllWithOptimizedEffect() = {
    import catchAllWithOptimizedEffectFixture._

    val io = for {
      t <- Task(fail())
            .flatMap(succ)
            .catchAll(refailAndLoseTrace)
    } yield t

    io causeMust { cause =>
      // after we refail and lose the trace, the only continuation we have left is the map from yield
      assert(
        cause.traces.size == 1,
        cause.traces.head.executionTrace.size == 2,
        mentionsMethod("refailAndLoseTrace", cause.traces.head.executionTrace.headOption.toList),
        mentionsMethod("fail", cause.traces.head.executionTrace.lastOption.toList),
        cause.traces.head.stackTrace.size == 2,
        mentionsMethod("catchAllWithOptimizedEffect", cause.traces.head.stackTrace.headOption.toList)
      )
    }
  }

  object catchAllWithOptimizedEffectFixture {
    val succ               = ZIO.succeed(_: ZTrace)
    val fail               = () => throw new Exception("error!")
    val refailAndLoseTrace = (_: Any) => ZIO.fail("bad!")
  }

  def mapErrorPreservesTrace() = {
    import mapErrorPreservesTraceFixture._

    val io = for {
      t <- Task(fail())
            .flatMap(succ)
            .mapError(mapError)
    } yield t

    io causeMust { cause =>
      // mapError does not change the trace in any way from its state during `fail()`
      // as a consequence, `executionTrace` is not updated with finalizer info, etc...
      // but overall it's a good thing since you're not losing traces at the border between your domain errors & Throwable
      assert(
        cause.traces.size == 1,
        cause.traces.head.executionTrace.size == 1,
        mentionsMethod("fail", cause.traces.head.executionTrace.headOption.toList),
        cause.traces.head.stackTrace.size == 4,
        mentionsMethod("succ", cause.traces.head.stackTrace.headOption.toList),
        mentionsMethod("mapError", cause.traces.head.stackTrace.drop(1).headOption.toList),
        mentionsMethod("mapErrorPreservesTrace", cause.traces.head.stackTrace.drop(2).headOption.toList)
      )
    }
  }

  object mapErrorPreservesTraceFixture {
    val succ     = ZIO.succeed(_: ZTrace)
    val fail     = () => throw new Exception("error!")
    val mapError = (_: Any) => ()
  }

  def foldMWithOptimizedEffect() = {
    import foldMWithOptimizedEffectFixture._

    val io = for {
      t <- Task(fail())
            .flatMap(badMethod1)
            .foldM(mkTrace, badMethod2)
    } yield t

    val trace: ZTrace = unsafeRun(io)
    show(trace)

    assert(
      trace.stackTrace.size == 2,
      mentionsMethod("foldMWithOptimizedEffect", trace.stackTrace),
      trace.executionTrace.size == 2,
      mentionsMethod("mkTrace", trace.executionTrace.headOption.toList),
      mentionsMethod("fail", trace.executionTrace.lastOption.toList)
    )
  }

  object foldMWithOptimizedEffectFixture {
    val mkTrace    = (_: Any) => ZIO.trace
    val fail       = () => throw new Exception("error!")
    val badMethod1 = ZIO.succeed(_: ZTrace)
    val badMethod2 = ZIO.succeed(_: ZTrace)
  }

}
