package zio

import java.util.concurrent.Callable
import java.util.concurrent.atomic.AtomicInteger

import com.github.ghik.silencer.silent
import zio.Cause.{ die, fail, Fail, Then }
import zio.duration._
import zio.clock.Clock

import scala.annotation.tailrec
import scala.util.{ Failure, Success }
import utest._
object RTSSpec extends TestRuntime with UtestScalacheckExtension {

  override def tests: Tests = Tests {
    test("RTS synchronous correctness") {
      test("widen Nothing") - testWidenNothing
      test("evaluation of point") - testPoint
      test("blocking caches threads") - testBlockingThreadCaching
      test("point must be lazy") - testPointIsLazy
      test("now must be eager") - testNowIsEager
      test("suspend must be lazy") - testSuspendIsLazy
      test("suspend must be evaluatable") - testSuspendIsEvaluatable
      test("point, bind, map") - testSyncEvalLoop
      test("effect, bind, map") - testSyncEvalLoopEffect
      test("effect, bind, map, redeem") - testSyncEvalLoopEffectThrow
      test("sync effect") - testEvalOfSyncEffect
      test("sync on defer") - testManualSyncOnDefer
      test("deep effects") - testEvalOfDeepSyncEffect
      test("flip must make error into value") - testFlipError
      test("flip must make value into error") - testFlipValue
      test("flipping twice returns identical value") - testFlipDouble
    }
    test("RTS failure") {
      test("error in sync effect") - testEvalOfRedeemOfSyncEffectError
      test("attempt.fail") - testEvalOfAttemptOfFail
      test("deep attempt sync effect error") - testAttemptOfDeepSyncEffectError
      test("deep attempt fail error") - testAttemptOfDeepFailError
      test("attempt.sandbox.terminate") - testSandboxAttemptOfTerminate
      test("fold.sandbox.terminate") - testSandboxFoldOfTerminate
      test("catch sandbox terminate") - testSandboxTerminate
      test("uncaught fail") - testEvalOfUncaughtFail
      test("uncaught fail supervised") - testEvalOfUncaughtFailSupervised
      test("uncaught sync effect error") - testEvalOfUncaughtThrownSyncEffect
      test("uncaught supervised sync effect error") - testEvalOfUncaughtThrownSupervisedSyncEffect
      test("deep uncaught sync effect error") - testEvalOfDeepUncaughtThrownSyncEffect
      test("deep uncaught fail") - testEvalOfDeepUncaughtFail
      test("catch failing finalizers with fail") - testFailOfMultipleFailingFinalizers
      test("catch failing finalizers with terminate") - testTerminateOfMultipleFailingFinalizers
      test("run preserves interruption status") - testRunInterruptIsInterrupted
      test("run swallows inner interruption") - testRunSwallowsInnerInterrupt
      test("timeout a long computation") - testTimeoutOfLongComputation
      test("catchAllCause") - testCatchAllCause
    }
    test("RTS finalizers") {
      test("fail ensuring") - testEvalOfFailEnsuring
      test("fail on error") - testEvalOfFailOnError
      test("finalizer errors not caught") - testErrorInFinalizerCannotBeCaught
      test("finalizer errors reported") - testErrorInFinalizerIsReported
      test("bracket exit is usage result") - testExitIsUsageResult
      test("error in just acquisition") - testBracketErrorInAcquisition
      test("error in just release") - testBracketErrorInRelease
      test("error in just usage") - testBracketErrorInUsage
      test("rethrown caught error in acquisition") - testBracketRethrownCaughtErrorInAcquisition
      test("rethrown caught error in release") - testBracketRethrownCaughtErrorInRelease
      test("rethrown caught error in usage") - testBracketRethrownCaughtErrorInUsage
      test("test eval of async fail") - testEvalOfAsyncAttemptOfFail
      test("bracket regression 1") - testBracketRegression1
      test("interrupt waits for finalizer") - testInterruptWaitsForFinalizer
    }
    test("RTS synchronous stack safety") {
      test("deep map of point") - testDeepMapOfPoint
      test("deep map of now") - testDeepMapOfNow
      test("deep map of sync effect") - testDeepMapOfSyncEffectIsStackSafe
      test("deep attempt") - testDeepAttemptIsStackSafe
      test("deep flatMap") - testDeepFlatMapIsStackSafe
      test("deep absolve / attempt is identity") - testDeepAbsolveAttemptIsIdentity
      test("deep async absolve / attempt is identity") - testDeepAsyncAbsolveAttemptIsIdentity
    }
    test("RTS asynchronous correctness") {
      test("simple async must return") - testAsyncEffectReturns
      test("simple asyncIO must return") - testAsyncIOEffectReturns
      test("deep asyncIO doesn't block threads") - testDeepAsyncIOThreadStarvation
      test("interrupt of asyncPure register") - testAsyncPureInterruptRegister
      test("sleep 0 must return") - testSleepZeroReturns
      test("shallow bind of async chain") - testShallowBindOfAsyncChainIsCorrect
      test("effectAsyncM can fail before registering") - testEffectAsyncMCanFail
    }
    test("RTS concurrency correctness") {
      test("shallow fork / join identity") - testForkJoinIsId
      test("deep fork / join identity") - testDeepForkJoinIsId
      test("asyncPure creation is interruptible") - testAsyncPureCreationIsInterruptible
      test("asyncInterrupt runs cancel token on interrupt") - testAsync0RunsCancelTokenOnInterrupt
      test("supervising returns fiber refs") - testSupervising
      test("supervising in unsupervised returns Nil") - testSupervisingUnsupervised
      test("supervise fibers") - testSupervise
      test("supervise fibers in supervised") - testSupervised
      test("supervise fibers in race") - testSuperviseRace
      test("supervise fibers in fork") - testSuperviseFork
      test("race of fail with success") - testRaceChoosesWinner
      test("race of terminate with success") - testRaceChoosesWinnerInTerminate
      test("race of fail with fail") - testRaceChoosesFailure
      test("race of value & never") - testRaceOfValueNever
      test("raceAll of values") - testRaceAllOfValues
      test("raceAll of failures") - testRaceAllOfFailures
      test("raceAll of failures & one success") - testRaceAllOfFailuresOneSuccess
      test("firstSuccessOf of values") - testFirstSuccessOfValues
      test("firstSuccessOf of failures") - testFirstSuccessOfFailures
      test("firstSuccessOF of failures & 1 success") - testFirstSuccessOfFailuresOneSuccess
      test("raceAttempt interrupts loser on success") - testRaceAttemptInterruptsLoserOnSuccess
      test("raceAttempt interrupts loser on failure") - testRaceAttemptInterruptsLoserOnFailure
      test("par regression") - testPar
      test("par of now values") - testRepeatedPar
      test("mergeAll") - testMergeAll
      test("mergeAllEmpty") - testMergeAllEmpty
      test("reduceAll") - testReduceAll
      test("reduceAll Empty List") - testReduceAllEmpty
      test("timeout of failure") - testTimeoutFailure
      test("timeout of terminate") - testTimeoutTerminate
    }
    test("RTS regression tests") {
      test("deadlock regression 1") - testDeadlockRegression
      test("check interruption regression 1") - testInterruptionRegression1
      test("manual sync interruption") - testManualSyncInterruption
    }
    test("RTS option tests") {
      test("lifting a value to an option") - testLiftingOptionalValue
      test("using the none value") - testLiftingNoneValue
    }
    test("RTS either helper tests") {
      test("lifting a value into right") - liftValueIntoRight
      test("lifting a value into left") - liftValueIntoLeft
    }
    test("RTS interruption") {
      test("blocking IO is effect blocking") - testBlockingIOIsEffectBlocking
      test("sync forever is interruptible") - testInterruptSyncForever
      test("interrupt of never") - testNeverIsInterruptible
      test("asyncPure is interruptible") - testAsyncPureIsInterruptible
      test("async is interruptible") - testAsyncIsInterruptible
      test("bracket is uninterruptible") - testBracketAcquireIsUninterruptible
      test("bracket0 is uninterruptible") - testBracket0AcquireIsUninterruptible
      test("bracket use is interruptible") - testBracketUseIsInterruptible
      test("bracket0 use is interruptible") - testBracket0UseIsInterruptible
      test("bracket release called on interrupt") - testBracketReleaseOnInterrupt
      test("bracket0 release called on interrupt") - testBracket0ReleaseOnInterrupt
      test("redeem + ensuring + interrupt") - testRedeemEnsuringInterrupt
      test("finalizer can detect interruption") - testFinalizerCanDetectInterruption
      test("interruption of raced") - testInterruptedOfRaceInterruptsContestents
      test("cancelation is guaranteed") - testCancelationIsGuaranteed
      test("interruption of unending bracket") - testInterruptionOfUnendingBracket
      test("recovery of error in finalizer") - testRecoveryOfErrorInFinalizer
      test("recovery of interruptible") - testRecoveryOfInterruptible
      test("sandbox of interruptible") - testSandboxOfInterruptible
      test("run of interruptible") - testRunOfInterruptible
      test("alternating interruptibility") - testAlternatingInterruptibility
      test("interruption after defect") - testInterruptionAfterDefect
      test("interruption after defect 2") - testInterruptionAfterDefect2
      test("cause reflects interruption") - testCauseReflectsInterruption
      test("bracket use inherits interrupt status") - testUseInheritsInterruptStatus
      test("bracket use inherits interrupt status 2") - testCauseUseInheritsInterruptStatus
      test("async can be uninterruptible") - testAsyncCanBeUninterruptible
    }
    test("RTS environment") {
      test("provide is modular") - testProvideIsModular
      test("provideManaged is modular") - testProvideManagedIsModular
      test("effectAsync can use environment") - testAsyncCanUseEnvironment
    }
    test("RTS forking inheritability") {
      test("interruption status is heritable") - testInterruptStatusIsHeritable
      test("executor is hereditble") - testExecutorIsHeritable
      test("supervision is heritable") - testSupervisionIsHeritable
      test("supervision inheritance") - testSupervisingInheritance
    }
  }

  def testPoint() =
    assert(unsafeRun(IO.succeedLazy(1)) == 1)

  def testWidenNothing() = {
    val op1 = IO.effectTotal[String]("1")
    val op2 = IO.effectTotal[String]("2")

    val result: IO[RuntimeException, String] = for {
      r1 <- op1
      r2 <- op2
    } yield r1 + r2

    assert(unsafeRun(result) == "12")
  }

  def testPointIsLazy() =
    interceptNot[Throwable] {
      IO.succeedLazy(throw new Error("Not lazy"))
      ()
    }

  @silent
  def testNowIsEager() =
    intercept[Error](IO.succeed(throw new Error("Eager")))

  def testSuspendIsLazy() =
    interceptNot[Throwable] {
      IO.suspend(throw new Error("Eager"))
      ()
    }

  def testSuspendIsEvaluatable() =
    assert(unsafeRun(IO.suspend(IO.succeedLazy[Int](42))) == 42)

  def testSyncEvalLoop() = {
    def fibIo(n: Int): Task[BigInt] =
      if (n <= 1) IO.succeedLazy(n)
      else
        for {
          a <- fibIo(n - 1)
          b <- fibIo(n - 2)
        } yield a + b

    assert(unsafeRun(fibIo(10)) == fib(10))
  }

  def testSyncEvalLoopEffect() = {
    def fibIo(n: Int): Task[BigInt] =
      if (n <= 1) IO.effect(n)
      else
        for {
          a <- fibIo(n - 1)
          b <- fibIo(n - 2)
        } yield a + b

    assert(unsafeRun(fibIo(10)) == fib(10))
  }

  def testSyncEvalLoopEffectThrow() = {
    def fibIo(n: Int): Task[BigInt] =
      if (n <= 1) Task.effect[BigInt](throw new Error).catchAll(_ => Task.effect(n))
      else
        for {
          a <- fibIo(n - 1)
          b <- fibIo(n - 2)
        } yield a + b

    assert(unsafeRun(fibIo(10)) == fib(10))
  }

  def testFlipError() = {
    val error = new Error("Left")
    val io    = IO.fail(error).flip
    assert(unsafeRun(io) == error)
  }

  def testFlipValue() = {
    val io = IO.succeed(100).flip
    assert(unsafeRun(io.either) == Left(100))
  }

  def testFlipDouble() = {
    val io = IO.succeedLazy(100)
    assert(unsafeRun(io.flip.flip) == unsafeRun(io))
  }

  def testEvalOfSyncEffect() = {
    def sumIo(n: Int): Task[Int] =
      if (n <= 0) IO.effectTotal(0)
      else IO.effectTotal(n).flatMap(b => sumIo(n - 1).map(a => a + b))

    assert(unsafeRun(sumIo(1000)) == sum(1000))
  }

  def testManualSyncOnDefer() = {
    def sync[A](effect: => A): IO[Throwable, A] =
      IO.effectTotal(effect)
        .foldCauseM({
          case Cause.Die(t) => IO.fail(t)
          case cause        => IO.halt(cause)
        }, IO.succeed(_))

    def putStrLn(text: String): IO[Throwable, Unit] =
      sync(println(text))

    unsafeRun(putStrLn("Hello")) must_=== (())
  }

  @silent
  def testEvalOfRedeemOfSyncEffectError() =
    assert(
      unsafeRun(
        IO.effect[Unit](throw ExampleError).fold[Option[Throwable]](Some(_), _ => None)
      ) == Some(ExampleError)
    )

  def testEvalOfAttemptOfFail() = {
    assert(unsafeRun(TaskExampleError.either) == Left(ExampleError))
    assert(unsafeRun(IO.suspend(IO.suspend(TaskExampleError).either)) == Left(ExampleError))
  }

  def testSandboxAttemptOfTerminate() =
    assert(unsafeRun(IO.effectTotal[Int](throw ExampleError).sandbox.either) == Left(die(ExampleError)))

  def testSandboxFoldOfTerminate() =
    assert(
      unsafeRun(
        IO.effectTotal[Int](throw ExampleError).sandbox.fold(Some(_), Function.const(None))
      ) == Some(die(ExampleError))
    )

  def testSandboxTerminate() =
    assert(
      unsafeRun(
        IO.effectTotal[Cause[Any]](throw ExampleError)
          .sandbox
          .fold[Cause[Any]](identity, identity)
      ) == die(ExampleError)
    )

  def testAttemptOfDeepSyncEffectError() =
    assert(unsafeRun(deepErrorEffect(100).either) == Left(ExampleError))

  def testAttemptOfDeepFailError() =
    assert(unsafeRun(deepErrorFail(100).either) == Left(ExampleError))

  def testEvalOfUncaughtFail() =
    assert(unsafeRunSync(Task.fail(ExampleError): Task[Any]) == Exit.Failure(fail(ExampleError)))

  def testEvalOfUncaughtFailSupervised() =
    assert(unsafeRunSync(Task.fail(ExampleError).interruptChildren: Task[Unit]) == Exit.Failure(fail(ExampleError)))

  def testEvalOfUncaughtThrownSyncEffect() =
    assert(unsafeRunSync(IO.effectTotal[Int](throw ExampleError)) == Exit.Failure(die(ExampleError)))

  def testEvalOfUncaughtThrownSupervisedSyncEffect() =
    assert(unsafeRunSync(IO.effectTotal[Int](throw ExampleError).interruptChildren) == Exit.Failure(die(ExampleError)))

  def testEvalOfDeepUncaughtThrownSyncEffect() =
    assert(unsafeRunSync(deepErrorEffect(100)) == Exit.Failure(fail(ExampleError)))

  def testEvalOfDeepUncaughtFail() =
    assert(unsafeRunSync(deepErrorEffect(100)) == Exit.Failure(fail(ExampleError)))

  def testFailOfMultipleFailingFinalizers() =
    assert(
      unsafeRun(
        TaskExampleError
          .ensuring(IO.effectTotal(throw InterruptCause1))
          .ensuring(IO.effectTotal(throw InterruptCause2))
          .ensuring(IO.effectTotal(throw InterruptCause3))
          .run
      ) == Exit.halt(
        fail(ExampleError) ++
          die(InterruptCause1) ++
          die(InterruptCause2) ++
          die(InterruptCause3)
      )
    )

  def testTerminateOfMultipleFailingFinalizers() =
    assert(
      unsafeRun(
        IO.die(ExampleError)
          .ensuring(IO.effectTotal(throw InterruptCause1))
          .ensuring(IO.effectTotal(throw InterruptCause2))
          .ensuring(IO.effectTotal(throw InterruptCause3))
          .run
      ) == Exit.halt(
        die(ExampleError) ++
          die(InterruptCause1) ++
          die(InterruptCause2) ++
          die(InterruptCause3)
      )
    )

  def testEvalOfFailEnsuring() = {
    var finalized = false

    unsafeRunSync((Task.fail(ExampleError): Task[Unit]).ensuring(IO.effectTotal[Unit] { finalized = true; () })) must_===
      Exit.Failure(fail(ExampleError))
    assert(finalized)
  }

  def testEvalOfFailOnError() = {
    @volatile var finalized = false
    val cleanup: Cause[Throwable] => UIO[Unit] =
      _ => IO.effectTotal[Unit] { finalized = true; () }
    assert(
      unsafeRunSync(
        Task.fail(ExampleError).onError(cleanup): Task[Unit]
      ) == Exit.Failure(fail(ExampleError))
    )

    // FIXME: Is this an issue with thread synchronization?
    while (!finalized) Thread.`yield`()

    assert(finalized)
  }

  def testErrorInFinalizerCannotBeCaught() = {

    val e2 = new Error("e2")
    val e3 = new Error("e3")

    val nested: Task[Int] =
      TaskExampleError
        .ensuring(IO.die(e2))
        .ensuring(IO.die(e3))

    assert(unsafeRunSync(nested) == Exit.Failure(Then(fail(ExampleError), Then(die(e2), die(e3)))))
  }

  def testLiftingOptionalValue() = assert(unsafeRun(ZIO.some(42)) == Some(42))

  def testLiftingNoneValue() = assert(unsafeRun(ZIO.none) == None)

  def liftValueIntoRight() = assert(unsafeRun(ZIO.right(42)) == Right(42))

  def liftValueIntoLeft() = assert(unsafeRun(ZIO.left(42)) == Left(42))

  def testErrorInFinalizerIsReported() = {
    @volatile var reported: Exit[Nothing, Int] = null

    unsafeRun {
      IO.succeedLazy[Int](42)
        .ensuring(IO.die(ExampleError))
        .fork
        .flatMap(_.await.flatMap[Any, Nothing, Any](e => UIO.effectTotal { reported = e }))
    }

    reported must_=== Exit.Failure(die(ExampleError))
  }

  def testExitIsUsageResult() =
    unsafeRun(IO.bracket(IO.unit)(_ => IO.unit)(_ => IO.succeedLazy[Int](42))) must_=== 42

  def testBracketErrorInAcquisition() =
    unsafeRunSync(IO.bracket(TaskExampleError)(_ => IO.unit)(_ => IO.unit)) must_=== Exit.Failure(fail(ExampleError))

  def testBracketErrorInRelease() =
    unsafeRunSync(IO.bracket(IO.unit)(_ => IO.die(ExampleError))(_ => IO.unit)) must_=== Exit.Failure(die(ExampleError))

  def testBracketErrorInUsage() =
    unsafeRunSync(Task.bracket(Task.unit)(_ => Task.unit)(_ => Task.fail(ExampleError): Task[Unit])) must_=== Exit
      .Failure(fail(ExampleError))

  def testBracketRethrownCaughtErrorInAcquisition() = {
    val io = IO.absolve(IO.bracket(TaskExampleError)(_ => IO.unit)(_ => IO.unit).either)

    unsafeRunSync(io) must_=== Exit.Failure(fail(ExampleError))
  }

  def testBracketRethrownCaughtErrorInRelease() = {
    val io = IO.bracket(IO.unit)(_ => IO.die(ExampleError))(_ => IO.unit)

    unsafeRunSync(io) must_=== Exit.Failure(die(ExampleError))
  }

  def testBracketRethrownCaughtErrorInUsage() = {
    val io =
      IO.absolve(
        IO.unit
          .bracket_[Any, Nothing]
          .apply[Any](IO.unit)(TaskExampleError)
          .either //    TODO: Dotty doesn't infer this properly
      )

    unsafeRunSync(io) must_=== Exit.Failure(fail(ExampleError))
  }

  def testEvalOfAsyncAttemptOfFail() = {
    val io1 = IO.unit.bracket_[Any, Nothing].apply[Any](AsyncUnit[Nothing])(asyncExampleError[Unit]) //    TODO: Dotty doesn't infer this properly
    val io2 = AsyncUnit[Throwable].bracket_[Any, Throwable].apply[Any](IO.unit)(asyncExampleError[Unit])

    unsafeRunSync(io1) must_=== Exit.Failure(fail(ExampleError))
    unsafeRunSync(io2) must_=== Exit.Failure(fail(ExampleError))
    unsafeRunSync(IO.absolve(io1.either)) must_=== Exit.Failure(fail(ExampleError))
    unsafeRunSync(IO.absolve(io2.either)) must_=== Exit.Failure(fail(ExampleError))
  }

  def testBracketRegression1() = {
    def makeLogger: Ref[List[String]] => String => UIO[Unit] =
      (ref: Ref[List[String]]) => (line: String) => ref.update(_ ::: List(line)).unit

    unsafeRun(for {
      ref <- Ref.make[List[String]](Nil)
      log = makeLogger(ref)
      f <- ZIO
            .bracket(
              ZIO.bracket(ZIO.unit)(_ => log("start 1") *> clock.sleep(10.millis) *> log("release 1"))(
                _ => ZIO.unit
              )
            )(_ => log("start 2") *> clock.sleep(10.millis) *> log("release 2"))(_ => ZIO.unit)
            .fork
      _ <- (ref.get <* clock.sleep(1.millis)).repeat(ZSchedule.doUntil[List[String]](_.contains("start 1")))
      _ <- f.interrupt
      _ <- (ref.get <* clock.sleep(1.millis)).repeat(ZSchedule.doUntil[List[String]](_.contains("release 2")))
      l <- ref.get
    } yield l) must_=== ("start 1" :: "release 1" :: "start 2" :: "release 2" :: Nil)
  }

  def testInterruptWaitsForFinalizer() =
    unsafeRun(for {
      r  <- Ref.make(false)
      p1 <- Promise.make[Nothing, Unit]
      p2 <- Promise.make[Nothing, Int]
      s <- (p1.succeed(()) *> p2.await)
            .ensuring(r.set(true) *> clock.sleep(10.millis))
            .fork
      _    <- p1.await
      _    <- s.interrupt
      test <- r.get
    } yield test must_=== true)

  def testRunInterruptIsInterrupted() =
    unsafeRun(for {
      p    <- Promise.make[Nothing, Unit]
      f    <- (p.succeed(()) *> IO.never).run.fork
      _    <- p.await
      _    <- f.interrupt
      test <- f.await.map(_.interrupted)
    } yield test) must_=== true

  def testRunSwallowsInnerInterrupt() =
    unsafeRun(for {
      p   <- Promise.make[Nothing, Int]
      _   <- IO.interrupt.run *> p.succeed(42)
      res <- p.await
    } yield res) must_=== 42

  //TODO: Figure out how to do timeout
  def testTimeoutOfLongComputation() = assert(true)
  /*aroundTimeout(10.milliseconds.asScala)(ee)
      .around(
        unsafeRun(
          clock.sleep(60.seconds) *> UIO(true)
        )
      )
      .message must_== "TIMEOUT: 10000000 nanoseconds"
   */
  def testCatchAllCause() =
    unsafeRun((for {
      _ <- ZIO succeed 42
      f <- ZIO fail "Uh oh!"
    } yield f) catchAllCause ZIO.succeed) must_=== Fail("Uh oh!")

  def testEvalOfDeepSyncEffect() = {
    def incLeft(n: Int, ref: Ref[Int]): Task[Int] =
      if (n <= 0) ref.get
      else incLeft(n - 1, ref) <* ref.update(_ + 1)

    def incRight(n: Int, ref: Ref[Int]): Task[Int] =
      if (n <= 0) ref.get
      else ref.update(_ + 1) *> incRight(n - 1, ref)

    val l = unsafeRun(for {
      ref <- Ref.make(0)
      v   <- incLeft(100, ref)
    } yield v)

    val r = unsafeRun(for {
      ref <- Ref.make(0)
      v   <- incRight(1000, ref)
    } yield v)

    l must_=== 0
    r must_=== 1000
  }

  def testDeepMapOfPoint() =
    unsafeRun(deepMapPoint(10000)) must_=== 10000

  def testDeepMapOfNow() =
    unsafeRun(deepMapNow(10000)) must_=== 10000

  def testDeepMapOfSyncEffectIsStackSafe() =
    unsafeRun(deepMapEffect(10000)) must_=== 10000

  def testDeepAttemptIsStackSafe() =
    unsafeRun((0 until 10000).foldLeft(IO.effect[Unit](())) { (acc, _) =>
      acc.either.unit
    }) must_=== (())

  def testDeepFlatMapIsStackSafe() = {
    def fib(n: Int, a: BigInt = 0, b: BigInt = 1): IO[Error, BigInt] =
      IO.succeed(a + b).flatMap { b2 =>
        if (n > 0)
          fib(n - 1, b, b2)
        else
          IO.succeed(b2)
      }

    val future = fib(1000)
    unsafeRun(future) must_=== BigInt(
      "113796925398360272257523782552224175572745930353730513145086634176691092536145985470146129334641866902783673042322088625863396052888690096969577173696370562180400527049497109023054114771394568040040412172632376"
    )
  }

  def testDeepAbsolveAttemptIsIdentity() =
    unsafeRun((0 until 1000).foldLeft(IO.succeedLazy[Int](42))((acc, _) => IO.absolve(acc.either))) must_=== 42

  def testDeepAsyncAbsolveAttemptIsIdentity() =
    unsafeRun(
      (0 until 1000)
        .foldLeft(IO.effectAsync[Int, Int](k => k(IO.succeed(42))))((acc, _) => IO.absolve(acc.either))
    ) must_=== 42

  def testAsyncEffectReturns() =
    unsafeRun(IO.effectAsync[Throwable, Int](k => k(IO.succeed(42)))) must_=== 42

  def testAsyncIOEffectReturns() =
    unsafeRun(IO.effectAsyncM[Throwable, Int](k => IO.effectTotal(k(IO.succeed(42))))) must_=== 42

  def testDeepAsyncIOThreadStarvation() = {
    def stackIOs(clock: Clock.Service[Any], count: Int): UIO[Int] =
      if (count <= 0) IO.succeed(42)
      else asyncIO(clock, stackIOs(clock, count - 1))

    def asyncIO(clock: Clock.Service[Any], cont: UIO[Int]): UIO[Int] =
      IO.effectAsyncM[Nothing, Int] { k =>
        clock.sleep(5.millis) *> cont *> IO.effectTotal(k(IO.succeed(42)))
      }

    val procNum = java.lang.Runtime.getRuntime.availableProcessors()

    unsafeRun(clock.clockService.flatMap(stackIOs(_, procNum + 1))) must_=== 42
  }

  def testAsyncPureInterruptRegister() =
    unsafeRun(for {
      release <- Promise.make[Nothing, Unit]
      acquire <- Promise.make[Nothing, Unit]
      fiber <- IO
                .effectAsyncM[Nothing, Unit] { _ =>
                  IO.bracket(acquire.succeed(()))(_ => release.succeed(()))(_ => IO.never)
                }
                .fork
      _ <- acquire.await
      _ <- fiber.interrupt.fork
      a <- release.await
    } yield a) must_=== (())

  def testEffectAsyncMCanFail() =
    unsafeRun {
      ZIO
        .effectAsyncM[Any, String, Nothing](_ => ZIO.fail("Ouch"))
        .flip
        .map(_ must_=== "Ouch")
    }

  def testSleepZeroReturns() =
    unsafeRun(clock.sleep(1.nanos)) must_=== ((): Unit)

  def testShallowBindOfAsyncChainIsCorrect() = {
    val result = (0 until 10).foldLeft[Task[Int]](IO.succeedLazy[Int](0)) { (acc, _) =>
      acc.flatMap(n => IO.effectAsync[Throwable, Int](_(IO.succeed(n + 1))))
    }

    unsafeRun(result) must_=== 10
  }

  def testForkJoinIsId() =
    unsafeRun(IO.succeedLazy[Int](42).fork.flatMap(_.join)) must_=== 42

  def testDeepForkJoinIsId() = {
    val n = 20

    unsafeRun(concurrentFib(n)) must_=== fib(n)
  }

  def testNeverIsInterruptible() = {
    val io =
      for {
        fiber <- IO.never.fork
        _     <- fiber.interrupt
      } yield 42

    unsafeRun(io) must_=== 42
  }

  def testBracketAcquireIsUninterruptible() = {
    val io =
      for {
        promise <- Promise.make[Nothing, Unit]
        fiber   <- IO.bracket(promise.succeed(()) <* IO.never)(_ => IO.unit)(_ => IO.unit).fork
        res     <- promise.await *> fiber.interrupt.timeoutTo(42)(_ => 0)(1.second)
      } yield res
    unsafeRun(io) must_=== 42
  }

  def testBracket0AcquireIsUninterruptible() = {
    val io =
      for {
        promise <- Promise.make[Nothing, Unit]
        fiber <- IO
                  .bracketExit(promise.succeed(()) *> IO.never *> IO.succeed(1))((_, _: Exit[_, _]) => IO.unit)(
                    _ => IO.unit: IO[Nothing, Unit]
                  )
                  .fork
        res <- promise.await *> fiber.interrupt.timeoutTo(42)(_ => 0)(1.second)
      } yield res
    unsafeRun(io) must_=== 42
  }

  def testBracketReleaseOnInterrupt() = {
    val io =
      for {
        p1    <- Promise.make[Nothing, Unit]
        p2    <- Promise.make[Nothing, Unit]
        fiber <- IO.bracket(IO.unit)(_ => p2.succeed(()) *> IO.unit)(_ => p1.succeed(()) *> IO.never).fork
        _     <- p1.await
        _     <- fiber.interrupt
        _     <- p2.await
      } yield ()

    unsafeRun(io.timeoutTo(42)(_ => 0)(1.second)) must_=== 0
  }

  def testBracket0ReleaseOnInterrupt() =
    unsafeRun(for {
      done <- Promise.make[Nothing, Unit]
      fiber <- withLatch { release =>
                IO.bracketExit(IO.unit)((_, _: Exit[_, _]) => done.succeed(()))(
                    _ => release *> IO.never
                  )
                  .fork
              }

      _ <- fiber.interrupt
      r <- done.await.timeoutTo(42)(_ => 0)(60.second)
    } yield r must_=== 0)

  def testRedeemEnsuringInterrupt() = {
    val io = for {
      cont <- Promise.make[Nothing, Unit]
      p1   <- Promise.make[Nothing, Boolean]
      f1   <- (cont.succeed(()) *> IO.never).catchAll(IO.fail).ensuring(p1.succeed(true)).fork
      _    <- cont.await
      _    <- f1.interrupt
      res  <- p1.await
    } yield res

    unsafeRun(io) must_=== true
  }

  def testFinalizerCanDetectInterruption() = {
    val io = for {
      p1  <- Promise.make[Nothing, Boolean]
      c   <- Promise.make[Nothing, Unit]
      f1  <- (c.succeed(()) *> IO.never).ensuring(IO.descriptor.flatMap(d => p1.succeed(d.interrupted))).fork
      _   <- c.await
      _   <- f1.interrupt
      res <- p1.await
    } yield res

    unsafeRun(io) must_=== true
  }

  def testInterruptedOfRaceInterruptsContestents() = {
    val io = for {
      ref   <- Ref.make(0)
      cont1 <- Promise.make[Nothing, Unit]
      cont2 <- Promise.make[Nothing, Unit]
      make  = (p: Promise[Nothing, Unit]) => (p.succeed(()) *> IO.never).onInterrupt(ref.update(_ + 1))
      raced <- (make(cont1) race (make(cont2))).fork
      _     <- cont1.await *> cont2.await
      _     <- raced.interrupt
      count <- ref.get
    } yield count

    unsafeRun(io) must_=== 2
  }

  def testCancelationIsGuaranteed() = {
    val io = for {
      release <- zio.Promise.make[Nothing, Int]
      latch   = internal.OneShot.make[Unit]
      async = IO.effectAsyncInterrupt[Nothing, Unit] { _ =>
        latch.set(()); Left(release.succeed(42).unit)
      }
      fiber  <- async.fork
      _      <- IO.effectTotal(latch.get(1000))
      _      <- fiber.interrupt.fork
      result <- release.await
    } yield result

    nonFlaky(io.map(_ must_=== 42))
  }

  def testInterruptionOfUnendingBracket() = {
    val io = for {
      startLatch <- Promise.make[Nothing, Int]
      exitLatch  <- Promise.make[Nothing, Int]
      bracketed = IO
        .succeed(21)
        .bracketExit[Any, Error, Int]( // TODO: Dotty doesn't infer curried version
          (r: Int, exit: Exit[Error, Int]) =>
            if (exit.interrupted) exitLatch.succeed(r)
            else IO.die(new Error("Unexpected case")),
          (a: Int) => startLatch.succeed(a) *> IO.never *> IO.succeed(1)
        )
      fiber      <- bracketed.fork
      startValue <- startLatch.await
      _          <- fiber.interrupt.fork
      exitValue  <- exitLatch.await
    } yield startValue + exitValue

    nonFlaky(io.map(_ must_=== 42))
  }

  def testRecoveryOfErrorInFinalizer() =
    unsafeRun(for {
      recovered <- Ref.make(false)
      fiber <- withLatch { release =>
                (release *> ZIO.never)
                  .ensuring(
                    (ZIO.unit *> ZIO.fail("Uh oh")).catchAll(_ => recovered.set(true))
                  )
                  .fork
              }
      _     <- fiber.interrupt
      value <- recovered.get
    } yield value must_=== true)

  def testRecoveryOfInterruptible() =
    unsafeRun(for {
      recovered <- Ref.make(false)
      fiber <- withLatch { release =>
                (release *> ZIO.never.interruptible)
                  .foldCauseM(
                    cause => recovered.set(cause.interrupted),
                    _ => recovered.set(false)
                  )
                  .uninterruptible
                  .fork
              }
      _     <- fiber.interrupt
      value <- recovered.get
    } yield value must_=== true)

  def testSandboxOfInterruptible() =
    unsafeRun(for {
      recovered <- Ref.make[Option[Either[Cause[Nothing], Any]]](None)
      fiber <- withLatch { release =>
                (release *> ZIO.never.interruptible).sandbox.either
                  .flatMap(exit => recovered.set(Some(exit)))
                  .uninterruptible
                  .fork
              }
      _     <- fiber.interrupt
      value <- recovered.get
    } yield value must_=== Some(Left(Cause.interrupt)))

  def testRunOfInterruptible() =
    unsafeRun(for {
      recovered <- Ref.make[Option[Exit[Nothing, Any]]](None)
      fiber <- withLatch { release =>
                (release *> ZIO.never.interruptible).run
                  .flatMap(exit => recovered.set(Some(exit)))
                  .uninterruptible
                  .fork
              }
      _     <- fiber.interrupt
      value <- recovered.get
    } yield value must_=== Some(Exit.Failure(Cause.interrupt)))

  def testAlternatingInterruptibility() =
    unsafeRun(for {
      counter <- Ref.make(0)
      fiber <- withLatch { release =>
                ((((release *> ZIO.never.interruptible.run *> counter
                  .update(_ + 1)).uninterruptible).interruptible).run
                  *> counter.update(_ + 1)).uninterruptible.fork
              }
      _     <- fiber.interrupt
      value <- counter.get
    } yield value must_=== 2)

  def testInterruptionAfterDefect() =
    unsafeRun(for {
      ref <- Ref.make(false)
      fiber <- withLatch { release =>
                (ZIO.succeedLazy(throw new Error).run *> release *> ZIO.never)
                  .ensuring(ref.set(true))
                  .fork
              }
      _     <- fiber.interrupt
      value <- ref.get
    } yield value must_=== true)

  def testInterruptionAfterDefect2() =
    unsafeRun(for {
      ref <- Ref.make(false)
      fiber <- withLatch { release =>
                (ZIO.succeedLazy(throw new Error).run *> release *> ZIO.unit.forever)
                  .ensuring(ref.set(true))
                  .fork
              }
      _     <- fiber.interrupt
      value <- ref.get
    } yield value must_=== true)

  def testCauseReflectsInterruption() =
    nonFlaky {
      for {
        finished <- Ref.make(false)
        fiber <- withLatch { release =>
                  (release *> ZIO.fail("foo")).catchAll(_ => finished.set(true)).fork
                }
        exit     <- fiber.interrupt
        finished <- finished.get
      } yield {
        val res1 = exit.interrupted
        val res2 = finished

        assert(res1 || res2)
      }
    }

  def testAsyncCanBeUninterruptible() =
    unsafeRun(for {
      ref <- Ref.make(false)
      fiber <- withLatch { release =>
                (release *> clock.sleep(10.millis) *> ref.set(true).unit).uninterruptible.fork
              }
      _     <- fiber.interrupt
      value <- ref.get
    } yield value must_=== true)

  def testUseInheritsInterruptStatus() =
    unsafeRun(
      for {
        ref <- Ref.make(false)
        fiber1 <- withLatch { (release2, await2) =>
                   withLatch { release1 =>
                     release1
                       .bracket_(ZIO.unit, await2 *> clock.sleep(10.millis) *> ref.set(true))
                       .uninterruptible
                       .fork
                   } <* release2
                 }
        _     <- fiber1.interrupt
        value <- ref.get
      } yield value must_=== true
    )

  def testCauseUseInheritsInterruptStatus() =
    unsafeRun(
      for {
        latch1 <- Promise.make[Nothing, Unit]
        latch2 <- Promise.make[Nothing, Unit]
        ref    <- Ref.make(false)
        fiber1 <- latch1
                   .succeed(())
                   .bracketExit[Clock, Nothing, Unit](
                     (_: Boolean, _: Exit[_, _]) => ZIO.unit,
                     (_: Boolean) => latch2.await *> clock.sleep(10.millis) *> ref.set(true).unit
                   )
                   .uninterruptible
                   .fork
        _     <- latch1.await
        _     <- latch2.succeed(())
        _     <- fiber1.interrupt
        value <- ref.get
      } yield value must_=== true
    )

  def testProvideIsModular() = {
    val zio =
      (for {
        v1 <- ZIO.environment[Int]
        v2 <- ZIO.environment[Int].provide(2)
        v3 <- ZIO.environment[Int]
      } yield (v1, v2, v3)).provide(4)
    unsafeRun(zio) must_=== ((4, 2, 4))
  }

  def testProvideManagedIsModular() = {
    def managed(v: Int): ZManaged[Any, Nothing, Int] =
      ZManaged.make(IO.succeed(v))(_ => IO.effectTotal { () })
    val zio = (for {
      v1 <- ZIO.environment[Int]
      v2 <- ZIO.environment[Int].provideManaged(managed(2))
      v3 <- ZIO.environment[Int]
    } yield (v1, v2, v3)).provideManaged(managed(4))

    unsafeRun(zio) must_=== ((4, 2, 4))
  }

  def testAsyncCanUseEnvironment() = unsafeRun {
    for {
      result <- ZIO
                 .effectAsync[Int, Nothing, Int] { cb =>
                   cb(ZIO.environment[Int])
                 }
                 .provide(10)
    } yield result must_=== 10
  }

  def testInterruptStatusIsHeritable() = nonFlaky {
    for {
      latch <- Promise.make[Nothing, Unit]
      ref   <- Ref.make(InterruptStatus.interruptible)
      _     <- ZIO.uninterruptible((ZIO.checkInterruptible(ref.set) *> latch.succeed(())).fork *> latch.await)
      v     <- ref.get
    } yield v must_=== InterruptStatus.uninterruptible
  }

  def testExecutorIsHeritable() =
    nonFlaky(for {
      ref  <- Ref.make(Option.empty[internal.Executor])
      exec = internal.Executor.fromExecutionContext(100)(scala.concurrent.ExecutionContext.Implicits.global)
      _    <- withLatch(release => IO.descriptor.map(_.executor).flatMap(e => ref.set(Some(e)) *> release).fork.lock(exec))
      v    <- ref.get
    } yield v must_=== Some(exec))

  def testSupervisionIsHeritable() = nonFlaky {
    for {
      latch <- Promise.make[Nothing, Unit]
      ref   <- Ref.make(SuperviseStatus.unsupervised)
      _     <- ((ZIO.checkSupervised(ref.set) *> latch.succeed(())).fork *> latch.await).supervised
      v     <- ref.get
    } yield v must_=== SuperviseStatus.Supervised
  }

  def testSupervisingInheritance() = {
    def forkAwaitStart[A](io: UIO[A], refs: Ref[List[Fiber[_, _]]]): UIO[Fiber[Nothing, A]] =
      withLatch(release => (release *> io).fork.tap(f => refs.update(f :: _)))

    nonFlaky(
      (for {
        ref  <- Ref.make[List[Fiber[_, _]]](Nil) // To make strong ref
        _    <- forkAwaitStart(forkAwaitStart(forkAwaitStart(IO.succeed(()), ref), ref), ref)
        fibs <- ZIO.children
        _    <- ref.get.map(list => println(list.mkString(", ")))
      } yield assert(fibs.size == 1)).supervised
    )
  }

  def testAsyncPureIsInterruptible() = {
    val io =
      for {
        fiber <- IO.effectAsyncM[Nothing, Nothing](_ => IO.never).fork
        _     <- fiber.interrupt
      } yield 42

    unsafeRun(io) must_=== 42
  }

  def testAsyncIsInterruptible() = {
    val io =
      for {
        fiber <- IO.effectAsync[Nothing, Nothing](_ => ()).fork
        _     <- fiber.interrupt
      } yield 42

    unsafeRun(io) must_=== 42
  }

  def testAsyncPureCreationIsInterruptible() = {
    val io = for {
      release <- Promise.make[Nothing, Int]
      acquire <- Promise.make[Nothing, Unit]
      task = IO.effectAsyncM[Nothing, Unit] { _ =>
        IO.bracket(acquire.succeed(()))(_ => release.succeed(42).unit)(_ => IO.never)
      }
      fiber <- task.fork
      _     <- acquire.await
      _     <- fiber.interrupt
      a     <- release.await
    } yield a

    unsafeRun(io) must_=== 42
  }

  def testAsync0RunsCancelTokenOnInterrupt() = {
    val io = for {
      release <- Promise.make[Nothing, Int]
      latch   = scala.concurrent.Promise[Unit]()
      async = IO.effectAsyncInterrupt[Nothing, Nothing] { _ =>
        latch.success(()); Left(release.succeed(42).unit)
      }
      fiber <- async.fork
      _ <- IO.effectAsync[Throwable, Unit] { k =>
            latch.future.onComplete {
              case Success(a) => k(IO.succeed(a))
              case Failure(t) => k(IO.fail(t))
            }(scala.concurrent.ExecutionContext.global)
          }
      _      <- fiber.interrupt
      result <- release.await
    } yield result

    unsafeRun(io) must_=== 42
  }

  def testBracketUseIsInterruptible() = {
    val io =
      for {
        fiber <- IO.bracket(IO.unit)(_ => IO.unit)(_ => IO.never).fork
        res   <- fiber.interrupt
      } yield res
    unsafeRun(io) must_=== Exit.interrupt
  }

  def testBracket0UseIsInterruptible() = {
    val io =
      for {
        fiber <- IO.bracketExit(IO.unit)((_, _: Exit[_, _]) => IO.unit)(_ => IO.never).fork
        res   <- fiber.interrupt.timeoutTo(42)(_ => 0)(1.second)
      } yield res
    unsafeRun(io) must_=== 0
  }

  def testSupervising() = {
    def forkAwaitStart(ref: Ref[List[Fiber[_, _]]]) =
      withLatch(release => (release *> UIO.never).fork.tap(fiber => ref.update(fiber :: _)))

    unsafeRun(
      (for {
        ref   <- Ref.make(List.empty[Fiber[_, _]])
        fibs0 <- ZIO.children
        _     <- forkAwaitStart(ref)
        fibs1 <- ZIO.children
        _     <- forkAwaitStart(ref)
        fibs2 <- ZIO.children
      } yield {
        assert(fibs0.size == 0)
        assert(fibs1.size == 1)
        assert(fibs2.size == 2)
      }).supervised
    )
  }

  def testSupervisingUnsupervised() =
    unsafeRun(
      for {
        ref  <- Ref.make(Option.empty[Fiber[_, _]])
        _    <- withLatch(release => (release *> UIO.never).fork.tap(fiber => ref.set(Some(fiber))))
        fibs <- ZIO.children
      } yield assert(fibs.size == 0)
    )

  def testSupervise() = {
    var counter = 0
    unsafeRun((for {
      ref <- Ref.make(List.empty[Fiber[_, _]])
      _   <- (clock.sleep(200.millis) *> IO.unit).fork.tap(fiber => ref.update(fiber :: _))
      _   <- (clock.sleep(400.millis) *> IO.unit).fork.tap(fiber => ref.update(fiber :: _))
    } yield ()).handleChildrenWith { fs =>
      fs.foldLeft(IO.unit)((io, f) => io *> f.join.either *> IO.effectTotal(counter += 1))
    })
    counter must_=== 2
  }

  def testSuperviseRace() =
    unsafeRun(for {
      pa <- Promise.make[Nothing, Int]
      pb <- Promise.make[Nothing, Int]

      p1 <- Promise.make[Nothing, Unit]
      p2 <- Promise.make[Nothing, Unit]
      f <- (
            p1.succeed(())
              .bracket_[Any, Nothing]
              .apply[Any](pa.succeed(1).unit)(IO.never) race //    TODO: Dotty doesn't infer this properly
              p2.succeed(()).bracket_[Any, Nothing].apply[Any](pb.succeed(2).unit)(IO.never)
          ).interruptChildren.fork
      _ <- p1.await *> p2.await

      _ <- f.interrupt
      r <- pa.await zip pb.await
    } yield r) must_=== (1 -> 2)

  def testSuperviseFork() =
    unsafeRun(for {
      pa <- Promise.make[Nothing, Int]
      pb <- Promise.make[Nothing, Int]

      p1 <- Promise.make[Nothing, Unit]
      p2 <- Promise.make[Nothing, Unit]
      f <- (
            p1.succeed(())
              .bracket_[Any, Nothing]
              .apply[Any](pa.succeed(1).unit)(IO.never)
              .fork *> //    TODO: Dotty doesn't infer this properly
              p2.succeed(()).bracket_[Any, Nothing].apply[Any](pb.succeed(2).unit)(IO.never).fork *>
              IO.never
          ).interruptChildren.fork
      _ <- p1.await *> p2.await

      _ <- f.interrupt
      r <- pa.await zip pb.await
    } yield r) must_=== (1 -> 2)

  def testSupervised() =
    nonFlaky {
      for {
        pa <- Promise.make[Nothing, Int]
        pb <- Promise.make[Nothing, Int]
        _ <- (for {
              p1 <- Promise.make[Nothing, Unit]
              p2 <- Promise.make[Nothing, Unit]
              _ <- p1
                    .succeed(())
                    .bracket_[Any, Nothing]
                    .apply[Any](pa.succeed(1).unit)(IO.never)
                    .fork //    TODO: Dotty doesn't infer this properly
              _ <- p2.succeed(()).bracket_[Any, Nothing].apply[Any](pb.succeed(2).unit)(IO.never).fork
              _ <- p1.await *> p2.await
            } yield ()).interruptChildren
        r <- pa.await zip pb.await
      } yield r must_=== (1 -> 2)
    }

  def testRaceChoosesWinner() =
    unsafeRun(IO.fail(42).race(IO.succeed(24)).either) must_=== Right(24)

  def testRaceChoosesWinnerInTerminate() =
    unsafeRun(IO.die(new Throwable {}).race(IO.succeed(24)).either) must_=== Right(24)

  def testRaceChoosesFailure() =
    unsafeRun(IO.fail(42).race(IO.fail(42)).either) must_=== Left(42)

  def testRaceOfValueNever() =
    unsafeRun(IO.succeedLazy(42).race(IO.never)) must_=== 42

  def testRaceOfFailNever() =
    unsafeRun(IO.fail(24).race(IO.never).timeout(10.milliseconds)) must_=== None

  def testRaceAllOfValues() =
    unsafeRun(IO.raceAll(IO.fail(42), List(IO.succeed(24))).either) must_=== Right(24)

  def testRaceAllOfFailures() =
    unsafeRun(ZIO.raceAll(IO.fail(24).delay(10.millis), List(IO.fail(24))).either) must_=== Left(24)

  def testRaceAllOfFailuresOneSuccess() =
    unsafeRun(ZIO.raceAll(IO.fail(42), List(IO.succeed(24).delay(1.millis))).either) must_=== Right(
      24
    )

  def testRaceBothInterruptsLoser() =
    unsafeRun(for {
      s      <- Semaphore.make(0L)
      effect <- Promise.make[Nothing, Int]
      winner = s.acquire *> IO.effectAsync[Throwable, Unit](_(IO.unit))
      loser  = IO.bracket(s.release)(_ => effect.succeed(42).unit)(_ => IO.never)
      race   = winner raceEither loser
      _      <- race.either
      b      <- effect.await
    } yield b) must_=== 42

  def testFirstSuccessOfValues() =
    unsafeRun(IO.firstSuccessOf(IO.fail(0), List(IO.succeed(100))).either) must_=== Right(100)

  def testFirstSuccessOfFailures() =
    unsafeRun(ZIO.firstSuccessOf(IO.fail(0).delay(10.millis), List(IO.fail(101))).either) must_=== Left(101)

  def testFirstSuccessOfFailuresOneSuccess() =
    unsafeRun(ZIO.firstSuccessOf(IO.fail(0), List(IO.succeed(102).delay(1.millis))).either) must_=== Right(
      102
    )

  def testRepeatedPar() = {
    def countdown(n: Int): UIO[Int] =
      if (n == 0) IO.succeed(0)
      else IO.succeed[Int](1).zipPar(IO.succeed[Int](2)).flatMap(t => countdown(n - 1).map(y => t._1 + t._2 + y))

    unsafeRun(countdown(50)) must_=== 150
  }

  def testRaceAttemptInterruptsLoserOnSuccess() =
    unsafeRun(for {
      s      <- Promise.make[Nothing, Unit]
      effect <- Promise.make[Nothing, Int]
      winner = s.await *> IO.fromEither(Right(()))
      loser  = IO.bracket(s.succeed(()))(_ => effect.succeed(42))(_ => IO.never)
      race   = winner raceAttempt loser
      _      <- race.either
      b      <- effect.await
    } yield b) must_=== 42

  def testRaceAttemptInterruptsLoserOnFailure() =
    unsafeRun(for {
      s      <- Promise.make[Nothing, Unit]
      effect <- Promise.make[Nothing, Int]
      winner = s.await *> IO.fromEither(Left(new Exception))
      loser  = IO.bracket(s.succeed(()))(_ => effect.succeed(42))(_ => IO.never)
      race   = winner raceAttempt loser
      _      <- race.either
      b      <- effect.await
    } yield b) must_=== 42

  def testPar() =
    nonFlaky {
      IO.succeed[Int](1).zipPar(IO.succeed[Int](2)).flatMap(t => IO.succeed(t._1 + t._2)).map(_ must_=== 3)
    }

  def testReduceAll() =
    unsafeRun(
      IO.reduceAll(IO.succeedLazy(1), List(2, 3, 4).map(IO.succeedLazy[Int](_)))(_ + _)
    ) must_=== 10

  def testReduceAllEmpty() =
    unsafeRun(
      IO.reduceAll(IO.succeedLazy(1), Seq.empty)(_ + _)
    ) must_=== 1

  def testTimeoutFailure() =
    intercept[FiberFailure](
      unsafeRun(
        IO.fail("Uh oh").timeout(1.hour) *> IO.unit
      )
    )

  def testTimeoutTerminate() =
    unsafeRunSync(
      IO.die(ExampleError).timeout(1.hour): ZIO[Clock, Nothing, Option[Int]]
    ) must_=== Exit.die(ExampleError)

  def testDeadlockRegression() = {

    import java.util.concurrent.Executors

    val rts = new DefaultRuntime {}

    val e = Executors.newSingleThreadExecutor()

    (0 until 10000).foreach { _ =>
      rts.unsafeRun {
        IO.effectAsync[Nothing, Int] { k =>
          val c: Callable[Unit] = () => k(IO.succeed(1))
          val _                 = e.submit(c)
        }
      }
    }

    e.shutdown() must_=== (())
  }

  def testInterruptionRegression1() = {

    val c = new AtomicInteger(0)

    val test =
      IO.effect {
        if (c.incrementAndGet() <= 1) throw new RuntimeException("x")
      }.forever
        .ensuring(IO.unit)
        .either
        .forever

    unsafeRun(
      for {
        f <- test.fork
        c <- (IO.effectTotal[Int](c.get) <* clock.sleep(1.millis)).repeat(ZSchedule.doUntil[Int](_ >= 1)) <* f.interrupt
      } yield assert(c >= 1)
    )

  }

  def testManualSyncInterruption() = {
    def sync[A](effect: => A): IO[Throwable, A] =
      IO.effectTotal(effect)
        .foldCauseM({
          case Cause.Die(t) => IO.fail(t)
          case cause        => IO.halt(cause)
        }, IO.succeed(_))

    def putStr(text: String): IO[Throwable, Unit] =
      sync(println(text))

    unsafeRun(
      for {
        fiber <- putStr(".").forever.fork
        _     <- fiber.interrupt
      } yield true
    )
  }

  def testBlockingThreadCaching() = {
    import zio.blocking.Blocking

    def runAndTrack(ref: Ref[Set[Thread]]): ZIO[Blocking with Clock, Nothing, Boolean] =
      blocking.blocking {
        UIO(Thread.currentThread()).flatMap(thread => ref.modify(set => (set.contains(thread), set + thread))) <* ZIO
          .sleep(1.millis)
      }

    unsafeRun(for {
      accum <- Ref.make(Set.empty[Thread])
      b     <- runAndTrack(accum).repeat(Schedule.doUntil[Boolean](_ == true))
    } yield b must_=== true)
  }

  def testBlockingIOIsEffectBlocking() = unsafeRun(
    for {
      done  <- Ref.make(false)
      start <- IO.succeed(internal.OneShot.make[Unit])
      fiber <- blocking.effectBlocking { start.set(()); Thread.sleep(60L * 60L * 1000L) }.ensuring(done.set(true)).fork
      _     <- IO.succeed(start.get())
      res   <- fiber.interrupt
      value <- done.get
    } yield (res, value) must_=== ((Exit.interrupt, true))
  )

  def testInterruptSyncForever() = unsafeRun(
    for {
      f <- IO.effectTotal[Int](1).forever.fork
      _ <- f.interrupt
    } yield true
  )

  // Utility stuff
  val ExampleError    = new Exception("Oh noes!")
  val InterruptCause1 = new Exception("Oh noes 1!")
  val InterruptCause2 = new Exception("Oh noes 2!")
  val InterruptCause3 = new Exception("Oh noes 3!")

  val TaskExampleError: Task[Int] = IO.fail[Throwable](ExampleError)

  def asyncExampleError[A]: Task[A] =
    IO.effectAsync[Throwable, A](_(IO.fail(ExampleError)))

  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def deepMapPoint(n: Int): UIO[Int] = {
    @tailrec
    def loop(n: Int, acc: UIO[Int]): UIO[Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.map(_ + 1))

    loop(n, IO.succeedLazy(0))
  }

  def deepMapNow(n: Int): UIO[Int] = {
    @tailrec
    def loop(n: Int, acc: UIO[Int]): UIO[Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.map(_ + 1))

    loop(n, IO.succeed(0))
  }

  def deepMapEffect(n: Int): UIO[Int] = {
    @tailrec
    def loop(n: Int, acc: UIO[Int]): UIO[Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.map(_ + 1))

    loop(n, IO.effectTotal(0))
  }

  def deepErrorEffect(n: Int): Task[Unit] =
    if (n == 0) IO.effect(throw ExampleError)
    else IO.unit *> deepErrorEffect(n - 1)

  def deepErrorFail(n: Int): Task[Unit] =
    if (n == 0) IO.fail(ExampleError)
    else IO.unit *> deepErrorFail(n - 1)

  def fib(n: Int): BigInt =
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)

  def concurrentFib(n: Int): Task[BigInt] =
    if (n <= 1) IO.succeedLazy[BigInt](n)
    else
      for {
        f1 <- concurrentFib(n - 1).fork
        f2 <- concurrentFib(n - 2).fork
        v1 <- f1.join
        v2 <- f2.join
      } yield v1 + v2

  def AsyncUnit[E]() = IO.effectAsync[E, Unit](_(IO.unit))

  def testMergeAll() =
    unsafeRun(
      IO.mergeAll(List("a", "aa", "aaa", "aaaa").map(IO.succeedLazy[String](_)))(0) { (b, a) =>
        b + a.length
      }
    ) must_=== 10

  def testMergeAllEmpty() =
    unsafeRun(
      IO.mergeAll(List.empty[UIO[Int]])(0)(_ + _)
    ) must_=== 0

  def nonFlaky(v: => ZIO[Environment, Any, Unit]): Unit =
    (1 to 100).foreach(_ => unsafeRun(v))
}
