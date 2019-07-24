package zio

import org.scalacheck._
import utest._

import scala.collection.mutable
import scala.util.Try
import zio.Cause.{ die, fail, interrupt, Both }

object IOSpec extends TestRuntime with GenIO with UtestScalacheckExtension {
  import Prop.forAll

  override def tests: Tests = Tests {
    test("Generate a list of String and a f: String => Task[Int]") {
      test("`IO.foreach` returns the list of results") - t1
    }
    test("Create a list of Strings and pass an f: String => IO[String, Int]") {
      test("`IO.foreach` both evaluates effects and returns the list of Ints in the same order") - t2
    }
    test("Create a list of String and pass an f: String => IO[String, Int]") {
      test("`IO.foreach` fails with a NumberFormatException exception") - t3
    }
    test("Create a list of Strings and pass an f: String => IO[String, Int]") {
      test("`IO.foreachPar` returns the list of Ints in the same order") - t4
    }
    test("Create an integer and an f: Int => String") {
      test("`IO.bimap(f, identity)` maps an IO[Int, String] into an IO[String, String]") - t5
    }
    test("Create a list of Ints and map with IO.point") {
      test("`IO.collectAllPar` returns the list of Ints in the same order") - t6
    }
    test("Create a list of Ints and map with IO.point") {
      test("`IO.forkAll` returns the list of Ints in the same order") - t7
    }
    test("Create a list of Strings and pass an f: String => UIO[Int]") {
      test("`IO.collectAllParN` returns the list of Ints in the same order") - t8
    }
    test("Create a list of Ints and pass an f: Int => UIO[Int]") {
      test("`IO.foreachParN` returns the list of created Strings in the appropriate order") - t9
    }
    test("Create a list of Ints") {
      test("`IO.foldLeft` with a successful step function sums the list properly") - t10
    }
    test("Create a non-empty list of Ints") {
      test("`IO.foldLeft` with a failing step function returns a failed IO") - t11
    }
    test("Check done lifts exit result into IO.") - testDone
    test("Check `when` executes correct branch only.") - testWhen
    test("Check `whenM` executes condition effect and correct branch.") - testWhenM
    test("Check `unsandbox` unwraps exception.") - testUnsandbox
    test("Check `supervise` returns same value as IO.supervise.") - testSupervise
    test("Check `flatten` method on IO[E, IO[E, String] returns the same IO[E, String] as `IO.flatten` does.") - testFlatten
    test(
      "Check `absolve` method on IO[E, Either[E, A]] returns the same IO[E, Either[E, String]] as `IO.absolve` does."
    ) - testAbsolve
    test("Check non-`memoize`d IO[E, A] returns new instances on repeated calls due to referential transparency.") - testNonMemoizationRT
    test("Check `memoize` method on IO[E, A] returns the same instance on repeated calls.") - testMemoization
    test("Check `raceAll` method returns the same IO[E, A] as `IO.raceAll` does.") - testRaceAll
    test("Check `firstSuccessOf` method returns the same IO[E, A] as `IO.firstSuccessOf` does.") - testfirstSuccessOf
    test("Check `zipPar` method does not swallow exit causes of loser.") - testZipParInterupt
    test("Check `zipPar` method does not report failure when interrupting loser after it succeeded.") - testZipParSucceed
    test("Check `orElse` method does not recover from defects.") - testOrElseDefectHandling
    test("Check `someOrFail` method extracts the optional value.") - testSomeOrFailExtractOptionalValue
    test("Check `someOrFail` method fails when given a None.") - testSomeOrFailWithNone
    test("Check `someOrFailException` method extracts the optional value.") - testSomeOrFailExceptionOnOptionalValue
    test("Check `someOrFailException` method fails when given a None.") - testSomeOrFailExceptionOnEmptyValue
    test("Check `rightOrFail` method extracts the Right value.") - testRightOrFailExtractsRightValue
    test("Check `rightOrFail` method fails when given a Left.") - testRightOrFailWithLeft
    test("Check `rightOrFailException` method extracts the Right value.") - testRightOrFailExceptionOnRightValue
    test("Check `rightOrFailException` method fails when given a Left.") - testRightOrFailExceptionOnLeftValue
    test("Check `leftOrFail` method extracts the Left value.") - testLeftOrFailExtractsLeftValue
    test("Check `leftOrFail` method fails when given a Right.") - testLeftOrFailWithRight
    test("Check `leftOrFailException` method extracts the Left value.") - testLeftOrFailExceptionOnLeftValue
    test("Check `leftOrFailException` method fails when given a Right.") - testLeftOrFailExceptionOnRightValue
    test("Check uncurried `bracket`.") - testUncurriedBracket
    test("Check uncurried `bracket_`.") - testUncurriedBracket_
    test("Check uncurried `bracketExit`.") - testUncurriedBracketExit
    test("Check `bracketExit` error handling.") - testBracketExitErrorHandling
    test("Check `foreach_` runs effects in order.") - testForeach_Order
    test("Check `foreach_` can be run twice.") - testForeach_Twice
    test("Check `foreachPar_` runs all effects.") - testForeachPar_Full
    test("Check `foreachParN_` runs all effects.") - testForeachParN_Full
    test("Check `filterOrElse` returns checked failure from held value") - testFilterOrElse
    test("Check `filterOrElse_` returns checked failure ignoring value") - testFilterOrElse_
    test("Check `filterOrFail` returns failure ignoring value") - testFilterOrFail
    test("Check `collect` returns failure ignoring value") - testCollect
    test("Check `collectM` returns failure ignoring value") - testCollectM
    test("Check `reject` returns failure ignoring value") - testReject
    test("Check `rejectM` returns failure ignoring value") - testRejectM
    test("Check `foreachParN` works on large lists") - testForeachParN_Threads
    test("Check `foreachParN` runs effects in parallel") - testForeachParN_Parallel
    test("Check `foreachParN` propogates error") - testForeachParN_Error
    test("Check `foreachParN` interrupts effects on first failure") - testForeachParN_Interruption
  }

  def functionIOGen: Gen[String => Task[Int]] =
    Gen.function1[String, Task[Int]](genSuccess[Throwable, Int])

  val listGen: Gen[List[String]] =
    Gen.listOfN(100, Gen.alphaNumStr)

  def t1() = propTest {
    forAll(functionIOGen, listGen) { (f, list) =>
      val res = unsafeRun(IO.foreach(list)(f))
      res.size == 100 && res.isInstanceOf[List[Int]]
    }
  }

  def t2() = {
    val list    = List("1", "2", "3")
    val effects = new mutable.ListBuffer[String]
    val res     = unsafeRun(IO.foreach(list)(x => IO.effectTotal(effects += x) *> IO.succeedLazy[Int](x.toInt)))
    assertTuple((effects.toList, res), (list, List(1, 2, 3)))
  }

  def t3() = {
    val list = List("1", "h", "3")
    val res  = Try(unsafeRun(IO.foreach(list)(x => IO.succeedLazy[Int](x.toInt))))
    assert(res.isFailure, res.failed.get.isInstanceOf[FiberFailure])
  }

  def t4() = {
    val list = List("1", "2", "3")
    val res  = unsafeRun(IO.foreachPar(list)(x => IO.succeedLazy[Int](x.toInt)))
    assert(res == (List(1, 2, 3)))
  }

  def t5() = propTest {
    forAll { (i: Int) =>
      val res = unsafeRun(IO.fail[Int](i).bimap(_.toString, identity).either)
      res == Left(i.toString)
    }
  }

  def t6() = {
    val list = List(1, 2, 3).map(IO.succeedLazy[Int](_))
    val res  = unsafeRun(IO.collectAllPar(list))
    assert(res == List(1, 2, 3))
  }

  def t7() = {
    val list = List(1, 2, 3).map(IO.succeedLazy[Int](_))
    val res  = unsafeRun(IO.forkAll(list).flatMap[Any, Nothing, List[Int]](_.join))
    assert(res == List(1, 2, 3))
  }

  def t8() = {
    val list = List(1, 2, 3).map(IO.succeedLazy[Int](_))
    val res  = unsafeRun(IO.collectAllParN(2)(list))
    assert(res == List(1, 2, 3))
  }

  def t9() = {
    val list = List(1, 2, 3)
    val res  = unsafeRun(IO.foreachParN(2)(list)(x => IO.succeedLazy(x.toString)))
    assert(res == List("1", "2", "3"))
  }

  def t10() = propTest {
    forAll { (l: List[Int]) =>
      unsafeRun(IO.foldLeft(l)(0)((acc, el) => IO.succeed(acc + el))) == unsafeRun(IO.succeed(l.sum))
    }
  }

  def t11() = propTest {
    forAll { (l: List[Int]) =>
      if (l.nonEmpty) {
        unsafeRunSync(IO.foldLeft(l)(0)((_, _) => IO.fail("fail"))) == unsafeRunSync(IO.fail("fail"))
      } else true
    }
  }

  private val exampleError = new Error("something went wrong")

  def testDone() = {
    val error                         = exampleError
    val completed                     = Exit.succeed(1)
    val interrupted: Exit[Error, Int] = Exit.interrupt
    val terminated: Exit[Error, Int]  = Exit.die(error)
    val failed: Exit[Error, Int]      = Exit.fail(error)

    assert(unsafeRun(IO.done(completed)) == 1)
    assert(unsafeRunSync(IO.done(interrupted)) == Exit.interrupt)
    assert(unsafeRunSync(IO.done(terminated)) == Exit.die(error))
    assert(unsafeRunSync(IO.done(failed)) == Exit.fail(error))
  }

  def testWhen() =
    unsafeRun(
      for {
        effectRef <- Ref.make(0)
        _         <- effectRef.set(1).when(false)
        val1      <- effectRef.get
        _         <- effectRef.set(2).when(true)
        val2      <- effectRef.get
        failure   = new Exception("expected")
        _         <- IO.fail(failure).when(false)
        failed    <- IO.fail(failure).when(true).either
      } yield assert(val1 == 0, val2 == 2, failed == Left(failure))
    )

  def testWhenM() =
    unsafeRun(
      for {
        effectRef      <- Ref.make(0)
        conditionRef   <- Ref.make(0)
        conditionTrue  = conditionRef.update(_ + 1).map(_ => true)
        conditionFalse = conditionRef.update(_ + 1).map(_ => false)
        _              <- effectRef.set(1).whenM(conditionFalse)
        val1           <- effectRef.get
        conditionVal1  <- conditionRef.get
        _              <- effectRef.set(2).whenM(conditionTrue)
        val2           <- effectRef.get
        conditionVal2  <- conditionRef.get
        failure        = new Exception("expected")
        _              <- IO.fail(failure).whenM(conditionFalse)
        failed         <- IO.fail(failure).whenM(conditionTrue).either
      } yield assert(val1 == 0, conditionVal1 == 1, val2 == 2, conditionVal2 == 2, failed == Left(failure))
    )

  def testUnsandbox() = {
    val failure: IO[Cause[Exception], String] = IO.fail(fail(new Exception("fail")))
    val success: IO[Cause[Any], Int]          = IO.succeed(100)
    unsafeRun(for {
      message <- failure.unsandbox.foldM(e => IO.succeed(e.getMessage), _ => IO.succeed("unexpected"))
      result  <- success.unsandbox
    } yield assert(message == "fail", result == 100))
  }

  def testSupervise() = {
    val io = IO.effectTotal("supercalifragilisticexpialadocious")
    unsafeRun(for {
      supervise1 <- io.interruptChildren
      supervise2 <- IO.interruptChildren(io)
    } yield assert(supervise1 == supervise2))
  }

  def testFlatten() = propTest {
    forAll(Gen.alphaStr) { str =>
      unsafeRun(for {
        flatten1 <- IO.succeedLazy(IO.succeedLazy(str)).flatten
        flatten2 <- IO.flatten(IO.succeedLazy(IO.succeedLazy(str)))
      } yield flatten1 == flatten2)
    }
  }

  def testAbsolve() = propTest {
    forAll(Gen.alphaStr) { str =>
      val ioEither: UIO[Either[Nothing, String]] = IO.succeed(Right(str))
      unsafeRun(for {
        abs1 <- ioEither.absolve
        abs2 <- IO.absolve(ioEither)
      } yield abs1 == abs2)
    }
  }

  def testNonMemoizationRT() = propTest {
    forAll(Gen.alphaStr) { str =>
      val io: UIO[Option[String]] = IO.succeedLazy(Some(str)) // using `Some` for object allocation
      unsafeRun(
        (io <*> io)
          .map(tuple => !(tuple._1 eq tuple._2))
      )
    }
  }

  def testMemoization() = propTest {
    forAll(Gen.alphaStr) { str =>
      val ioMemo: UIO[UIO[Option[String]]] = IO.succeedLazy(Some(str)).memoize // using `Some` for object allocation
      unsafeRun(
        ioMemo
          .flatMap(io => io <*> io)
          .map(tuple => tuple._1 == tuple._2)
      )
    }
  }

  def testRaceAll() = {
    val io  = IO.effectTotal("supercalifragilisticexpialadocious")
    val ios = List.empty[UIO[String]]
    unsafeRun(for {
      race1 <- io.raceAll(ios)
      race2 <- IO.raceAll(io, ios)
    } yield assert(race1 == race2))
  }

  def testfirstSuccessOf() = {
    val io  = IO.effectTotal("supercalifragilisticexpialadocious")
    val ios = List.empty[UIO[String]]
    unsafeRun(for {
      race1 <- io.firstSuccessOf(ios)
      race2 <- IO.firstSuccessOf(io, ios)
    } yield assert(race1 == race2))
  }

  def testZipParInterupt() = {
    val io = ZIO.interrupt.zipPar(IO.interrupt)
    assert(unsafeRunSync(io) == Exit.Failure(Both(interrupt, interrupt)))
  }

  def testZipParSucceed() = {
    val io = ZIO.interrupt.zipPar(IO.succeed(1))
    assert(unsafeRun(io.sandbox.either).left.map(_.interrupted) == Left(true))
  }

  def testOrElseDefectHandling() = {
    val ex = new Exception("Died")

    unsafeRun {
      for {
        plain <- (ZIO.die(ex) <> IO.unit).run
        both  <- (ZIO.halt(Cause.Both(interrupt, die(ex))) <> IO.unit).run
        thn   <- (ZIO.halt(Cause.Then(interrupt, die(ex))) <> IO.unit).run
        fail  <- (ZIO.fail(ex) <> IO.unit).run
      } yield assert(plain == Exit.die(ex), both == Exit.die(ex), thn == Exit.die(ex), fail == Exit.succeed(()))
    }
  }

  def testSomeOrFailWithNone() = {
    val task: Task[Int] = UIO(Option.empty[Int]).someOrFail(exampleError)
    intercept[FiberFailure](unsafeRun(task *> UIO.unit))
  }

  def testSomeOrFailExtractOptionalValue() = {
    val task: Task[Int] = UIO(Some(42)).someOrFail(exampleError)
    assert(unsafeRun(task) == 42)
  }

  def testSomeOrFailExceptionOnOptionalValue() = assert(unsafeRun(ZIO.succeed(Some(42)).someOrFailException) == 42)

  def testSomeOrFailExceptionOnEmptyValue() = {
    val task = ZIO.succeed(Option.empty[Int]).someOrFailException
    intercept[FiberFailure](unsafeRun(task *> UIO.unit))

  }

  def testRightOrFailExceptionOnRightValue() = assert(unsafeRun(ZIO.succeed(Right(42)).rightOrFailException) == 42)

  def testRightOrFailExceptionOnLeftValue() = {
    val task: Task[Int] = ZIO.succeed(Left(2)).rightOrFailException
    intercept[FiberFailure](unsafeRun(task *> UIO.unit))
  }

  def testRightOrFailExtractsRightValue() = {
    val task: Task[Int] = UIO(Right(42)).rightOrFail(exampleError)
    assert(unsafeRun(task) == 42)
  }

  def testRightOrFailWithLeft() = {
    val task: Task[Int] = UIO(Left(1)).rightOrFail(exampleError)
    intercept[FiberFailure](unsafeRun(task *> UIO.unit))
  }

  def testLeftOrFailExceptionOnLeftValue() = assert(unsafeRun(ZIO.succeed(Left(42)).leftOrFailException) == 42)

  def testLeftOrFailExceptionOnRightValue() = {
    val task: Task[Int] = ZIO.succeed(Right(2)).leftOrFailException
    intercept[FiberFailure](unsafeRun(task *> UIO.unit))
  }

  def testLeftOrFailExtractsLeftValue() = {
    val task: Task[Int] = UIO(Left(42)).leftOrFail(exampleError)
    assert(unsafeRun(task) == 42)
  }

  def testLeftOrFailWithRight() = {
    val task: Task[Int] = UIO(Right(12)).leftOrFail(exampleError)
    intercept[FiberFailure](unsafeRun(task *> UIO.unit))
  }

  def testUncurriedBracket() =
    unsafeRun {
      for {
        release  <- Ref.make(false)
        result   <- ZIO.bracket(IO.succeed(42), (_: Int) => release.set(true), (a: Int) => ZIO.succeedLazy(a + 1))
        released <- release.get
      } yield assert(result == 43, released)
    }

  def testUncurriedBracket_() =
    unsafeRun {
      for {
        release  <- Ref.make(false)
        result   <- IO.succeed(42).bracket_(release.set(true), ZIO.succeedLazy(0))
        released <- release.get
      } yield assert(result == 0, released)
    }

  def testUncurriedBracketExit() =
    unsafeRun {
      for {
        release <- Ref.make(false)
        result <- ZIO.bracketExit(
                   IO.succeed(42),
                   (_: Int, _: Exit[_, _]) => release.set(true),
                   (_: Int) => IO.succeed(0L)
                 )
        released <- release.get
      } yield assert(result == 0L, released)
    }

  def testBracketExitErrorHandling() = {
    val releaseDied = new RuntimeException("release died")
    val exit: Exit[String, Int] = unsafeRunSync {
      ZIO.bracketExit[Any, String, Int, Int](
        ZIO.succeed(42),
        (_, _) => ZIO.die(releaseDied),
        _ => ZIO.fail("use failed")
      )
    }

    exit.fold[Unit](
      cause => assert(cause.failures == List("use failed"), cause.defects == List(releaseDied)),
      value => assert(s"unexpectedly completed with value $value" == "") //fails
    )
  }

  object UncurriedBracketCompilesRegardlessOrderOfEAndRTypes {
    class A
    class B
    class R
    class R1 extends R
    class R2 extends R1
    class E
    class E1 extends E

    def infersEType1(): ZIO[R, E, B] = {
      val acquire: ZIO[R, E, A]            = ???
      val release: A => ZIO[R, Nothing, _] = ???
      val use: A => ZIO[R, E1, B]          = ???
      ZIO.bracket(acquire, release, use)
    }

    def infersEType2(): ZIO[R, E, B] = {
      val acquire: ZIO[R, E1, A]           = ???
      val release: A => ZIO[R, Nothing, _] = ???
      val use: A => ZIO[R, E, B]           = ???
      ZIO.bracket(acquire, release, use)
    }

    def infersRType1(): ZIO[R2, E, B] = {
      val acquire: ZIO[R, E, A]             = ???
      val release: A => ZIO[R1, Nothing, _] = ???
      val use: A => ZIO[R2, E, B]           = ???
      ZIO.bracket(acquire, release, use)
    }

    def infersRType2(): ZIO[R2, E, B] = {
      val acquire: ZIO[R2, E, A]            = ???
      val release: A => ZIO[R1, Nothing, _] = ???
      val use: A => ZIO[R, E, B]            = ???
      ZIO.bracket(acquire, release, use)
    }

    def infersRType3(): ZIO[R2, E, B] = {
      val acquire: ZIO[R1, E, A]            = ???
      val release: A => ZIO[R2, Nothing, _] = ???
      val use: A => ZIO[R, E, B]            = ???
      ZIO.bracket(acquire, release, use)
    }
  }

  def testForeach_Order() = {
    val as = List(1, 2, 3, 4, 5)
    val r = unsafeRun {
      for {
        ref <- Ref.make(List.empty[Int])
        _   <- ZIO.foreach_(as)(a => ref.update(_ :+ a))
        rs  <- ref.get
      } yield rs
    }
    assert(r == as)
  }

  def testForeach_Twice() = {
    val as = List(1, 2, 3, 4, 5)
    val r = unsafeRun {
      for {
        ref <- Ref.make(0)
        zio = ZIO.foreach_(as)(a => ref.update(_ + a))
        _   <- zio
        _   <- zio
        sum <- ref.get
      } yield sum
    }
    assert(r == 30)
  }

  def testForeachPar_Full() = {
    val as = Seq(1, 2, 3, 4, 5)
    val r = unsafeRun {
      for {
        ref <- Ref.make(Seq.empty[Int])
        _   <- ZIO.foreachPar_(as)(a => ref.update(_ :+ a))
        rs  <- ref.get
      } yield rs
    }
    assert(r.length == as.length, r.sameElements(as))
  }

  def testForeachParN_Full() = {
    val as = Seq(1, 2, 3, 4, 5)
    val r = unsafeRun {
      for {
        ref <- Ref.make(Seq.empty[Int])
        _   <- ZIO.foreachParN_(2)(as)(a => ref.update(_ :+ a))
        rs  <- ref.get
      } yield rs
    }
    assert(r.length == as.length, r.forall(as.contains), as.forall(r.contains))
  }

  def testFilterOrElse() = {
    val goodCase = unsafeRun(
      exactlyOnce[Any, Int, Int](0)(_.filterOrElse(_ == 0)(a => ZIO.fail(s"$a was not 0"))).sandbox.either
    )

    val badCase = unsafeRun(
      exactlyOnce(1)(_.filterOrElse(_ == 0)(a => ZIO.fail(s"$a was not 0"))).sandbox.either
    ).left.map(_.failureOrCause)

    assert(goodCase == Right(0), badCase == Left(Left("1 was not 0")))
  }

  def testFilterOrElse_() = {
    val goodCase = unsafeRun(
      exactlyOnce(0)(_.filterOrElse_(_ == 0)(ZIO.fail("Predicate failed!"))).sandbox.either
    )

    val badCase = unsafeRun(
      exactlyOnce(1)(_.filterOrElse_(_ == 0)(ZIO.fail("Predicate failed!"))).sandbox.either
    ).left.map(_.failureOrCause)

    assert(goodCase == Right(0), badCase == Left(Left("Predicate failed!")))
  }

  def testFilterOrFail() = {
    val goodCase = unsafeRun(
      exactlyOnce(0)(_.filterOrFail(_ == 0)("Predicate failed!")).sandbox.either
    )

    val badCase = unsafeRun(
      exactlyOnce(1)(_.filterOrFail(_ == 0)("Predicate failed!")).sandbox.either
    ).left.map(_.failureOrCause)

    assert(goodCase == Right(0), badCase == Left(Left("Predicate failed!")))
  }

  def testCollect() = {
    val goodCase = unsafeRun(
      exactlyOnce(0)(_.collect(s"value was not 0")({ case v @ 0 => v })).sandbox.either
    )

    val badCase = unsafeRun(
      exactlyOnce(1)(_.collect(s"value was not 0")({ case v @ 0 => v })).sandbox.either
    ).left.map(_.failureOrCause)

    assert(goodCase == Right(0), badCase == Left(Left("value was not 0")))
  }

  def testCollectM() = {
    val goodCase = unsafeRun(
      exactlyOnce[Any, Int, Int](0)(_.collectM("Predicate failed!")({ case v @ 0 => ZIO.succeed(v) })).sandbox.either
    )

    val partialBadCase = unsafeRun(
      exactlyOnce(0)(_.collectM("Predicate failed!")({ case v @ 0 => ZIO.fail("Partial failed!") })).sandbox.either
    ).left.map(_.failureOrCause)

    val badCase = unsafeRun(
      exactlyOnce(1)(_.collectM("Predicate failed!")({ case v @ 0 => ZIO.succeed(v) })).sandbox.either
    ).left.map(_.failureOrCause)

    assert(
      goodCase == Right(0),
      partialBadCase == Left(Left("Partial failed!")),
      badCase == Left(Left("Predicate failed!"))
    )
  }

  def testReject() = {
    val goodCase = unsafeRun(
      exactlyOnce(0)(_.reject({ case v if v != 0 => "Partial failed!" })).sandbox.either
    )

    val badCase = unsafeRun(
      exactlyOnce(1)(_.reject({ case v if v != 0 => "Partial failed!" })).sandbox.either
    ).left.map(_.failureOrCause)

    assert(goodCase == Right(0), badCase == Left(Left("Partial failed!")))
  }

  def testRejectM() = {
    val goodCase = unsafeRun(
      exactlyOnce[Any, Int, Int](0)(_.rejectM({ case v if v != 0 => ZIO.succeed("Partial failed!") })).sandbox.either
    )

    val partialBadCase = unsafeRun(
      exactlyOnce(1)(_.rejectM({ case v if v != 0 => ZIO.fail("Partial failed!") })).sandbox.either
    ).left.map(_.failureOrCause)

    val badCase = unsafeRun(
      exactlyOnce(1)(_.rejectM({ case v if v != 0 => ZIO.fail("Partial failed!") })).sandbox.either
    ).left.map(_.failureOrCause)

    assert(
      goodCase == Right(0),
      partialBadCase == Left(Left("Partial failed!")),
      badCase == Left(Left("Partial failed!"))
    )
  }

  def testForeachParN_Threads() = {
    val n   = 10L
    val seq = 0 to 100000
    val res = unsafeRun(IO.foreachParN(n)(seq)(UIO.succeed))
    assert(res == seq)
  }

  def testForeachParN_Parallel() = {
    val io = for {
      p <- Promise.make[Nothing, Unit]
      _ <- UIO.foreachParN(2)(List(UIO.never, p.succeed(())))(a => a).fork
      _ <- p.await
    } yield true
    assert(unsafeRun(io))
  }

  def testForeachParN_Error() = {
    val ints = List(1, 2, 3, 4, 5, 6)
    val odds = ZIO.foreachParN(4)(ints) { n =>
      if (n % 2 != 0) ZIO.succeed(n) else ZIO.fail("not odd")
    }
    assert(unsafeRun(odds.either) == Left("not odd"))
  }

  def testForeachParN_Interruption() = {
    val actions = List(
      ZIO.never,
      ZIO.succeed(1),
      ZIO.fail("C")
    )
    val io = ZIO.foreachParN(4)(actions)(a => a)
    assert(unsafeRun(io.either) == Left("C"))
  }
}
