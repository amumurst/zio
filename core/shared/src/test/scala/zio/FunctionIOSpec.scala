package zio

import utest._
import zio.FunctionIO.{ test => _, _ }

object FunctionIOSpec extends BaseCrossPlatformSpec2 with UtestScalacheckExtension {

  override def tests: Tests = Tests {
    test("Check if the functions in `FunctionIO` work correctly") {
      test("`fromFunction` lifts from A => B into effectful function") - e1
      test("`identity` returns the identity of the input without modification") - e2
      test("`>>>` is a symbolic operator of `andThen`which does a Backwards composition of effectful functions") - e3
      test("`<<<` is a symbolic operator of `compose` which compose two effectful functions") - e4
      test("`zipWith` zips the output of two effectful functions") - e5
      test("`&&&` zips the output of two effectful functions and returns a tuple of their result") - e6
      test("`|||` computes two effectful functions left and right from from an Either input") - e7
      test("`first` returns a tuple: the output on the first element and input on the second element") - e8
      test("`second` returns a tuple: the input on the first element and output on the second element") - e9
      test(
        "`left` takes an Either as input and computes it if it is Left otherwise returns the same value of the input"
      ) - e10
      test(
        "`right`takes an Either as input and computes it if it is Right otherwise returns the same value of the input  "
      ) - e11
      test("`asEffect` returns the input value") - e12
      test("`test` check a condition and returns an Either output: Left if the condition is true otherwise false") - e13
      test(
        "`ifThenElse` check an impure condition if it is true then computes an effectful function `then0` else computes `else0`"
      ) - e14a
      test(
        "`ifThenElse` check a pure condition if it is true then computes an effectful function `then0` else computes `else0`"
      ) - e14b
      test("`whileDo` take a condition and run the body until the condition will be  false with impure function") - e15a
      test("`whileDo` take a condition and run the body until the condition will be  false with pure function") - e15b
      test("`_1` extracts out the first element of a tuple") - e16
      test("`_2` extracts out the second element of a tuple") - e17
      test("`fail` returns a failure ") - e18a
      test("`effect` can translate an Exception to an error ") - e18b
      test("`ignore` ignores a effect failure") - e19
    }
  }

  def e1() =
    unsafeRun(
      for {
        v <- fromFunction[Int, Int](_ + 1).run(4)
      } yield v must_=== 5
    )

  def e2() =
    unsafeRun(
      for {
        v <- identity[Int].run(1)
      } yield v must_=== 1
    )

  def e3() =
    unsafeRun(
      for {
        v <- (fromFunction[Int, Int](_ + 1) >>> fromFunction[Int, Int](_ * 2)).run(6)
      } yield v must_=== 14
    )

  def e4() =
    unsafeRun(
      for {
        v <- (fromFunction[Int, Int](_ + 1) <<< fromFunction[Int, Int](_ * 2)).run(6)
      } yield v must_=== 13
    )

  def e5() =
    unsafeRun(
      for {
        v <- succeedLazy(1)
              .zipWith[Nothing, Int, Int, Int](succeedLazy(2))((a, b) => a + b)
              .run(1)
      } yield v must_=== 3
    )

  def e6() =
    unsafeRun(
      for {
        v <- (fromFunction[Int, Int](_ + 1) &&& fromFunction[Int, Int](_ * 2)).run(6)
      } yield assert(v._1 == 7, v._2 == 12)
    )

  def e7() =
    unsafeRun(
      for {
        l <- (fromFunction[Int, Int](_ + 1) ||| fromFunction[Int, Int](_ * 2)).run(Left(25))
        r <- (fromFunction[List[Int], Int](_.sum) ||| fromFunction[List[Int], Int](_.size))
              .run(Right(List(1, 3, 5, 2, 8)))
      } yield assert(l == 26, r == 5)
    )

  def e8() =
    unsafeRun(
      for {
        v <- fromFunction[Int, Int](_ * 2).first.run(100)
      } yield assert(v._1 == 200, v._2 == 100)
    )

  def e9() =
    unsafeRun(
      for {
        v <- fromFunction[Int, Int](_ * 2).second.run(100)
      } yield assert(v._1 == 100, v._2 == 200)
    )
  def e10() =
    unsafeRun(
      for {
        v1 <- fromFunction[Int, Int](_ * 2).left[Int].run(Left(6))
        v2 <- succeedLazy(1).left[String].run(Right("hi"))
      } yield assert(v1 == Left(12), v2 == Right("hi"))
    )

  def e11() =
    unsafeRun(
      for {
        v1 <- fromFunction[Int, Int](_ * 2).right[String].run(Left("no value"))
        v2 <- fromFunction[Int, Int](_ * 2).right[Int].run(Right(7))
      } yield assert(v1 == Left("no value"), v2 == Right(14))
    )

  def e12() =
    unsafeRun(
      for {
        v <- fromFunction[Int, Int](_ * 2).asEffect.run(56)
      } yield v must_=== 56
    )

  def e13() =
    unsafeRun(
      for {
        v1 <- FunctionIO.test(fromFunction[Array[Int], Boolean](_.sum > 10)).run(Array(1, 2, 5))
        v2 <- FunctionIO.test(fromFunction[Array[Int], Boolean](_.sum > 10)).run(Array(1, 2, 5, 6))
      } yield assert(
        v1.isRight,
        v1.toOption.get.sameElements(Array(1, 2, 5)),
        v2.isLeft,
        v2.swap.toOption.get.sameElements(Array(1, 2, 5, 6))
      )
    )

  def e14a() =
    unsafeRun(
      for {
        v1 <- ifThenElse(fromFunction[Int, Boolean](_ > 0))(succeedLazy("is positive"))(
               succeedLazy("is negative")
             ).run(-1)
        v2 <- ifThenElse(fromFunction[Int, Boolean](_ > 0))(succeedLazy("is positive"))(
               succeedLazy("is negative")
             ).run(1)
      } yield assert(v1 == "is negative", v2 == "is positive")
    )

  def e14b() =
    unsafeRun(
      for {
        v1 <- ifThenElse(fromFunctionM[Nothing, Int, Boolean](a => IO.succeed(a > 0)))(succeedLazy("is positive"))(
               succeedLazy("is negative")
             ).run(-1)
        v2 <- ifThenElse(fromFunctionM[Nothing, Int, Boolean](a => IO.succeed(a > 0)))(succeedLazy("is positive"))(
               succeedLazy("is negative")
             ).run(1)
      } yield assert(v1 == "is negative", v2 == "is positive")
    )

  def e15a() =
    unsafeRun(
      for {
        v <- whileDo[Nothing, Int](fromFunction[Int, Boolean](_ < 10))(fromFunction[Int, Int](_ + 1)).run(1)
      } yield v must_=== 10
    )

  def e15b() =
    unsafeRun(
      for {
        v <- whileDo[Nothing, Int](fromFunctionM[Nothing, Int, Boolean](a => IO.succeed[Boolean](a < 10)))(
              fromFunctionM[Nothing, Int, Int](a => IO.effectTotal[Int](a + 1))
            ).run(1)
      } yield v must_=== 10
    )

  def e16() =
    unsafeRun(
      for {
        v <- _1[Nothing, Int, String].run((1, "hi"))
      } yield v must_=== 1
    )

  def e17() =
    unsafeRun(
      for {
        v <- _2[Nothing, Int, String].run((2, "hola"))
      } yield v must_=== "hola"
    )

  def e18a() =
    unsafeRun(
      for {
        a <- fail[String]("error").run(1).either
      } yield a must_=== Left("error")
    )
  def e18b() =
    unsafeRun(
      for {
        a <- effect[String, Int, Int] { case _: Throwable => "error" }(_ => throw new Exception).run(9).either
      } yield a must_=== Left("error")
    )

  def e19() =
    unsafeRun(
      for {
        a <- effect[String, Int, Int] { case _: Throwable => "error" }(_ => throw new Exception).run(9).ignore
      } yield a
    )
}
