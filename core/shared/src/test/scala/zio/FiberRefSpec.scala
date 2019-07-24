package zio
import utest._

object FiberRefSpec extends BaseCrossPlatformSpec2 {

  override def tests: Tests = Tests {
    test("Create a new FiberRef with a specified value and check if:") {
      test("`get` returns the current value.                         ") - unsafeRun(e1)
      test("`get` returns the current value for a child.             ") - unsafeRun(e2)
      test("`set` updates the current value.                         ") - unsafeRun(e3)
      test("`set` by a child doesn't update parent's value.          ") - unsafeRun(e4)
      test("`locally` restores original value.                       ") - unsafeRun(e5)
      test("`locally` restores parent's value.                       ") - unsafeRun(e6)
      test("`locally` restores undefined value.                      ") - unsafeRun(e7)
      test("its value is inherited on join.                          ") - unsafeRun(e8)
      test("initial value is always available.                       ") - unsafeRun(e9)
      test("`update` changes value.                                  ") - unsafeRun(e10)
      test("`updateSome` changes value.                              ") - unsafeRun(e11)
      test("`updateSome` not changes value.                          ") - unsafeRun(e12)
      test("`modify` changes value.                                  ") - unsafeRun(e13)
      test("`modifySome` not changes value.                          ") - unsafeRun(e14)
    }
  }

  val (initial, update) = ("initial", "update")

  def e1() =
    for {
      fiberRef <- FiberRef.make(initial)
      value    <- fiberRef.get
    } yield assert(value == initial)

  def e2() =
    for {
      fiberRef <- FiberRef.make(initial)
      child    <- fiberRef.get.fork
      value    <- child.join
    } yield assert(value == initial)

  def e3() =
    for {
      fiberRef <- FiberRef.make(initial)
      _        <- fiberRef.set(update)
      value    <- fiberRef.get
    } yield assert(value == update)

  def e4() =
    for {
      fiberRef <- FiberRef.make(initial)
      promise  <- Promise.make[Nothing, Unit]
      _        <- (fiberRef.set(update) *> promise.succeed(())).fork
      _        <- promise.await
      value    <- fiberRef.get
    } yield assert(value == initial)

  def e5() =
    for {
      fiberRef <- FiberRef.make(initial)
      local    <- fiberRef.locally(update)(fiberRef.get)
      value    <- fiberRef.get
    } yield assert(local == update, value == initial)

  def e6() =
    for {
      fiberRef <- FiberRef.make(initial)
      child    <- fiberRef.locally(update)(fiberRef.get).fork
      local    <- child.join
      value    <- fiberRef.get
    } yield assert(local == update, value == initial)

  def e7() =
    for {
      child <- FiberRef.make(initial).fork
      // Don't use join as it inherits values from child.
      fiberRef   <- child.await.flatMap(ZIO.done)
      localValue <- fiberRef.locally(update)(fiberRef.get)
      value      <- fiberRef.get
    } yield assert(localValue == update, value == initial)

  def e8() =
    for {
      fiberRef <- FiberRef.make(initial)
      child    <- fiberRef.set(update).fork
      _        <- child.join
      value    <- fiberRef.get
    } yield assert(value == update)

  def e9() =
    for {
      child    <- FiberRef.make(initial).fork
      fiberRef <- child.await.flatMap(ZIO.done)
      value    <- fiberRef.get
    } yield assert(value == initial)

  def e10() =
    for {
      fiberRef <- FiberRef.make(initial)
      value1   <- fiberRef.update(_ => update)
      value2   <- fiberRef.get
    } yield assert(value1 == update, value2 == update)

  def e11() =
    for {
      fiberRef <- FiberRef.make(initial)
      value1 <- fiberRef.updateSome {
                 case _ => update
               }
      value2 <- fiberRef.get
    } yield assert(value1 == update, value2 == update)

  def e12() =
    for {
      fiberRef <- FiberRef.make(initial)
      value1 <- fiberRef.updateSome {
                 case _ if false => update
               }
      value2 <- fiberRef.get
    } yield assert(value1 == initial, value2 == initial)

  def e13() =
    for {
      fiberRef <- FiberRef.make(initial)
      value1 <- fiberRef.modify {
                 case _ => (1, update)
               }
      value2 <- fiberRef.get
    } yield assert(value1 == 1, value2 == update)

  def e14() =
    for {
      fiberRef <- FiberRef.make(initial)
      value1 <- fiberRef.modifySome(2) {
                 case _ if false => (1, update)
               }
      value2 <- fiberRef.get
    } yield assert(value1 == 2, value2 == initial)
}
