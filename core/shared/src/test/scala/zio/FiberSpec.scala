package zio
import utest._

object FiberSpec extends BaseCrossPlatformSpec2 with UtestScalacheckExtension {

  override def tests: Tests = Tests {
    test("Create a new Fiber and") {
      test("lift it into Managed") - unsafeRun(e1)
    }
    test("`inheritLocals` works for Fiber created using") {
      test("`map`") - unsafeRun(e2)
      test("`orElse`") - unsafeRun(e3)
      test("`zip`") - unsafeRun(e4)
    }
  }

  val (initial, update) = ("initial", "update")

  def e1() =
    for {
      ref <- Ref.make(false)
      fiber <- withLatch { release =>
                (release *> IO.unit)
                  .bracket_[Any, Nothing]
                  .apply[Any](ref.set(true))(IO.never)
                  .fork //    TODO: Dotty doesn't infer this properly
              }
      _     <- fiber.toManaged.use(_ => IO.unit)
      _     <- fiber.await
      value <- ref.get
    } yield assert(value)

  def e2() =
    for {
      fiberRef <- FiberRef.make(initial)
      child <- withLatch { release =>
                (fiberRef.set(update) *> release).fork
              }
      _     <- child.map(_ => ()).inheritFiberRefs
      value <- fiberRef.get
    } yield assert(value == update)

  def e3() =
    for {
      fiberRef  <- FiberRef.make(initial)
      semaphore <- Semaphore.make(2)
      _         <- semaphore.acquireN(2)
      child1    <- (fiberRef.set("child1") *> semaphore.release).fork
      child2    <- (fiberRef.set("child2") *> semaphore.release).fork
      _         <- semaphore.acquireN(2)
      _         <- child1.orElse(child2).inheritFiberRefs
      value     <- fiberRef.get
    } yield assert(value == "child1")

  def e4() =
    for {
      fiberRef  <- FiberRef.make(initial)
      semaphore <- Semaphore.make(2)
      _         <- semaphore.acquireN(2)
      child1    <- (fiberRef.set("child1") *> semaphore.release).fork
      child2    <- (fiberRef.set("child2") *> semaphore.release).fork
      _         <- semaphore.acquireN(2)
      _         <- child1.zip(child2).inheritFiberRefs
      value     <- fiberRef.get
    } yield assert(value == "child1")
}
