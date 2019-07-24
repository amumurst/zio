package zio

import utest._

@deprecated("use FiberRef", "1.0.0")
object FiberLocalSpec extends BaseCrossPlatformSpec2 with UtestScalacheckExtension {

  override def tests: Tests = Tests {
    test("Create a new FiberLocal and") {
      test("retrieve fiber -local data that has been set") - unsafeRun(e1)
      test("empty fiber -local data") - unsafeRun(e2)
      test("automatically sets and frees data") - unsafeRun(e3)
      test("fiber - local data cannot be accessed by other fibers") - unsafeRun(e4)
      test("setting does not overwrite existing fiber -local data") - unsafeRun(e5)
    }
  }

  def e1() =
    for {
      local <- FiberLocal.make[Int]
      _     <- local.set(10)
      v     <- local.get
    } yield v must_=== Some(10)

  def e2() =
    for {
      local <- FiberLocal.make[Int]
      _     <- local.set(10)
      _     <- local.empty
      v     <- local.get
    } yield v must_=== None

  def e3() =
    for {
      local <- FiberLocal.make[Int]
      v1    <- local.locally(10)(local.get)
      v2    <- local.get
    } yield assert(v1 == Some(10), v2 == None)

  def e4() =
    for {
      local <- FiberLocal.make[Int]
      p     <- Promise.make[Nothing, Unit]
      _     <- (local.set(10) *> p.succeed(())).fork
      _     <- p.await
      v     <- local.get
    } yield assert(v == None)

  def e5() =
    for {
      local <- FiberLocal.make[Int]
      p     <- Promise.make[Nothing, Unit]
      f     <- (local.set(10) *> p.await *> local.get).fork
      _     <- local.set(20)
      _     <- p.succeed(())
      v1    <- f.join
      v2    <- local.get
    } yield assert(v1 == Some(10), v2 == Some(20))

}
