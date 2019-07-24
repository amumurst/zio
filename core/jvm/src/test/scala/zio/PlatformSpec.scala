package zio

import utest._

object PlatformSpec extends TestRuntime {

  override def tests: Tests = Tests {
    test("PlatformLive fatal") {
      test("`Platform.fatal` should identify a nonFatal exception ") - e1
      test("`Platform.fatal` should identify a fatal exception ") - e2
    }
  }

  def e1() = {
    val nonFatal = new Exception

    assert(Platform.fatal(nonFatal) == false)
  }

  def e2() = {
    val fatal = new OutOfMemoryError

    assert(Platform.fatal(fatal) == true)
  }
}
