package zio.internal

import utest._

object OneShotSpec extends TestSuite {
  override def tests: Tests = Tests {
    test("Make a new OneShot") {
      test("set must accept a non-null value") - {
        val oneShot = OneShot.make[Int]
        oneShot.set(1)

        assert(oneShot.get() == 1)
      }
      test("set must not accept a null value") - {
        val oneShot = OneShot.make[Object]

        intercept[Error](oneShot.set(null))
      }
      test("isSet must report if a value is set") - {
        val oneShot = OneShot.make[Int]
        assert(!oneShot.isSet)
        oneShot.set(1)
        assert(oneShot.isSet)
      }
      test("get must fail if no value is set") - {
        val oneShot = OneShot.make[Object]
        intercept[Error] { val _ = oneShot.get(10000L) }
      }
      test("cannot set value twice") - {
        val oneShot = OneShot.make[Int]
        oneShot.set(1)
        intercept[Error](oneShot.set(2))
      }
    }
  }
}
