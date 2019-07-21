package zio
package system

import utest._

import scala.reflect.io.File

object SystemSpec extends TestSuite with DefaultRuntime {
  override def tests: Tests = Tests {
    test("Fetch an environment variable and check that") {
      test("If it exists, return a reasonable value") - {
        val io = unsafeRun(system.env("PATH"))

        assert(io.isDefined, io.get.contains(File.separator + "bin"))
      }
      test("If it does not exist, return None") - {
        val io = unsafeRun(system.env("QWERTY"))
        assert(io.isEmpty)
      }
    }
    test("Fetch a VM property and check that") {
      test("If it exists, return a reasonable value") - {
        val io = unsafeRun(property("java.vm.name"))
        assert(io.isDefined, io.get.contains("VM"))
      }
      test("If it does not exist, return None") - {
        val io = unsafeRun(property("qwerty"))
        assert(io.isEmpty)
      }
    }
    test("Fetch the system's line separator and check that") {
      test("It is identical to System.lineSeparator") - {
        assert(unsafeRun(lineSeparator) == java.lang.System.lineSeparator)
      }
    }
  }
}
