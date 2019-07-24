package zio.testkit

import utest._
import zio.testkit.TestSystem.Data
import zio.{ IO, Ref, TestRuntime }

object SystemSpec extends TestRuntime {
  override def tests: Tests = Tests {
    test("Fetch an environment variable and check that") - {
      test("If it exists, return a reasonable value") - {
        unsafeRun(
          for {
            data       <- Ref.make(Data(envs = Map("k1" -> "v1")))
            testSystem <- IO.succeed(TestSystem(data))
            env        <- testSystem.env("k1")
          } yield assert(env == Option("v1"))
        )

      }
      test("If it does not exist, return None") - {
        unsafeRun(
          for {
            data       <- Ref.make(Data())
            testSystem <- IO.succeed(TestSystem(data))
            env        <- testSystem.env("k1")
          } yield assert(env == Option.empty)
        )
      }
    }
    test("Fetch a VM property and check that") {
      test("If it exists, return a reasonable value") - {
        unsafeRun(
          for {
            data       <- Ref.make(Data(properties = Map("k1" -> "v1")))
            testSystem <- IO.succeed(TestSystem(data))
            prop       <- testSystem.property("k1")
          } yield assert(prop == Option("v1"))
        )
      }
      test("If it does not exist, return None") - {
        unsafeRun(
          for {
            data       <- Ref.make(Data())
            testSystem <- IO.succeed(TestSystem(data))
            prop       <- testSystem.property("k1")
          } yield assert(prop == Option.empty)
        )
      }
    }
    test("Fetch the system's line separator and check that") - {
      unsafeRun(
        for {
          data       <- Ref.make(Data(lineSeparator = ","))
          testSystem <- IO.succeed(TestSystem(data))
          lineSep    <- testSystem.lineSeparator
        } yield assert(lineSep == ",")
      )
    }
  }
}
