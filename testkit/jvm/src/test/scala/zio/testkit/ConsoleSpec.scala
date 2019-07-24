package zio.testkit

import utest._
import zio._
import zio.testkit.TestConsole.Data

object ConsoleSpec extends TestRuntime {
  override def tests: Tests = Tests {
    test("Outputs nothing") - {
      unsafeRun(
        for {
          ref         <- Ref.make(Data())
          testConsole <- IO.succeed(TestConsole(ref))
          output      <- testConsole.ref.get.map(_.output)
        } yield assert(output.isEmpty)
      )
    }
    test("Writes to output") - {
      unsafeRun(
        for {
          ref         <- Ref.make(Data())
          testConsole <- IO.succeed(TestConsole(ref))
          _           <- testConsole.putStr("First line")
          _           <- testConsole.putStr("Second line")
          output      <- testConsole.ref.get.map(_.output)
        } yield assert(output == Vector("First line", "Second line"))
      )
    }
    test("Writes line to output") - {
      unsafeRun(
        for {
          ref         <- Ref.make(Data())
          testConsole <- IO.succeed(TestConsole(ref))
          _           <- testConsole.putStrLn("First line")
          _           <- testConsole.putStrLn("Second line")
          output      <- testConsole.ref.get.map(_.output)
        } yield assert(output == Vector("First line\n", "Second line\n"))
      )
    }
    test("Reads from input") - {
      unsafeRun(
        for {
          ref         <- Ref.make(Data(List("Input 1", "Input 2"), Vector.empty))
          testConsole <- IO.succeed(TestConsole(ref))
          input1      <- testConsole.getStrLn
          input2      <- testConsole.getStrLn
        } yield {
          assert(input1 == "Input 1")
          assert(input2 == "Input 2")
        }
      )
    }
    test("Fails on empty input") - {
      unsafeRun(
        for {
          ref         <- Ref.make(Data())
          testConsole <- IO.succeed(TestConsole(ref))
          failed      <- testConsole.getStrLn.either
          message     = failed.fold(_.getMessage, identity)
        } yield {
          assert(failed.isLeft)
          assert(message == "There is no more input left to read")
        }
      )
    }
  }
}
