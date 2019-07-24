package zio
import utest._

object ParallelErrorsSpec extends BaseCrossPlatformSpec2 {

  override def tests: Tests = Tests {
    test("Returns a list of 2 errors") - unsafeRun(allFailures)
    test("Returns a list of 1 errors") - unsafeRun(oneFailure)
  }

  def allFailures() =
    for {
      f1     <- IO.fail("error1").fork
      f2     <- IO.fail("error2").fork
      errors <- f1.zip(f2).join.parallelErrors[String].flip
    } yield assert(errors == ::("error1", List("error2")))

  def oneFailure() =
    for {
      f1     <- IO.fail("error1").fork
      f2     <- IO.succeed("success1").fork
      errors <- f1.zip(f2).join.parallelErrors[String].flip
    } yield assert(errors == ::("error1", List()))
}
