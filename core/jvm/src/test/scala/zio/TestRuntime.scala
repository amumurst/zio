package zio

abstract class TestRuntime extends BaseCrossPlatformSpec2 {

  //TODO: Something like this to mimic timeouts. Implementation below breaks for big tests like ZStreamSpec
  /*
  //import zio.duration.Duration
  //import scala.concurrent.ExecutionContext
  override def utestWrap(path: Seq[String], runBody: => concurrent.Future[Any])(
    implicit ec: ExecutionContext
  ): concurrent.Future[Any] = {
    val testTask = ZIO.fromFuture(_ => runBody).map(_ => "Success")

    val resultTask = testTask.timeout(Duration.fromScala(DefaultTimeout)).map {
      case Some(_) => assert(true)
      case None    => assert(false)
    }

    unsafeRunToFuture(resultTask)
  }*/

  //Specs2 impl
  /*
  override final def around[R: AsResult](r: => R): Result =
    AsResult.safely(upTo(DefaultTimeout)(r)) match {
      case Skipped(m, e) if m contains "TIMEOUT" => Failure(m, e)
      case other                                 => other
    }

  override final def aroundTimeout(to: Duration)(implicit ee: ExecutionEnv): Around =
    new Around {
      def around[T: AsResult](t: => T): Result = {
        lazy val result = t
        val termination = terminate(retries = 1000, sleep = (to.toMicros / 1000).micros)
          .orSkip(_ => "TIMEOUT: " + to)(Expectations.createExpectable(result))

        if (!termination.toResult.isSkipped) AsResult(result)
        else termination.toResult
      }
    }*/
}
