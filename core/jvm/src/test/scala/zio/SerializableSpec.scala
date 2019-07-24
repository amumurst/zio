package zio

import java.io._
import utest._

object SerializableSpec extends TestRuntime {

  def serializeAndBack[T](a: T): IO[_, T] = {
    import SerializableSpecMethods._

    for {
      obj       <- IO.effectTotal(serializeToBytes(a))
      returnObj <- IO.effectTotal(getObjFromBytes[T](obj))
    } yield returnObj
  }

  override def tests: Tests = Tests {
    test("Test all classes are Serializable") {
      test("Semaphore is serializable") - e1
      test("Clock is serializable") - e2
      test("Queue is serializable") - e3
      test("Ref is serializable") - e4
      test("IO is serializable") - e5
      test("FunctionIO is serializable") - e6
      test("FiberStatus is serializable") - e7
      test("Duration is serializable") - e8
    }
  }

  def e1() = {
    val n = 20L
    unsafeRun(
      for {
        semaphore   <- Semaphore.make(n)
        count       <- semaphore.available
        returnSem   <- serializeAndBack(semaphore)
        returnCount <- returnSem.available
      } yield assert(returnCount == count)
    )
  }

  def e2() =
    unsafeRun(
      for {
        time1 <- clock.nanoTime
        time2 <- serializeAndBack(clock.nanoTime).flatten
      } yield assert(time1 <= time2)
    )

  def e3() = unsafeRun(
    for {
      queue       <- Queue.bounded[Int](100)
      _           <- queue.offer(10)
      returnQueue <- serializeAndBack(queue)
      v1          <- returnQueue.take
      _           <- returnQueue.offer(20)
      v2          <- returnQueue.take
    } yield assert(v1 == 10, v2 == 20)
  )

  def e4() = {
    val current = "This is some value"
    unsafeRun(
      for {
        ref       <- Ref.make(current)
        returnRef <- serializeAndBack(ref)
        value     <- returnRef.get
      } yield assert(value == current)
    )
  }

  def e5() = {
    val list = List("1", "2", "3")
    val io   = IO.succeedLazy(list)
    unsafeRun(
      for {
        returnIO <- serializeAndBack(io)
        result   <- returnIO
      } yield assert(result == list)
    )
  }

  def e6() = {
    import FunctionIO._
    val v = fromFunction[Int, Int](_ + 1)
    unsafeRun(
      for {
        returnKleisli <- serializeAndBack(v)
        computeV      <- returnKleisli.run(9)
      } yield assert(computeV == 10)
    )
  }

  def e7() = {
    val list = List("1", "2", "3")
    val io   = IO.succeed(list)
    val exit = unsafeRun(
      for {
        fiber          <- io.fork
        status         <- fiber.await
        returnedStatus <- serializeAndBack(status)
      } yield returnedStatus
    )
    val result = exit match {
      case Exit.Success(value) => value
      case _                   => List.empty
    }
    assert(result == list)
  }

  def e8() = {
    import zio.duration.Duration
    val duration = Duration.fromNanos(1)
    val returnDuration = unsafeRun(
      for {
        returnDuration <- serializeAndBack(duration)
      } yield returnDuration
    )

    assert(returnDuration == duration)
  }
}

object SerializableSpecMethods {
  def serializeToBytes[T](a: T): Array[Byte] = {
    val bf  = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bf)
    oos.writeObject(a)
    oos.close()
    bf.toByteArray
  }

  def getObjFromBytes[T](bytes: Array[Byte]): T = {
    val ios = new ObjectInputStream(new ByteArrayInputStream(bytes))
    ios.readObject().asInstanceOf[T]
  }

  def serializeAndDeserialize[T](a: T): T = getObjFromBytes(serializeToBytes(a))
}
