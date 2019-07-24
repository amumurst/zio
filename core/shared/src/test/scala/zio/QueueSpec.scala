package zio

import utest._
import zio.clock.Clock
import zio.duration._

import scala.collection.immutable.Range

object QueueSpec extends BaseCrossPlatformSpec2 {

  override def tests: Tests = Tests {
    test("Make a Queue and add values then call") {
      test("`take` to retrieve them in correct order.") - unsafeRun(e1)
    }
    test("`take` is called by fiber waiting on values to be added to the queue and") {
      test("join the fiber to get the added values correctly.") - unsafeRun(e2)
    }
    test("fork 10 takers and offer 10 values, join takers, the result must contain all offered values") - unsafeRun(e3)
    test("fork 10 putters and offer for each one 10 values then take the values 100 times,") {
      test("the values must be correct after join those fibers") - unsafeRun(e4)
    }
    test("make a bounded queue with capacity = 10, then put 10 values then add 10 other values and") {
      test("check that `offer`is suspended") - unsafeRun(e5)
    }
    test("make a bounded queue with capacity = 5, offer 10 values in a fiber and") {
      test("check that you can take the 10 values") - unsafeRun(e6)
    }
    test("`take` can be interrupted and all resources in takers are released") - unsafeRun(e7)
    test("`offer` can be interrupted and all resources in putters are released") - unsafeRun(e8)
    test("in an unbounded queue add values then call `take`, the values must be in correct order") - unsafeRun(e9)
    test("in an unbounded queue add values then call `takeAll`, the values must be in correct order") - unsafeRun(e10)
    test("in an unbounded queue call `takeAll` in an empty queue must return an empty list") - unsafeRun(e11)

    test("in a queue with capacity = 4 add 5 values then call `takeAll`,") {
      test("it must return a list with the 4 first values in correct order") - unsafeRun(e12)
    }
    test("make an empty queue, and `takeUpTo` with max = 2, must return an empty list") - unsafeRun(e13)
    test(
      "make a bounded queue of size 100, call `takeUpTo` with max = 101  without adding values must return an empty list"
    ) - unsafeRun(e14)
    test(
      "make a bounded queue, offer 2 values, `takeUpTo` with max = 2, must return a list that contains the first 2 offered values"
    ) - unsafeRun(e15)
    test(
      "make a bounded queue, offer 4 values, `takeUpTo` with max = 2, must return a list that contains the first 2 values"
    ) - unsafeRun(e16)
    test(
      "make a bounded queue, offer 4 values, `takeUpTo` with max = 10, must return a list that contains the offered values"
    ) - unsafeRun(e17)
    test("make a bounded queue, offer 4 values, `takeUpTo` with max = 0, must return an empty list") - unsafeRun(e18)
    test("make a bounded queue, offer 1 value, `takeUpTo` with max = -1, must return an empty list") - unsafeRun(e19)
    test("""make a bounded queue, offer 2 values, `takeUpTo` with max = 2, offer 2 values again,
            and `takeUpTo` with max = 2 again, the first result must be a list that contains the first 2 values and
            the second one must be a list with the second 2 values in order """) - unsafeRun(e20)
    test("""make a bounded queue, offer 4 values, `takeUpTo` with max = 2, and then `takeUpTo` again with max = 2;
            the first result must contain the first 2 values and the second one must
            contain the next 2 values in order""") - unsafeRun(e21)
    test("""make a bounded queue of size 4, fork offer 5 values, and `takeUpTo` with max=5 must return a list that
            contains the first 4 values in correct order """) - unsafeRun(e22)
    test("""make a bounded queue of size 10 then call `offerAll` with a list of 10 elements to add
            all values in the queue""") - unsafeRun(e23)
    test("""make a bounded queue of size 2 then call `offerAll` with a list of 3 elements. The producer should be
            suspended and the queue should have the same size as the elements offered""") - unsafeRun(e24)
    test("`offerAll` can be interrupted and all resources are released") - unsafeRun(e25)
    test("`offerAll should preserve the order of the list") - unsafeRun(e26)
    test("`offerAll` does preserve the order of the list when it exceeds the queue's capacity") - unsafeRun(e27)

    test("""make a bounded queue of size 50 then fork 100 takers, and offer as many elements as there are takers,
            the values must be correct after joining those fibers""") - unsafeRun(e28)
    test("""make a bounded queue of size 256 then fork 64 takers, and offer more elements than there are takers,
            the values must be correct after joining those fibers""") - unsafeRun(e29)
    test("""make a bounded queue of size 32 then fork 128 takers, and offer more elements than there are takers and
            capacity in the queue, the values must be correct after joining those fibers""") - unsafeRun(e30)
    test("""fork some takers, and offer less elements than there are takers in the queue, the values must be correct
            after joining those fibers""") - unsafeRun(e31)
    test("""make bounded queue of size 2 then offer more elements than there is capacity in the queue, taking elements
            should work correctly""") - unsafeRun(e32)
    test("""make bounded queue offer more elements than there are takers and capacity in the queue, taking elements
            should preserve putters queue order""") - unsafeRun(e33)
    test("""make bounded queue of size 100 then `offer` one element then `offerAll` some elements without exceeding
            the queue's capacity, when calling `takeAll` the values should be in correct order""") - unsafeRun(e34)
    test("""make bounded queue `offer` some elements then `offerAll` elements exceeding the queue's capacity,
            the values should be in correct order""") - unsafeRun(e35)
    test("""make a bounded queue of size 3, `take` a value in a fork, then `shutdown` the queue,
            the fork should be interrupted""") - unsafeRun(e36)
    test("""make a sliding queue of size 1, `take` a value in a fork, then `shutdown` the queue,
            the fork should be interrupted""") - unsafeRun(e37)
    test(
      "make a bounded queue of size 2, `offer` a value 3 times, then `shutdown` the queue, the third fork should be interrupted"
    ) - unsafeRun(e38)
    test("make a bounded queue of size 1, `shutdown` the queue, then `offer` an element, `offer` should be interrupted") - unsafeRun(
      e39
    )
    test("make a bounded queue of size 1, `shutdown` the queue, then `take` an element, `take` should be interrupted") - unsafeRun(
      e40
    )
    test(
      "make a bounded queue of size 1, `shutdown` the queue, then `takeAll` elements, `takeAll` should be interrupted"
    ) - unsafeRun(e41)
    test(
      "make a bounded queue of size 1, `shutdown` the queue, then `takeUpTo` 1 element, `takeUpTo` should be interrupted"
    ) - unsafeRun(e42)
    test("make a bounded queue of size 1, `shutdown` the queue, then get the `size`, `size` should be interrupted") - unsafeRun(
      e43
    )
    test("make a bounded queue, fill it with one offer waiting, calling `take` should free the waiting offer") - unsafeRun(
      e44
    )
    test("make a bounded queue, fill it with one offer waiting, calling `takeAll` should free the waiting offer") - unsafeRun(
      e45
    )
    test("make a bounded queue, fill it with one offer waiting, calling `takeUpTo` should free the waiting offer") - unsafeRun(
      e46
    )
    test(
      "make a bounded queue with capacity 2, fill it then offer 3 more items, calling `takeAll` 3 times should return the first 2 items, then the next 2, then the last one"
    ) - unsafeRun(e47)
    test("make a sliding queue of size 2, offering 3 values should return false and the first should be dropped") - unsafeRun(
      e48
    )
    test("make a sliding queue of size 2, offering 3 values should return fals") - unsafeRun(e49)
    test("make a sliding queue of size 100, offer values and retrieve in correct order") - unsafeRun(e50)
    test("make a sliding queue, forking takers, offering values and joining fibers should return correct value") - unsafeRun(
      e51
    )
    test("make a sliding queue of size 2, offering 6 values the queue slides correctly") - unsafeRun(e52)
    test(
      "make a bounded queue, create a shutdown hook completing a promise, then shutdown the queue, the promise should be completed"
    ) - unsafeRun(e53)
    test(
      "make a bounded queue, create a shutdown hook completing a promise twice, then shutdown the queue, both promises should be completed"
    ) - unsafeRun(e54)
    test(
      "make a bounded queue, shut it down, create a shutdown hook completing a promise, the promise should be completed immediately"
    ) - unsafeRun(e55)
    test("make a dropping queue of size 4, offering 5 values and the last should be dropped") - unsafeRun(e56)
    test("make a dropping queue of size 2, offering 6 values should return false") - unsafeRun(e57)
    test("make a dropping queue of size 128, offer values up to 256 and retrieve up to 128 in correct order") - unsafeRun(
      e58
    )
    test("make a dropping queue, forking takers, offering values and joining fibers should return the correct value") - unsafeRun(
      e59
    )
    test("make a dropping queue of size 2, offering 6 values the queue drops offers correctly") - unsafeRun(e60)
    test("make a dropping queue of size 5, offer 3 values and receive all 3 values back and should return true") - unsafeRun(
      e61
    )
    test("make a dropping queue of size 2, fork a take and then offer 4 values. Must return first item upon join") - unsafeRun(
      e62
    )
    test("make a sliding queue of size 2, fork a take and then offer 4 values. Must return last item upon join") - unsafeRun(
      e63
    )
    test("make a sliding queue of size 5 and offer 3 values. offerAll must return true") - unsafeRun(e64)
    test("make a bounded queue of size 5 and offer 3 values. offerAll must return true") - unsafeRun(e65)
    test("make a bounded queue, `poll` on empty queue must return None") - unsafeRun(e66)
    test("make a bounded queue, offer 4 values, `takeAll`, `poll` must return None") - unsafeRun(e67)
    test(
      "make a bounded queue, offer 2 values, first two `poll` return values wrapped in Some, further `poll` return None"
    ) - unsafeRun(e68)
    test("make a bounded queue, map it, offer 1 value, take returns a value equivalent to applying the function") - unsafeRun(
      e69
    )
    test("make a bounded queue, map it with identity, offer 1 value, take returns the offered value") - unsafeRun(e70)
    test("make a bounded queue, mapM it, offer 1 value, take returns a value equivalent to applying the function") - unsafeRun(
      e71
    )
    test(
      "make a bounded queue, mapM it with identity, offer 1 failing IO value and 1 successful IO value, take behaves as expected"
    ) - unsafeRun(e72)
    test("make 2 bounded queues, compose them with `both`, offer 1 value, take yields a tuple of that value") - unsafeRun(
      e73
    )
    test("make a bounded queue, contramap it, offer 1 value, take yields the result of applying the function") - unsafeRun(
      e74
    )
    test("make a bounded queue, apply filterInput, offer a value that doesn't pass, size should match") - unsafeRun(e75)
    test(
      "make a bounded queue, shut it down, offer a value, takeAllValues, shut it down, isShutdown should return false only after shutdown"
    ) - unsafeRun(e76)
  }

  def e1() =
    for {
      queue <- Queue.bounded[Int](100)
      o1    <- queue.offer(10)
      v1    <- queue.take
      o2    <- queue.offer(20)
      v2    <- queue.take
    } yield assert(v1 == 10, v2 == 20, o1, o2)

  def e2() =
    for {
      queue <- Queue.bounded[String](100)
      f1 <- queue.take
             .zipWith(queue.take)(_ + _)
             .fork
      _ <- queue.offer("don't ") *> queue.offer("give up :D")
      v <- f1.join
    } yield assert(v == "don't give up :D")

  def e3() =
    for {
      queue  <- Queue.bounded[Int](10)
      f      <- IO.forkAll(List.fill(10)(queue.take))
      values = Range.inclusive(1, 10).toList
      _      <- values.map(queue.offer).foldLeft[UIO[Boolean]](IO.succeed(false))(_ *> _)
      v      <- f.join
    } yield assert(v.forall(values.contains), values.forall(v.contains))

  def e4() =
    for {
      queue  <- Queue.bounded[Int](10)
      values = Range.inclusive(1, 10).toList
      f      <- IO.forkAll(values.map(queue.offer))
      _      <- waitForSize(queue, 10)
      l      <- queue.take.repeat(ZSchedule.recurs(9) *> ZSchedule.identity[Int].collectAll)
      _      <- f.join
    } yield assert(l.forall(values.contains), values.forall(l.contains))

  def e5() =
    (for {
      queue        <- Queue.bounded[Int](10)
      _            <- queue.offer(1).repeat(ZSchedule.recurs(9))
      refSuspended <- Ref.make[Boolean](true)
      _            <- (queue.offer(2).repeat(ZSchedule.recurs(9)) *> refSuspended.set(false)).fork
      isSuspended  <- refSuspended.get
    } yield assert(isSuspended)).interruptChildren

  def e6() =
    for {
      queue  <- Queue.bounded[Int](5)
      values = Range.inclusive(1, 10).toList
      _      <- IO.forkAll(values.map(queue.offer))
      _      <- waitForSize(queue, 10)
      l <- queue.take
            .repeat(ZSchedule.recurs(9) *> ZSchedule.identity[Int].collectAll)
    } yield assert(l.forall(values.contains), values.forall(l.contains))

  def e7() =
    for {
      queue <- Queue.bounded[Int](100)
      f     <- queue.take.fork
      _     <- waitForSize(queue, -1)
      _     <- f.interrupt
      size  <- queue.size
    } yield assert(size == 0)

  def e8() =
    for {
      queue <- Queue.bounded[Int](2)
      _     <- queue.offer(1)
      _     <- queue.offer(1)
      f     <- queue.offer(1).fork
      _     <- waitForSize(queue, 3)
      _     <- f.interrupt
      size  <- queue.size
    } yield assert(size == 2)

  def e9() =
    for {
      queue <- Queue.unbounded[Int]
      _     <- queue.offer(1)
      _     <- queue.offer(2)
      _     <- queue.offer(3)
      v1    <- queue.take
      v2    <- queue.take
      v3    <- queue.take
    } yield assert(v1 == 1, v2 == 2, v3 == 3)

  def e10() =
    for {
      queue <- Queue.unbounded[Int]
      _     <- queue.offer(1)
      _     <- queue.offer(2)
      _     <- queue.offer(3)
      v     <- queue.takeAll
    } yield assert(v == List(1, 2, 3))

  def e11() =
    for {
      queue <- Queue.unbounded[Int]
      c     <- queue.takeAll
      _     <- queue.offer(1)
      _     <- queue.take
      v     <- queue.takeAll
    } yield assert(c == List.empty, v == List.empty)

  def e12() =
    for {
      queue  <- Queue.bounded[Int](4)
      values = List(1, 2, 3, 4)
      _      <- values.map(queue.offer).foldLeft(IO.succeed(false))(_ *> _)
      _      <- queue.offer(5).fork
      _      <- waitForSize(queue, 5)
      v      <- queue.takeAll
      c      <- queue.take
    } yield assert(v.forall(values.contains), values.forall(v.contains), c == 5)

  def e13() =
    for {
      queue <- Queue.bounded[Int](100)
      list  <- queue.takeUpTo(2)
    } yield assert(list == Nil)

  def e14() =
    for {
      queue <- Queue.bounded[Int](100)
      list  <- queue.takeUpTo(101)
    } yield assert(list == Nil)

  def e15() =
    for {
      queue <- Queue.bounded[Int](100)
      _     <- queue.offer(10)
      _     <- queue.offer(20)
      list  <- queue.takeUpTo(2)
    } yield assert(list == List(10, 20))

  def e16() =
    for {
      queue <- Queue.bounded[Int](100)
      _     <- queue.offer(10)
      _     <- queue.offer(20)
      _     <- queue.offer(30)
      _     <- queue.offer(40)
      list  <- queue.takeUpTo(2)
    } yield assert(list == List(10, 20))

  def e17() =
    for {
      queue <- Queue.bounded[Int](100)
      _     <- queue.offer(10)
      _     <- queue.offer(20)
      _     <- queue.offer(30)
      _     <- queue.offer(40)
      list  <- queue.takeUpTo(10)
    } yield assert(list == List(10, 20, 30, 40))

  def e18() =
    for {
      queue <- Queue.bounded[Int](100)
      _     <- queue.offer(10)
      _     <- queue.offer(20)
      _     <- queue.offer(30)
      _     <- queue.offer(40)
      list  <- queue.takeUpTo(0)
    } yield assert(list == Nil)

  def e19() =
    for {
      queue <- Queue.bounded[Int](100)
      _     <- queue.offer(10)
      list  <- queue.takeUpTo(-1)
    } yield assert(list == Nil)

  def e20() =
    for {
      queue <- Queue.bounded[Int](100)
      _     <- queue.offer(10)
      _     <- queue.offer(20)
      list1 <- queue.takeUpTo(2)
      _     <- queue.offer(30)
      _     <- queue.offer(40)
      list2 <- queue.takeUpTo(2)
    } yield assert(list1 == List(10, 20), list2 == List(30, 40))

  def e21() =
    for {
      queue <- Queue.bounded[Int](100)
      _     <- queue.offer(10)
      _     <- queue.offer(20)
      _     <- queue.offer(30)
      _     <- queue.offer(40)
      list1 <- queue.takeUpTo(2)
      list2 <- queue.takeUpTo(2)
    } yield assert(list1 == List(10, 20), list2 == List(30, 40))

  def e22() =
    (for {
      queue  <- Queue.bounded[Int](4)
      values = List(1, 2, 3, 4)
      _      <- values.map(queue.offer).foldLeft(IO.succeed(false))(_ *> _)
      _      <- queue.offer(5).fork
      _      <- waitForSize(queue, 5)
      l      <- queue.takeUpTo(5)
    } yield assert(l == List(1, 2, 3, 4))).interruptChildren

  def e23() =
    for {
      queue  <- Queue.bounded[Int](10)
      orders = Range.inclusive(1, 10).toList
      _      <- queue.offerAll(orders)
      _      <- waitForSize(queue, 10)
      l      <- queue.takeAll
    } yield assert(l == orders)

  def e24() =
    for {
      queue  <- Queue.bounded[Int](2)
      orders = Range.inclusive(1, 3).toList
      _      <- queue.offerAll(orders).fork
      size   <- waitForSize(queue, 3)
      l      <- queue.takeAll
    } yield assert(size == 3, l == List(1, 2))

  def e25() =
    for {
      queue   <- Queue.bounded[Int](2)
      orders1 = Range.inclusive(1, 2).toList
      orders2 = Range.inclusive(3, 4).toList
      _       <- queue.offerAll(orders1)
      f       <- queue.offerAll(orders2).fork
      _       <- waitForSize(queue, 4)
      _       <- f.interrupt
      l1      <- queue.takeAll
      l2      <- queue.takeAll
    } yield assert(l1 == orders1, l2 == Nil)

  def e26() =
    for {
      queue  <- Queue.bounded[Int](100)
      orders = Range.inclusive(1, 100).toList
      _      <- queue.offerAll(orders)
      _      <- waitForSize(queue, 100)
      l      <- queue.takeAll
    } yield assert(l == orders)

  def e27() =
    for {
      queue  <- Queue.bounded[Int](64)
      orders = Range.inclusive(1, 128).toList
      _      <- queue.offerAll(orders).fork
      _      <- waitForSize(queue, 128)
      l      <- queue.takeAll
    } yield assert(l == Range.inclusive(1, 64).toList)

  def e28() =
    for {
      queue  <- Queue.bounded[Int](50)
      orders = Range.inclusive(1, 100).toList
      takers <- IO.forkAll(List.fill(100)(queue.take))
      _      <- waitForSize(queue, -100)
      _      <- queue.offerAll(orders)
      l      <- takers.join
      s      <- queue.size
    } yield assert(l.toSet == orders.toSet, s == 0)

  def e29() =
    for {
      queue  <- Queue.bounded[Int](256)
      orders = Range.inclusive(1, 128).toList
      takers <- IO.forkAll(List.fill(64)(queue.take))
      _      <- waitForSize(queue, -64)
      _      <- queue.offerAll(orders)
      l      <- takers.join
      s      <- queue.size
      values = orders.take(64)
    } yield assert(l.forall(values.contains), values.forall(l.contains), s == 64)

  def e30() =
    for {
      queue  <- Queue.bounded[Int](32)
      orders = Range.inclusive(1, 256).toList
      takers <- IO.forkAll(List.fill(128)(queue.take))
      _      <- waitForSize(queue, -128)
      _      <- queue.offerAll(orders).fork
      l      <- takers.join
      _      <- waitForSize(queue, 128)
      values = orders.take(128)
    } yield assert(l.forall(values.contains), values.forall(l.contains))

  def e31() =
    for {
      queue  <- Queue.bounded[Int](200)
      values = Range.inclusive(1, 100).toList
      takers <- IO.forkAll(List.fill(100)(queue.take))
      _      <- waitForSize(queue, -100)
      _      <- IO.forkAll(List.fill(100)(queue.take))
      _      <- waitForSize(queue, -200)
      _      <- queue.offerAll(values)
      l      <- takers.join
      s      <- queue.size
    } yield assert(l.forall(values.contains), values.forall(l.contains), s == -100)

  def e32() =
    for {
      queue  <- Queue.bounded[Int](2)
      orders = Range.inclusive(1, 3).toList
      _      <- queue.offerAll(orders).fork
      _      <- waitForSize(queue, 3)
      v1     <- queue.take
      v2     <- queue.take
      v3     <- queue.take
    } yield assert(v1 == 1, v2 == 2, v3 == 3)

  def e33() =
    for {
      queue   <- Queue.bounded[Int](2)
      orders  = Range.inclusive(1, 3).toList
      orders2 = Range.inclusive(4, 5).toList
      _       <- queue.offerAll(orders).fork
      _       <- waitForSize(queue, 3)
      _       <- queue.offerAll(orders2).fork
      _       <- waitForSize(queue, 5)
      v1      <- queue.take
      v2      <- queue.take
      v3      <- queue.take
      v4      <- queue.take
      v5      <- queue.take
    } yield assert(v1 == 1, v2 == 2, v3 == 3, v4 == 4, v5 == 5)

  def e34() =
    for {
      queue  <- Queue.bounded[Int](1000)
      orders = Range.inclusive(2, 1000).toList
      _      <- queue.offer(1)
      _      <- queue.offerAll(orders)
      _      <- waitForSize(queue, 1000)
      v1     <- queue.takeAll
    } yield assert(v1 == Range.inclusive(1, 1000).toList)

  def e35() =
    for {
      queue  <- Queue.bounded[Int](32)
      orders = Range.inclusive(3, 35).toList
      _      <- queue.offer(1)
      _      <- queue.offer(2)
      _      <- queue.offerAll(orders).fork
      _      <- waitForSize(queue, 35)
      v      <- queue.takeAll
      v1     <- queue.take
      v2     <- queue.take
      v3     <- queue.take
    } yield assert(v == Range.inclusive(1, 32).toList, v1 == 33, v2 == 34, v3 == 35)

  def e36() =
    (
      for {
        queue <- Queue.bounded[Int](3)
        f     <- queue.take.fork
        _     <- waitForSize(queue, -1)
        _     <- queue.shutdown
        _     <- f.join
      } yield ()
    ) mustFailBecauseOf Cause.interrupt

  def e37() =
    (
      for {
        queue <- Queue.sliding[Int](1)
        f     <- queue.take.fork
        _     <- waitForSize(queue, -1)
        _     <- queue.shutdown
        _     <- f.join
      } yield ()
    ) mustFailBecauseOf Cause.interrupt

  def e38() =
    (
      for {
        queue <- Queue.bounded[Int](2)
        _     <- queue.offer(1)
        _     <- queue.offer(1)
        f     <- queue.offer(1).fork
        _     <- waitForSize(queue, 3)
        _     <- queue.shutdown
        _     <- f.join
      } yield ()
    ) mustFailBecauseOf Cause.interrupt

  def e39() =
    (
      for {
        queue <- Queue.bounded[Int](1)
        _     <- queue.shutdown
        _     <- queue.offer(1)
      } yield ()
    ) mustFailBecauseOf Cause.interrupt

  def e40() =
    (
      for {
        queue <- Queue.bounded[Int](1)
        _     <- queue.shutdown
        _     <- queue.take
      } yield ()
    ) mustFailBecauseOf Cause.interrupt

  def e41() =
    (
      for {
        queue <- Queue.bounded[Int](1)
        _     <- queue.shutdown
        _     <- queue.takeAll
      } yield ()
    ) mustFailBecauseOf Cause.interrupt

  def e42() =
    (
      for {
        queue <- Queue.bounded[Int](1)
        _     <- queue.shutdown
        _     <- queue.takeUpTo(1)
      } yield ()
    ) mustFailBecauseOf Cause.interrupt

  def e43() =
    (
      for {
        queue <- Queue.bounded[Int](1)
        _     <- queue.shutdown
        _     <- queue.size
      } yield ()
    ) mustFailBecauseOf Cause.interrupt

  def e44() =
    for {
      queue <- Queue.bounded[Int](2)
      _     <- queue.offerAll(List(1, 2))
      f     <- queue.offer(3).fork
      _     <- waitForSize(queue, 3)
      v1    <- queue.take
      v2    <- queue.take
      _     <- f.join
    } yield assert(v1 == 1, v2 == 2)

  def e45() =
    for {
      queue <- Queue.bounded[Int](2)
      _     <- queue.offerAll(List(1, 2))
      f     <- queue.offer(3).fork
      _     <- waitForSize(queue, 3)
      v1    <- queue.takeAll
      _     <- f.join
    } yield assert(v1 == List(1, 2))

  def e46() =
    for {
      queue <- Queue.bounded[Int](2)
      _     <- queue.offerAll(List(1, 2))
      f     <- queue.offer(3).fork
      _     <- waitForSize(queue, 3)
      v1    <- queue.takeUpTo(2)
      _     <- f.join
    } yield assert(v1 == List(1, 2))

  def e47() =
    for {
      queue <- Queue.bounded[Int](2)
      _     <- queue.offerAll(List(1, 2))
      f     <- queue.offerAll(List(3, 4, 5)).fork
      _     <- waitForSize(queue, 5)
      v1    <- queue.takeAll
      v2    <- queue.takeAll
      v3    <- queue.takeAll
      _     <- f.join
    } yield assert(v1 == List(1, 2), v2 == List(3, 4), v3 == List(5))

  def e48() =
    for {
      queue <- Queue.sliding[Int](2)
      _     <- queue.offer(1)
      v1    <- queue.offer(2)
      v2    <- queue.offer(3)
      l     <- queue.takeAll
    } yield assert(l == List(2, 3), v1, !v2)

  def e49() =
    for {
      queue <- Queue.sliding[Int](2)
      v     <- queue.offerAll(List(1, 2, 3))
      size  <- queue.size
    } yield assert(size == 2, !v)

  def e50() =
    for {
      queue <- Queue.sliding[Int](100)
      _     <- queue.offer(1)
      _     <- queue.offer(2)
      _     <- queue.offer(3)
      l     <- queue.takeAll
    } yield assert(l == List(1, 2, 3))

  def e51() =
    for {
      queue <- Queue.sliding[Int](5)
      f1 <- queue.take
             .zipWith(queue.take)(_ + _)
             .fork
      _ <- queue.offer(1) *> queue.offer(2)
      v <- f1.join
    } yield assert(v == 3)

  def e52() =
    for {
      queue <- Queue.sliding[Int](2)
      v1    <- queue.offerAll(Iterable(1, 2, 3, 4, 5, 6))
      l     <- queue.takeAll
    } yield assert(l == List(5, 6), !v1)

  def e53() =
    for {
      queue <- Queue.bounded[Int](3)
      p     <- Promise.make[Nothing, Boolean]
      _     <- (queue.awaitShutdown *> p.succeed(true)).fork
      _     <- queue.shutdown
      res   <- p.await
    } yield assert(res)

  def e54() =
    for {
      queue <- Queue.bounded[Int](3)
      p1    <- Promise.make[Nothing, Boolean]
      p2    <- Promise.make[Nothing, Boolean]
      _     <- (queue.awaitShutdown *> p1.succeed(true)).fork
      _     <- (queue.awaitShutdown *> p2.succeed(true)).fork
      _     <- queue.shutdown
      res1  <- p1.await
      res2  <- p2.await
    } yield assert(res1, res2)

  def e55() =
    for {
      queue <- Queue.bounded[Int](3)
      _     <- queue.shutdown
      p     <- Promise.make[Nothing, Boolean]
      _     <- (queue.awaitShutdown *> p.succeed(true)).fork
      res   <- p.await
    } yield assert(res)

  def e56() =
    for {
      capacity <- IO.succeed(4)
      queue    <- Queue.dropping[Int](capacity)
      iter     = Range.inclusive(1, 5)
      _        <- queue.offerAll(iter)
      ta       <- queue.takeAll
    } yield assert(ta == List(1, 2, 3, 4), ta.size == capacity)

  def e57() =
    for {
      capacity <- IO.succeed(2)
      queue    <- Queue.dropping[Int](capacity)
      v1       <- queue.offerAll(Iterable(1, 2, 3, 4, 5, 6))
      ta       <- queue.takeAll
    } yield assert(ta.size == 2, !v1)

  def e58() =
    for {
      capacity <- IO.succeed(128)
      queue    <- Queue.dropping[Int](capacity)
      iter     = Range.inclusive(1, 256)
      _        <- queue.offerAll(iter)
      ta       <- queue.takeAll
    } yield assert(ta == Range.inclusive(1, 128).toList, ta.size == capacity)

  def e59() =
    for {
      queue <- Queue.dropping[Int](5)
      f1 <- queue.take
             .zipWith(queue.take)(_ + _)
             .fork
      _ <- queue.offer(1) *> queue.offer(2)
      v <- f1.join
    } yield assert(v == 3)

  def e60() =
    for {
      capacity <- IO.succeed(2)
      queue    <- Queue.dropping[Int](capacity)
      iter     = Range.inclusive(1, 6)
      _        <- queue.offerAll(iter)
      ta       <- queue.takeAll
    } yield assert(ta == List(1, 2), ta.size == capacity)

  def e61() =
    for {
      capacity <- IO.succeed(5)
      queue    <- Queue.dropping[Int](capacity)
      iter     = Range.inclusive(1, 3)
      v1       <- queue.offerAll(iter)
      ta       <- queue.takeAll
    } yield assert(ta == List(1, 2, 3), v1)

  def e62() =
    for {
      capacity <- IO.succeed(2)
      queue    <- Queue.dropping[Int](capacity)
      iter     = Range.inclusive(1, 4)
      f        <- queue.take.fork
      _        <- waitForSize(queue, -1)
      oa       <- queue.offerAll(iter.toList)
      j        <- f.join
    } yield assert(j == 1, !oa)

  def e63() =
    for {
      capacity <- IO.succeed(2)
      queue    <- Queue.sliding[Int](capacity)
      iter     = Range.inclusive(1, 4)
      _        <- queue.take.fork
      _        <- waitForSize(queue, -1)
      oa       <- queue.offerAll(iter.toList)
      t        <- queue.take
    } yield assert(t == 3, !oa)

  def e64() =
    for {
      capacity <- IO.succeed(5)
      queue    <- Queue.sliding[Int](capacity)
      iter     = Range.inclusive(1, 3)
      oa       <- queue.offerAll(iter.toList)
    } yield assert(oa)

  def e65() =
    for {
      capacity <- IO.succeed(5)
      queue    <- Queue.bounded[Int](capacity)
      iter     = Range.inclusive(1, 3)
      oa       <- queue.offerAll(iter.toList)
    } yield assert(oa)

  def e66() =
    for {
      queue <- Queue.bounded[Int](5)
      t     <- queue.poll
    } yield assert(t.isEmpty)

  def e67() =
    for {
      queue <- Queue.bounded[Int](5)
      iter  = Range.inclusive(1, 4)
      _     <- queue.offerAll(iter.toList)
      _     <- queue.takeAll
      t     <- queue.poll
    } yield assert(t.isEmpty)

  def e68() =
    for {
      queue <- Queue.bounded[Int](5)
      iter  = Range.inclusive(1, 2)
      _     <- queue.offerAll(iter.toList)
      t1    <- queue.poll
      t2    <- queue.poll
      t3    <- queue.poll
      t4    <- queue.poll
    } yield assert(t1.contains(1), t2.contains(2), t3.isEmpty, t4.isEmpty)

  def e69() =
    for {
      q <- Queue.bounded[Int](100).map(_.map(_.toString))
      _ <- q.offer(10)
      v <- q.take
    } yield assert(v == "10")

  def e70() =
    for {
      q <- Queue.bounded[Int](100).map(_.map(identity))
      _ <- q.offer(10)
      v <- q.take
    } yield assert(v == 10)

  def e71() =
    for {
      q <- Queue.bounded[Int](100).map(_.mapM(IO.succeed))
      _ <- q.offer(10)
      v <- q.take
    } yield assert(v == 10)

  def e72() =
    for {
      q  <- Queue.bounded[IO[String, Int]](100).map(_.mapM(identity))
      _  <- q.offer(IO.fail("Ouch"))
      _  <- q.offer(IO.succeed(10))
      v1 <- q.take.run
      v2 <- q.take.run
    } yield assert(v1 == Exit.fail("Ouch"), v2 == Exit.succeed(10))

  def e73() =
    for {
      q1 <- Queue.bounded[Int](100)
      q2 <- Queue.bounded[Int](100)
      q  = q1 both q2
      _  <- q.offer(10)
      v  <- q.take
    } yield assert(v._1 == 10, v._2 == 10)

  def e74() =
    for {
      q <- Queue.bounded[String](100).map(_.contramap[Int](_.toString))
      _ <- q.offer(10)
      v <- q.take
    } yield assert(v == "10")

  def e75() =
    for {
      q  <- Queue.bounded[Int](100).map(_.filterInput[Int](_ % 2 == 0))
      _  <- q.offer(1)
      s1 <- q.size
      _  <- q.offer(2)
      s2 <- q.size
    } yield assert(s1 == 0, s2 == 1)

  def e76() =
    for {
      queue <- Queue.bounded[Int](5)
      r1    <- queue.isShutdown
      _     <- queue.offer(1)
      r2    <- queue.isShutdown
      _     <- queue.takeAll
      r3    <- queue.isShutdown
      _     <- queue.shutdown
      r4    <- queue.isShutdown
    } yield assert(!r1, !r2, !r3, r4)

  def waitForSize[A](queue: Queue[A], size: Int): ZIO[Clock, Nothing, Int] =
    (queue.size <* clock.sleep(10.millis)).repeat(ZSchedule.doWhile(_ != size))
}
