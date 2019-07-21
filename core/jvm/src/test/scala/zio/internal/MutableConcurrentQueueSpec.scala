package zio.internal

import utest._
import zio.SerializableSpec._

/*
 * This spec is just a sanity check and tests RingBuffer correctness
 * in a single-threaded case.
 *
 * Concurrent tests are run via jcstress and are in [[RingBufferConcurrencyTests]].
 */

object MutableConcurrentQueueSpec extends TestSuite {
  override def tests: Tests = Tests {
    test("Make a bounded MutableConcurrentQueue") {
      test("of capacity 1 return a queue of capacity 1") - {
        val q = MutableConcurrentQueue.bounded(1).capacity
        assert(q == 1)
      }
      test("of capacity 2 returns a queue of capacity 2") - {
        val q = MutableConcurrentQueue.bounded(2).capacity
        assert(q == 2)
      }
      test("of capacity 3 returns a queue of capacity 3") - {
        val q = MutableConcurrentQueue.bounded(3).capacity
        assert(q == 3)
      }
    }
    test("With a RingBuffer of capacity 2") {
      test("`offer` of 2 items succeeds, further offers fail") - {
        val q = MutableConcurrentQueue.bounded[Int](2)

        assert(q.offer(1))
        assert(q.size() == 1)
        assert(q.offer(2))
        assert(q.size() == 2)
        assert(!q.offer(3))
        assert(q.isFull())

      }
      test("`poll` of 2 items from full queue succeeds, further `poll`s return default value") - {
        val q = MutableConcurrentQueue.bounded[Int](2)
        q.offer(1)
        q.offer(2)

        assert(q.poll(-1) == 1)
        assert(q.poll(-1) == 2)
        assert(q.poll(-1) == -1)
        assert(q.isEmpty())
      }
    }
    test("Serialization works for") {
      test("a one element queue") - {
        val q = MutableConcurrentQueue.bounded[Int](1)
        q.offer(1)
        val returnQ = serializeAndDeserialize(q)
        returnQ.offer(2)

        assert(returnQ.poll(-1) == 1)
        assert(returnQ.poll(-1) == -1)
      }
      test("a pow 2 capacity ring buffer") - {
        val q = MutableConcurrentQueue.bounded[Int](3)
        q.offer(1)
        val returnQ = serializeAndDeserialize(q)
        returnQ.offer(2)

        assert(returnQ.poll(-1) == 1)
        assert(returnQ.poll(-1) == 2)
        assert(returnQ.poll(-1) == -1)
      }
      test("an arbitrary capacity ring buffer") - {
        val q = MutableConcurrentQueue.bounded[Int](2)
        q.offer(1)
        val returnQ = serializeAndDeserialize(q)
        returnQ.offer(2)

        assert(returnQ.poll(-1) == 1)
        assert(returnQ.poll(-1) == 2)
        assert(returnQ.poll(-1) == -1)
      }
    }
  }
}
