package zio.stream

import org.scalacheck.Prop.forAll
import zio.{ Chunk, UtestScalacheckExtension }
import utest._
import ArbitraryChunk._
import org.scalacheck._

object ChunkSpec extends TestSuite with UtestScalacheckExtension {
  override def tests: Tests = Tests {
    test("chunk apply") - propTest {
      implicit val chunkGen: Gen[(Chunk[Int], Int)] = for {
        chunk <- Arbitrary.arbitrary[Chunk[Int]].filter(_.length > 0)
        len   <- Gen.chooseNum(0, chunk.length - 1)
      } yield (chunk, len)

      forAll(chunkGen) { t: (Chunk[Int], Int) =>
        t._1.apply(t._2) == t._1.toSeq.apply(t._2)
      }
    }
    test("chunk length") - propTest {
      forAll { chunk: Chunk[Int] =>
        chunk.length == chunk.toSeq.length
      }
    }
    test("chunk equality prop") - propTest {
      forAll((c1: Chunk[Int], c2: Chunk[Int]) => c1.equals(c2) == c1.toSeq.equals(c2.toSeq))
    }
    test("chunk inequality") - {
      assert(Chunk(1, 2, 3, 4, 5) != Chunk(1, 2, 3, 4, 5, 6))
    }
    test("flatMap chunk") - propTest {
      forAll { (c: Chunk[Int], f: Int => Chunk[Int]) =>
        c.flatMap(f).toSeq == c.toSeq.flatMap(f.andThen(_.toSeq))
      }
    }
    test("map chunk") - propTest {
      forAll { (c: Chunk[Int], f: Int => String) =>
        c.map(f).toSeq == c.toSeq.map(f)
      }
    }
    test("materialize chunk") - propTest {
      forAll { c: Chunk[Int] =>
        c.materialize.toSeq == c.toSeq
      }
    }
    test("foldLeft chunk") - propTest {
      forAll { (s0: String, f: (String, Int) => String, c: Chunk[Int]) =>
        c.foldLeft(s0)(f) == c.toArray.foldLeft(s0)(f)
      }
    }
    test("filter chunk") - propTest {
      forAll { (chunk: Chunk[String], p: String => Boolean) =>
        chunk.filter(p).toSeq == chunk.toSeq.filter(p)
      }
    }
    test("drop chunk") - propTest {
      forAll { (chunk: Chunk[Int], n: Int) =>
        chunk.drop(n).toSeq == chunk.toSeq.drop(n)
      }
    }
    test("take chunk") - propTest {
      forAll { (c: Chunk[Int], n: Int) =>
        c.take(n).toSeq == c.toSeq.take(n)
      }
    }
    test("dropWhile chunk") - propTest {
      forAll { (c: Chunk[Int], p: Int => Boolean) =>
        c.dropWhile(p).toSeq == c.toSeq.dropWhile(p)
      }
    }
    test("takeWhile chunk") - propTest {
      forAll { (c: Chunk[Int], p: Int => Boolean) =>
        c.takeWhile(p).toSeq == c.toSeq.takeWhile(p)
      }
    }
    test("toArray") - propTest {
      forAll { c: Chunk[Int] =>
        c.toArray.toSeq == c.toSeq
      }
    }
    test("foreach") - propTest {
      forAll { c: Chunk[Int] =>
        var sum = 0
        c.foreach(sum += _)

        sum == c.toSeq.sum
      }
    }
    test("concat chunk") - propTest {
      forAll { (c1: Chunk[Int], c2: Chunk[Int]) =>
        (c1 ++ c2).toSeq == (c1.toSeq ++ c2.toSeq)
      }
    }
    test("chunk transitivity") - {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      val c3 = Chunk(1, 2, 3)
      assert((c1 == c2) && (c2 == c3) && (c1 == c3))
    }
    test("chunk symmetry") - {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert((c1 == c2) && (c2 == c1))
    }
    test("chunk reflexivity") - {
      val c1 = Chunk(1, 2, 3)
      assert(c1 == c1)
    }
    test("chunk negation") - {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert(c1 != c2 == !(c1 == c2))
    }
    test("chunk substitutivity") - {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert((c1 == c2) && (c1.toString == c2.toString))
    }
    test("chunk consistency") - {
      val c1 = (1, 2, 3)
      val c2 = (1, 2, 3)
      assert((c1 == c2) && (c1.hashCode == c2.hashCode))
    }
    test("An Array-based chunk that is filtered empty and mapped must not throw NPEs.") - {
      val c = Chunk.fromArray(Array(1, 2, 3, 4, 5))

      // foreach should not throw
      c.foreach(_ => ())

      assert(c.filter(_ => false).map(_ * 2).length == 0)
    }
    test("toArray on concat of a slice must work properly.") - {
      val onlyOdd: Int => Boolean = _ % 2 != 0
      val concat = Chunk(1, 1, 1).filter(onlyOdd) ++
        Chunk(2, 2, 2).filter(onlyOdd) ++
        Chunk(3, 3, 3).filter(onlyOdd)

      val array = concat.toArray

      assert(array.sameElements(Array(1, 1, 1, 3, 3, 3)))
    }
    test("toArray on concat of empty and integers must work properly.") - {
      assert((Chunk.empty ++ Chunk.fromArray(Array(1, 2, 3))).toArray.sameElements(Array(1, 2, 3)))
    }
    test("Chunk.filter that results in an empty Chunk must use Chunk.empty") - {
      assert(Chunk.fromArray(Array(1, 2, 3)).filter(_ => false) == Chunk.empty)
    }
  }
}
