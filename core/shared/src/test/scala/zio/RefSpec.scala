package zio

import utest._

object RefSpec extends TestSuite with DefaultRuntime {
  val (current, update) = ("value", "new value")

  sealed trait State
  case object Active  extends State
  case object Changed extends State
  case object Closed  extends State

  override def tests: Tests = Tests {
    test("Create a new Ref with a specified value and check if") {
      test("`read` returns the current value.") - {
        unsafeRun(
          for {
            ref   <- Ref.make(current)
            value <- ref.get
          } yield assert(value == current)
        )
      }
      test("`write` puts the new value correctly.") - {
        unsafeRun(
          for {
            ref   <- Ref.make(current)
            _     <- ref.set(update)
            value <- ref.get
          } yield assert(value == update)
        )
      }
      test("`update` changes the value and returns the updated value.") - {
        unsafeRun(
          for {
            ref   <- Ref.make(current)
            value <- ref.update(_ => update)
          } yield assert(value == update)
        )
      }
      test("`updateSome` changes a given type State in some cases and returns the updated value.") - {
        unsafeRun(
          for {
            ref    <- Ref.make[State](Active)
            value1 <- ref.updateSome { case Active => Changed }
            value2 <- ref.updateSome {
                       case Active  => Changed
                       case Changed => Closed
                     }
          } yield {
            assert(value1 == Changed)
            assert(value2 == Closed)
          }
        )
      }
      test("`updateSome` returns the old value for an undefined State.") - {
        unsafeRun(
          for {
            ref   <- Ref.make[State](Active)
            value <- ref.updateSome { case Closed => Changed }
          } yield assert(value == Active)
        )

      }
      test("`modify` changes the value and returns another value computed from the modification.") - {
        unsafeRun(
          for {
            ref   <- Ref.make(current)
            r     <- ref.modify[String](_ => ("hello", update))
            value <- ref.get
          } yield {
            assert(r == "hello")
            assert(value == update)
          }
        )
      }
      test("`modifySome` changes a given type State in some cases and returns a value computed from the modification.") - {
        unsafeRun(
          for {
            ref    <- Ref.make[State](Active)
            value1 <- ref.modifySome[String]("doesn't change the state") { case Active => ("changed", Changed) }
            value2 <- ref.modifySome[String]("doesn't change the state") {
                       case Active  => ("changed", Changed)
                       case Changed => ("closed", Closed)
                     }
          } yield {
            assert(value1 == "changed")
            assert(value2 == "closed")
          }
        )
      }
      test("`modifySome` returns a default value without modifying the State.") - {
        unsafeRun(
          for {
            ref   <- Ref.make[State](Active)
            value <- ref.modifySome[String]("State doesn't change") { case Closed => ("active", Active) }
          } yield assert(value == "State doesn't change")
        )
      }
    }
  }
}
