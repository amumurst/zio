package zio
import utest._

object RefMSpec extends BaseCrossPlatformSpec2 {

  override def tests: Tests = Tests {
    test("Create a new RefM with a specified value and check if") {
      test("`read` returns the current value.") - unsafeRun(e1)
      test("`write` puts the new value correctly.") - unsafeRun(e2)
      test("`update` changes the value and returns the updated value.") - unsafeRun(e3)
      test("`update` returns an error if update effect failed") - unsafeRun(e4)
      test("`updateSome` changes a given type State in some cases and returns the updated value.") - unsafeRun(e5)
      test("`updateSome` returns the old value for an undefined State.") - unsafeRun(e6)
      test("`updateSome` returns an error if update effect failed") - unsafeRun(e7)
      test("`modify` changes the value and returns another value computed from the modification.") - unsafeRun(e8)
      test("`modify` returns a error if modification effect failed") - unsafeRun(e9)
      test("`modifySome` changes a given type State in some cases and returns a value computed from the modification.") - unsafeRun(
        e10
      )
      test("`modifySome` returns a default value without modifying the State.") - unsafeRun(e11)
      test("`modifySome` returns a default value if modification effect failed") - unsafeRun(e12)
      test("`modifySome` returns a error if modification effect failed") - unsafeRun(e13)
    }
  }

  val (current, update) = ("value", "new value")
  val fail              = "fail"

  sealed trait State
  case object Active  extends State
  case object Changed extends State
  case object Closed  extends State

  def e1() =
    for {
      refM  <- RefM.make(current)
      value <- refM.get
    } yield assert(value == current)

  def e2() =
    for {
      refM  <- RefM.make(current)
      _     <- refM.set(update)
      value <- refM.get
    } yield assert(value == update)

  def e3() =
    for {
      refM  <- RefM.make(current)
      value <- refM.update(_ => IO.effectTotal(update))
    } yield assert(value == update)

  def e4() =
    (for {
      refM  <- RefM.make[String](current)
      value <- refM.update(_ => IO.fail(fail))
    } yield value).flip.map(s => assert(s == fail))

  def e5() =
    for {
      refM   <- RefM.make[State](Active)
      value1 <- refM.updateSome { case Active => IO.succeed(Changed) }
      value2 <- refM.updateSome {
                 case Active  => IO.succeed(Changed)
                 case Changed => IO.succeed(Closed)
               }
    } yield assert(value1 == Changed, value2 == Closed)

  def e6() =
    for {
      refM  <- RefM.make[State](Active)
      value <- refM.updateSome { case Closed => IO.succeed(Active) }
    } yield assert(value == Active)

  def e7() =
    (for {
      refM <- RefM.make[State](Active)
      _    <- refM.updateSome { case Active => IO.fail(fail) }
      value2 <- refM.updateSome {
                 case Active  => IO.succeed(Changed)
                 case Changed => IO.succeed(Closed)
               }
    } yield value2).flip.map(s => assert(s == fail))

  def e8() =
    for {
      refM  <- RefM.make(current)
      r     <- refM.modify(_ => IO.effectTotal(("hello", update)))
      value <- refM.get
    } yield assert(r == "hello", value == update)

  def e9() =
    (for {
      refM <- RefM.make[String](current)
      r    <- refM.modify(_ => IO.fail(fail))
    } yield r).flip map (s => assert(s == fail))

  def e10() =
    for {
      refM   <- RefM.make[State](Active)
      r1     <- refM.modifySome("doesn't change the state") { case Active => IO.succeed("changed" -> Changed) }
      value1 <- refM.get
      r2 <- refM.modifySome("doesn't change the state") {
             case Active  => IO.succeed("changed" -> Changed)
             case Changed => IO.succeed("closed"  -> Closed)
           }
      value2 <- refM.get
    } yield assert(r1 == "changed", value1 == Changed, r2 == "closed", value2 == Closed)

  def e11() =
    for {
      refM  <- RefM.make[State](Active)
      r     <- refM.modifySome("State doesn't change") { case Closed => IO.succeed("active" -> Active) }
      value <- refM.get
    } yield assert(r == "State doesn't change", value == Active)

  def e12() =
    for {
      refM  <- RefM.make[State](Active)
      r     <- refM.modifySome("State doesn't change") { case Closed => IO.fail(fail) }
      value <- refM.get
    } yield assert(r == "State doesn't change", value == Active)

  def e13() =
    (for {
      refM  <- RefM.make[State](Active)
      _     <- refM.modifySome("State doesn't change") { case Active => IO.fail(fail) }
      value <- refM.get
    } yield value).flip.map(s => assert(s == fail))
}
