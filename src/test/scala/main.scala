import org.specs2.mutable._

class SispSpec extends Specification {
  import com.daewon.sisp.Sisp._

  "Atom test" should {
    "nil is Atom" in {
      nil mustEqual nil
    }
  }

  "Predicate test" should {
    nilp(nil) mustEqual true
    listp(cons(Integer(1), Integer(2))) mustEqual false
    listp(cons(Integer(1), nil)) mustEqual true
  }

  "Symbol test" should {
    Symbol("daewon") mustEqual Symbol("daewon")
  }

  "Show test" should {
    show( cons(Integer(1), Integer(2))) mustEqual "(1 . 2)"
    show( cons(Integer(1), cons(Integer(2), nil)) ) mustEqual "(1 2)"
    show( cons(Symbol("dun"), cons(Integer(1), nil)) ) mustEqual "(dun 1)"
    show( cons(Symbol("jay"), cons(Symbol("leonard"), nil)) ) mustEqual "(jay leonard)"
    show( cons(Symbol("kina"), Symbol("day")) ) mustEqual "(kina . day)"
  }
}
