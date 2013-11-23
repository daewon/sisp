import org.specs2.mutable._

class SispSpec extends Specification {
  import com.daewon.sisp.Sisp._

  "Atom test" should {
    "nil is Atom" in {
      nil mustEqual nil
    }
  }

  "print test" should {
    show( cons(Integer(1), Integer(2)))

    println( listp(cons(Integer(1), Integer(2))))
    println( listp(cons(Integer(1), nil)))

    show( cons(Integer(1), cons(Integer(2), nil)) )
    show( cons(Symbol("dun"), cons(Integer(1), nil)) )

    show( cons(Symbol("kina"), cons(Symbol("leonard"), nil)) )
    show( cons(Symbol("jay"), Symbol("day")) )
    show( cons(Symbol("kina"), Symbol("day")) )

  }
}
