import org.scalatest.FunSuite

class SispSpec extends FunSuite {
  import com.daewon.sisp.Sisp._

  test("create symbol") {
    assert(Integer(1).value == 1)
    assert(Symbol("dun").value == "dun")
    assert(nil == nil)
    val a = BuiltIn(Symbol("+")) { _args: Atom =>
      def sum(args: Atom): Int = args match {
        case Pair(Integer(n), tl) => n + sum(tl)
        case `nil` => 0
      }
      Integer(sum(_args))
    }
    // sh(a.call(Pair(Integer(1), Pair(Integer(2), nil))))

    val car = BuiltIn(Symbol("car")) { args: Atom =>
      args match {
        case Pair(hd, _) => hd
        case `nil` => nil
      }
    }
    // sh(car.call(Pair(Integer(1), Pair(Integer(2), nil))))

    val cdr = BuiltIn(Symbol("cdr")) { args: Atom =>
      args match {
        case Pair(_, tl) => tl
        case `nil` => nil
      }
    }
    // sh(cdr.call(Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), nil)))))

    // assert(BuiltIn(Symbol("+"), Pair(Integer(1), Pair(Integer(2), nil))).name
    //   == Symbol("+"))
  }

  test("show") {
    assert(show(Pair(Integer(1), nil)) == "(1)")
    assert(show(Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), nil)))) == "(1 2 3)")
    assert(show(Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), Pair(Integer(4), nil))))) == "(1 2 3 4)")

    assert(show(Pair(Pair(Integer(1), nil), nil)) == "((1))")
    assert(show(Pair(Pair(Integer(1), nil), Pair(Integer(2), nil))) == "((1) 2)")

    val lst123 = Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), nil)))
    assert(show(Pair(Symbol("foo"), lst123)) == "(foo 1 2 3)")
    assert(show(Pair(Symbol("foo"), Pair(lst123, nil))) == "(foo (1 2 3))")

    assert(show(Pair(Integer(1), Integer(2))) == "(1 . 2)")
    assert(show(Pair(Integer(1), Pair(Integer(2), nil))) == "(1 2)")

    assert(show(Pair(Symbol("dun"), Pair(Integer(1), nil))) == "(dun 1)")
    assert(show(Pair(Pair(Symbol("dun"), Integer(1)), Integer(2))) == "((dun . 1) . 2)")

    val a = Pair(Symbol("a"), Integer(10))
    val b = Pair(Symbol("b"), Integer(20))
    val c = Pair(Symbol("c"), Integer(30))
    val d = Pair(Symbol("d"), Integer(40))

    assert(show(Pair(a, Pair(b, Pair(c, Pair(d, nil))))) == "((a . 10) (b . 20) (c . 30) (d . 40))")
    // assert(show(BuiltIn(Symbol("+"), Pair(Integer(1), Pair(Integer(2), nil)))) == "(+ 1 2)")
  }

  test("predicate") {
    // nil
    assert(nilp(nil))

    // (1 . 2)
    assert(pairp(Pair(Integer(1), Integer(2))))

    // (1 2)
    assert(listp(Pair(Integer(1), Integer(2))) == false)

    // ((dun . 1) . 2)
    assert(pairp(Pair(Pair(Symbol("dun"), Integer(1)), Integer(2))))
    assert(listp(Pair(Pair(Symbol("dun"), Integer(1)), Integer(2))) == false)
  }

  test("env") {
    import Environment._
    val a = Pair(Symbol("a"), Integer(10))
    val b = Pair(Symbol("b"), Integer(20))
    val c = Pair(Symbol("c"), Integer(30))
    val d = Pair(Symbol("d"), Integer(40))

    val parent = (Pair(a, Pair(b, Pair(c, Pair(d, nil)))))
    var env = createEnv(parent)
    assert(Pair(a, Pair(b, Pair(c, Pair(d, nil)))) == car(env))

    // unset
    val current = parent
    env = Pair(nil, current)
    assert(Pair(nil, Pair(a, Pair(b, Pair(c, nil)))) == unset(env, Symbol("d")))
    assert(Pair(nil, Pair(b, Pair(c, Pair(d, nil)))) == unset(env, Symbol("a")))
    assert(Pair(nil, Pair(a, Pair(c, Pair(d, nil)))) == unset(env, Symbol("b")))

    // set
    env = set(env, Symbol("age"), Integer(10))
    assert(car(env) == nil)
    assert(Pair(Symbol("age"), Integer(10)) == cadr(env))
    env = set(env, Symbol("alias"), Symbol("daewon"))
    assert(Pair(Symbol("alias"), Symbol("daewon")) == cadr(env))

    env = set(env, Symbol("a"), Integer(100))
    assert(Pair(Symbol("a"), Integer(100)) == cadr(env))

    // get
    env = Pair(parent, cdr(env))
    assert(Symbol("daewon") == get(env, Symbol("alias")))
    assert(Integer(100) == get(env, Symbol("a"))) // find from parent
  }

  test("closure") {
    import Environment._
    var env = createEnv()
    env = set(env, Symbol("age"), Integer(10))
    val args = Pair(Symbol("age"), Pair(Integer(2), nil))
  }

  test("eval") {
    import Environment._
    val define = Symbol("define")
    val quote = Symbol("quote")

    var env = createEnv()

    var res = eval(env, Pair(define, Pair(Symbol("age"), Integer(10))))
    assert(cdr(res) == Integer(10))
    env = car(res)

    // sh(Pair(define, Pair(Symbol("age"), Integer(10))))
    res = eval(env, Pair(quote, Pair(define, Pair(Symbol("age"), Integer(10)))))
    assert(cdr(res) == Pair(define, Pair(Symbol("age"), Integer(10))))
    env = car(res)
  }

  // // lexer
  // Lexer.lex("(foo bar)") mustEqual List("(", "foo", "bar", ")")
  // val plus = cons(Builtin("+"), cons(Integer(1), cons(Integer(2), nil)))
  // val res = eval(cons(cons(Builtin("+"), Integer(3)), plus))
  // println(show(res))
}
