import org.scalatest.FunSuite

class SispSpec extends FunSuite {
  import com.daewon.sisp.Sisp._

  test("create symbol") {
    assert(Integer(1).value == 1)
    assert(Symbol("dun").value == "dun")
    assert(nil == nil)
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
    // show(cons(Builtin("+"), cons(Integer(1), cons(Integer(2), nil)))) mustEqual "(+ 1 2)"
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

  // test("eval") {
  //   import Environment._
  //   implicit var env: Atom = createEnv
  //   def evalTest(expr: Atom)(implicit ev: Atom): Atom = {
  //     val (res, e) = eval(expr, ev)
  //     env = e
  //     res
  //   }

  //   println(show(env))

  //   assert(
  //     Symbol("foo") ==
  //       evalTest(Pair(Symbol("define"), Pair(Symbol("foo"), Integer(10))))
  //   )

  //   println(show(env))

  //   assert(
  //     evalTest(Symbol("foo")) == Integer(10)
  //   )

  //   assert(
  //     evalTest(Integer(10)) == Integer(10)
  //   )

  //   assert(
  //     Symbol("foo") ==
  //       evalTest(Pair(Symbol("define"), Pair(Symbol("foo"),
  //         Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), nil)))
  //       )))
  //   )

  //   assert(
  //     evalTest(Symbol("foo")) ==
  //       Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), nil)))
  //   )

  //   println(show(env))
  // }

  // // lexer
  // Lexer.lex("(foo bar)") mustEqual List("(", "foo", "bar", ")")
  // val plus = cons(Builtin("+"), cons(Integer(1), cons(Integer(2), nil)))
  // val res = eval(cons(cons(Builtin("+"), Integer(3)), plus))
  // println(show(res))
}
