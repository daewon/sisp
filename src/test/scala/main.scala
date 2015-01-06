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

    // show(cons(Builtin("+"), cons(Integer(1), cons(Integer(2), nil)))) mustEqual "(+ 1 2)"
  }

  test("predicate") {
    // nil
    assert(nilp(nil))
    // (1 . 2)
    assert(pairp(Pair(Integer(1), Integer(2))))
    // ((dun . 1) . 2)
    assert(pairp(Pair(Pair(Symbol("dun"), Integer(1)), Integer(2))))
    assert(listp(Pair(Pair(Symbol("dun"), Integer(1)), Integer(2))) == false)
    // (1 2)
    assert(listp(Pair(Integer(1), Integer(2))) == false)
  }

  // test("env") {
  //   import Environment._

  //   var env: Atom = createEnv
  //   env = set(Symbol("foo"), Integer(10), env)
  //   assert(car(env) == Pair(Symbol("foo"), Integer(10)))
  //   assert(get(Symbol("foo"), env) == Integer(10))

  //   env = set(Symbol("foo"), Integer(15), env)
  //   assert(car(env) == Pair(Symbol("foo"), Integer(15)))
  //   assert(get(Symbol("foo"), env) == Integer(15))

  //   env = remove(Symbol("foo"), env)
  //   env = set(Symbol("foo1"), Integer(10), env)
  //   env = set(Symbol("foo2"), Integer(20), env)
  //   env = set(Symbol("foo3"), Integer(30), env)

  //   assert(get(Symbol("foo3"), env) == Integer(30))

  //   env = set(Symbol("foo3"), nil, env)
  //   assert(get(Symbol("foo3"), env) == nil)

  //   env = set(Symbol("foo"), Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), nil))), env)
  //   assert(get(Symbol("foo"), env) ==
  //     Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), nil))))
  // }

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
