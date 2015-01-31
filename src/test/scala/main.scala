import org.scalatest.FunSuite

class SispSpec extends FunSuite {
  import com.daewon.sisp.Sisp._

  test("create symbol") {
    assert(Integer(1).value == 1)
    assert(Symbol("dun").value == "dun")
    assert(nil == nil)
    val add = BuiltIn { _args: Atom =>
      def sum(args: Atom): Int = args match {
        case Pair(Integer(n), tl) => n + sum(tl)
        case `nil` => 0
      }
      Integer(sum(_args))
    }
  }

  test("list") {
    val a = Symbol("a")
    val b = Symbol("b")
    val c = Integer(10)
    assert(list(a, b) == Pair(a, Pair(b, nil)))
    assert(list(a, b, c) == Pair(a, Pair(b, Pair(c, nil))))
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
    val lambda = Closure(nil,
      Pair(Symbol("a"), Pair(Symbol("b"), nil)),
      Pair(Symbol("+"), Pair(Symbol("a"), Pair(Symbol("b"), nil)))
    )
    assert(show(lambda) == "(lambda (a b) (+ a b))")
    assert(show(BuiltIn{_ => nil}) == "built-in <function1>")

    val lambdaPair =
      Pair(Symbol("a"), list(Symbol("lambda"), list(Symbol("a")), Symbol("a")))
    // special case for lambda pair
    assert(show(lambdaPair) == "(a . (lambda (a) a))")
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

    val parent = Pair(a, Pair(b, Pair(c, Pair(d, nil))))
    var env = createEnv(parent)
    assert(parent == car(env))
    assert(nil == cdr(env))

    // unset
    env = Pair(nil, parent) // createEnv with parent value
                            // unset values just current env, without parent env
    assert(Pair(nil, Pair(a, Pair(b, Pair(c, nil)))) == Pair(nil, unset(env, Symbol("d"))))
    assert(Pair(nil, Pair(b, Pair(c, Pair(d, nil)))) == Pair(nil, unset(env, Symbol("a"))))
    assert(Pair(nil, Pair(a, Pair(c, Pair(d, nil)))) == Pair(nil, unset(env, Symbol("b"))))

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
    val add = BuiltIn { _args: Atom =>
      def sum(args: Atom): Int = args match {
        case Pair(Integer(n), tl) => n + sum(tl)
        case `nil` => 0
      }
      Integer(sum(_args))
    }
    env = set(env, Symbol("+"), add)

    val cls = Closure(env,
      list(Symbol("a"), Symbol("b")),
      list(Symbol("+"), Symbol("a"), Symbol("b"))
    )
  }

  test("eval") {
    import Environment._
    val define = Symbol("define")
    val quote = Symbol("quote")
    val lambda = Symbol("lambda")

    var env = createEnv()
    var value: Atom = nil
    var ret: Atom = nil
    var exp: Atom = nil

    // (define a 10) == a
    exp = list(define, Symbol("a"), Integer(10))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Symbol("a"))

    // (define b 20) == b
    exp = list(define, Symbol("b"), Integer(20))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Symbol("b"))

    // a == 10
    exp = Symbol("a")
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(10))

    // 100 == 100
    exp = Integer(100)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(100))

    // (lambda (a) a) == (lambda (a) a)
    exp = list(lambda, list(Symbol("a")), Symbol("a"))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Closure(env, list(Symbol("a")), Symbol("a")))

    // (define id (lambda (a) a)) == id
    var oldEnv = env;
    exp = list(define, Symbol("id"),
      list(lambda, list(Symbol("a")), Symbol("a")))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Symbol("id"))

    // id == (lambda (a) a)
    exp = Symbol("id")
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Closure(oldEnv, list(Symbol("a")), Symbol("a")))

    // (id 100) == 100
    exp = list(Symbol("id"), Integer(100))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(100))

    // built-in add
    val binaryAdd = BuiltIn { _ match {
      case Pair(Integer(a), Pair(Integer(b), `nil`)) => Integer(a + b)
    }}
    env = set(env, Symbol("+"), binaryAdd)

    // (+ 100 200) == 300
    exp = list(Symbol("+"), Integer(100), Integer(200))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(300))

    // (define x 100) == x
    exp = list(define, Symbol("x"), Integer(100))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Symbol("x"))

    // (+ x 100) == 200
    exp = list(Symbol("+"), Integer(100), Symbol("x"))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(200))

    oldEnv = env
    // (define make-adder (lambda (a) (lambda (b) (+ a b))))
    exp = list(define, Symbol("make-adder"),
      list(lambda, list(Symbol("a")),
        list(lambda, list(Symbol("b")),
          list(Symbol("+"), Symbol("a"), Symbol("b"))
        )))

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Symbol("make-adder"))

    // (make-adder 10)
    exp = list(Symbol("make-adder"), Integer(10))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)

    val envAdded = car(eval(oldEnv, list(define, Symbol("a"), Integer(10))))
    assert(value ==
      Closure(envAdded, list(Symbol("b")),
        list(Symbol("+"), Symbol("a"), Symbol("b"))))

    // ((lambda (b) (+ a b)) 100) == 110
    exp = list(value, Integer(100))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(110))

    // (((lambda (a) (lambda (b) (+ a b))) 1) 2) == 3
    exp =
      list(
        list(
          list(lambda, list(Symbol("a")),
            list(lambda, list(Symbol("b")),
              list(Symbol("+"), Symbol("a"), Symbol("b"))
            )),
          Integer(1)),
        Integer(2))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(3))

    // make build-in car function
    val builtInCar = BuiltIn { _ match {
      case Pair(hd, _) => hd
      case `nil` => nil
    }}

    val builtInCdr = BuiltIn { _ match {
      case Pair(_, tl) => tl
      case `nil` => nil
    }}

    // set built-in functions
    exp = list(define, Symbol("car"), builtInCar)
    ret = eval(env, exp)
    env = car(ret)

    exp = list(define, Symbol("cdr"), builtInCdr)
    ret = eval(env, exp)
    env = car(ret)

    exp = list(Symbol("car"), Integer(10), Integer(20))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(10))

    exp = list(Symbol("cdr"), Integer(10), Integer(20))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == list(Integer(20)))
  }

  // // lexer
  // Lexer.lex("(foo bar)") mustEqual List("(", "foo", "bar", ")")
  // val plus = cons(Builtin("+"), cons(Integer(1), cons(Integer(2), nil)))
  // val res = eval(cons(cons(Builtin("+"), Integer(3)), plus))
  // println(show(res))
}
