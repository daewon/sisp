import org.scalatest.FunSuite

class SispSpec extends FunSuite {
  import com.daewon.sisp.Sisp._

  import Environment._
  import Helpers._

  test("create symbol") {
    assert(Integer(1).value == 1)
    assert(Sym('dun).value == 'dun)
    assert(nil == nil)

    val sum = BuiltIn { _args: Atom =>
      def sum(args: Atom): Int = args match {
        case Pair(Integer(n), tl) => n + sum(tl)
        case `nil` => 0
      }
      Integer(sum(_args))
    }
  }

  test("list") {
    assert(l('a, 'b) == Pair(Sym('a), Pair(Sym('b), nil)))
    assert(l('a, 'b, 'c) == Pair(Sym('a), Pair(Sym('b), Pair(Sym('c), nil))))
  }

  test("show") {
    assert(show(Pair(Integer(1), nil)) == "(1)")
    assert(show(Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), nil)))) == "(1 2 3)")
    assert(show(Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), Pair(Integer(4), nil))))) == "(1 2 3 4)")

    assert(show(Pair(Pair(Integer(1), nil), nil)) == "((1))")
    assert(show(Pair(Pair(Integer(1), nil), Pair(Integer(2), nil))) == "((1) 2)")

    val lst123 = Pair(Integer(1), Pair(Integer(2), Pair(Integer(3), nil)))
    assert(show(Pair(Sym('foo), lst123)) == "(foo 1 2 3)")
    assert(show(Pair(Sym('foo), Pair(lst123, nil))) == "(foo (1 2 3))")

    assert(show(Pair(Integer(1), Integer(2))) == "(1 . 2)")
    assert(show(Pair(Integer(1), Pair(Integer(2), nil))) == "(1 2)")

    assert(show(Pair(Sym('dun), Pair(Integer(1), nil))) == "(dun 1)")
    assert(show(Pair(Pair(Sym('dun), Integer(1)), Integer(2))) == "((dun . 1) . 2)")

    val a = Pair(Sym('a), Integer(10))
    val b = Pair(Sym('b), Integer(20))
    val c = Pair(Sym('c), Integer(30))
    val d = Pair(Sym('d), Integer(40))

    assert(show(Pair(a, Pair(b, Pair(c, Pair(d, nil))))) == "((a . 10) (b . 20) (c . 30) (d . 40))")
    val lambda = Closure(nil,
      Pair(Sym('a), Pair(Sym('b), nil)),
      Pair(Sym('+), Pair(Sym('a), Pair(Sym('b), nil)))
    )

    assert(show(lambda) == "(lambda (a b) (+ a b))")
    assert(show(BuiltIn{_ => nil}) == "built-in <function1>")

    val lambdaPair =
      Pair(Sym('a), l(Sym('lambda), l(Sym('a)), Sym('a)))

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
    assert(pairp(Pair(Pair('dun, Integer(1)), Integer(2))))
    assert(listp(Pair(Pair('dun, Integer(1)), Integer(2))) == false)
  }

  test("env") {
    // get
    var parent = createEnv()
    parent = set(parent, 'a, 10)
    parent = set(parent, 'b, 20)

    var env = createEnv(parent)
    env = set(env, 'a, 200) // override parent value current env
    env = set(env, 'c, 100) // set only current env

    assert(get(env, 'a) == Integer(200)) // from current env
    assert(get(env, 'b) == Integer(20)) // from parent env
    assert(get(env, 'c) == Integer(100)) // from parent env

    // unset values just current env, without parent env
    assert(Pair(Pair('c, Integer(100)), nil) == unset(env, 'a))
  }

  test("closure") {
    var env = createEnv()
    var ret: Atom = nil
    var value: Atom = nil

    val add = BuiltIn { _args: Atom =>
      def sum(args: Atom): Int = args match {
        case Pair(Integer(n), tl) => n + sum(tl)
        case `nil` => 0
      }
      Integer(sum(_args))
    }

    env = set(env, 'age, 10)
    env = set(env, '+, add)

    val expr = l('+, 1, 'age)
    ret = eval(env, expr)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(11))

    val cls = Closure(createEnv(env), l('a, 'b), l('+, 'a, 'b))
    ret = eval(env, l(cls, 1, 2))
    env = car(ret)
    value = cdr(ret)

    assert(value == Integer(3))
  }

  test("eval") {
    var env = createEnv()
    var value: Atom = nil
    var ret: Atom = nil
    var exp: Atom = nil

    // (define a 10) == a
    exp = l('define, 'a, 10)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym('a))

    // (define b 20) == b
    exp = l('define, 'b, 20)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym('b))

    // a == 10
    exp = 'a
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(10))

    // 100 == 100
    exp = 100
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(100))

    // (lambda (a) a) == (lambda (a) a)
    exp = l('lambda, l('a), 'a)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Closure(createEnv(env), l('a), 'a))

    // (define id (lambda (a) a)) == id
    exp = l('define, 'id, l('lambda, l('a), 'a))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym('id))

    // id == (lambda (a) a)
    exp = 'id
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Closure(createEnv(env), l('a), 'a))

    // (id 100) == 100
    exp = l('id, 100)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(100))

    // built-in add
    val binaryAdd = BuiltIn { _ match {
      case Pair(Integer(a), Pair(Integer(b), `nil`)) => Integer(a + b)
    }}
    val binarySub = BuiltIn { _ match {
      case Pair(Integer(a), Pair(Integer(b), `nil`)) => Integer(a - b)
    }}
    val binaryMul = BuiltIn { _ match {
      case Pair(Integer(a), Pair(Integer(b), `nil`)) => Integer(a * b)
    }}

    env = set(env, '+, binaryAdd)
    env = set(env, '-, binarySub)
    env = set(env, '*, binaryMul)

    // (+ 100 200) == 300
    exp = l('+, 100, 200)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(300))

    // (define x 100) == x
    exp = l('define, 'x, 100)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym('x))

    // (+ x 100) == 200
    exp = l('+, 100, 'x)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(200))

    // (define make-adder (lambda (a) (lambda (b) (+ a b))))
    exp = l('define, Sym("make-adder"),
      l('lambda, l('a),
        l('lambda, l('b),
          l('+, 'a, 'b))))

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym("make-adder"))

    // make-adder
    exp = Sym("make-adder")
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value ==
      Closure(
        createEnv(env),
        l('a),
        l('lambda, l('b), l('+, 'a, 'b)))
    )

    val closureEnv = createEnv(env)
    // (make-adder 10)
    exp = l(Sym("make-adder"), 10)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)

    // assert(value ==
    //   Closure(
    //     createEnv(closureEnv),
    //     list('b),
    //     list('+, 'a, 'b)))

    // ((lambda (b) (+ a b)) 100) == 110
    exp = l(value, 100)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(110))

    // (((lambda (a) (lambda (b) (+ a b))) 1) 2) == 3
    exp =
      l(
        l(
          l('lambda, l('a),
            l('lambda, l('b),
              l('+, 'a, 'b)
            )),
          1),
        2)

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
    env = set(env, 'car, builtInCar)
    env = set(env, 'cdr, builtInCdr)

    // (car 10 20)
    exp = l('car, 10, 20)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(10))

    // (cdr 10 20)
    exp = l('cdr, 10, 20)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == l(Integer(20)))

    env = set(env, 't, 't)

    // t == true
    exp = 't
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym('t))

    // nil == false
    exp = 'nil
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == nil)

    // make if

    // (if t 3 4)
    exp = l('if, 't, 3, 4)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(3))

    // (if nil 3 4)
    exp = l('if, 'nil, 3, 4)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(4))

    val equal = BuiltIn { args: Atom =>
      if (car(args) == cadr(args)) Sym('t)
      else nil
    }

    // (if (= 1 1) 3 4)
    env = set(env, '=, equal)
    exp = l('if, l('=, 1, 1), 3, 4)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(3))

    // (if (= 1 2) 3 4)
    exp = l('if, l('=, 1, 2), 3, 4)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(4))

    // (* 2 ((lambda (x) (+ x 10) x)))
    exp = l('*, 2, l(l('lambda, l('x), l('+, 'x, 'x)), 3))
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(12))

    // (define fact (lambda (x) (if (= x 0) 1 (* x (fact (- x 1))))))
    exp = l(
      'define,
      'fact,
      l('lambda, l('x),
        l('if, l('=, 'x, 0), 1,
          l('*, 'x, l('fact, l('-, 'x, 1)) ))))

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym('fact))

    exp = l('fact, 5)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(120))
  }

  // // lexer
  // Lexer.lex("(foo bar)") mustEqual List("(", "foo", "bar", ")")
  // val plus = cons(Builtin("+"), cons(Integer(1), cons(Integer(2), nil)))
  // val res = eval(cons(cons(Builtin("+"), Integer(3)), plus))
  // println(show(res))
}
