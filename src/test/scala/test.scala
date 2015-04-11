/* references
 http://www.furidamu.org/blog/2012/03/23/scalisp-a-lisp-interpreter-in-scala/

 basic parser
 http://stackoverflow.com/a/11533809/1513517
 */

import org.scalatest.FunSuite
import com.daewon.sisp.Sisp._

import Environment._
import Helpers._

// class ScalaTest extends FunSuite {
//   test("scalaTest") {
//     trait Obj
//     class Child(a: => Obj) extends Obj
//     class Parent(a: => Obj) extends Obj

//     lazy val a: Obj = new Child(b)
//     lazy val b: Obj = new Parent(a)

//     trait YesNo[A] {
//       def yesno(value: A): Boolean
//     }

//     object YesNo {
//       implicit val intYesNo = new YesNo[Int] {
//         def yesno(value: Int) = value match {
//           case 0 => false
//           case _ => true
//         }
//       }

//       implicit val stringYesNo = new YesNo[String] {
//         def yesno(value: String) = value match {
//           case "" | "false" => false
//           case _  => true
//         }
//       }
//     }

//     object YesNoWriter {
//       def write2[A](value: A)(implicit conv: YesNo[A]): Boolean = {
//         conv.yesno(value)
//       }

//       def write[A: YesNo](value: A): Boolean = {
//         implicitly[YesNo[A]].yesno(value)
//       }
//     }

//     // println(YesNoWriter.write(1))
//     // println(YesNoWriter.write(0))

//     // println(YesNoWriter.write("false"))
//     // println(YesNoWriter.write(""))
//   }
// }

class MacroTest extends FunSuite {
  test("macro") {
    var env = createEnv()
    var ret: Atom = nil
    var value: Atom = nil
    var parsed: List[Atom] = List(nil)
    var expr = ""

    // make build-in car function
    val builtInCar = BuiltIn { _ match {
      case Cons(hd, tl) => hd match {
        case Cons(hd, tl) => hd
        case _ => nil
      }
      case `nil` => nil
    }}

    val builtInCdr = BuiltIn { _ match {
      case Cons(hd, tl) => cdr(hd)
      case `nil` => nil
    }}

    val builtInCons = BuiltIn { _ match {
      case a@Cons(hd, tl) => Cons(hd, car(tl))
      case `nil` => nil
    }}

    // built-in add
    val binaryAdd = BuiltIn { _ match {
      case Cons(Integer(a), Cons(Integer(b), `nil`)) => Integer(a + b)
    }}
    val binarySub = BuiltIn { _ match {
      case Cons(Integer(a), Cons(Integer(b), `nil`)) => Integer(a - b)
    }}
    val binaryMul = BuiltIn { _ match {
      case Cons(Integer(a), Cons(Integer(b), `nil`)) => Integer(a * b)
    }}

    env = set(env, '+, binaryAdd)
    env = set(env, '-, binarySub)
    env = set(env, '*, binaryMul)

    // set built-in functions
    env = set(env, 'car, builtInCar)
    env = set(env, 'cdr, builtInCdr)
    env = set(env, 'cons, builtInCons)

    expr = "(define plus (lambda (a b) (+ a b)))"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)

    expr = "(plus 1 2)"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)

    expr = """
    |(defmacro
    |  (unless pre yes no)
    |    (cons 'if (cons 'pre (cons no (cons yes nil)))))
    """.stripMargin

    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)

    expr = "(unless nil (cons 10 20) (cons 20 30))"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)
    assert(value == Cons(Integer(10), Integer(20)))

    expr = "(defmacro (ignore x) (cons 'quote (cons x nil)))"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)

    expr = "(ignore foo)"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)

    expr = "foo"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)
  }
}

class VarArgsTest extends FunSuite {
  test("varArgs") {
    var env = createEnv()
    var ret: Atom = nil
    var value: Atom = nil
    var parsed: List[Atom] = List(nil)
    var expr = ""

    expr = "((lambda (hd . tl) hd) 1 2)"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(1))

    expr = "((lambda (hd . tl) tl) 1 2)"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)
    assert(value == l(2))

    expr = "((lambda (a b c . rest) rest) 1 2 3 4 5)"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)
    assert(value == l(4, 5))

    expr = "((lambda (a b c . rest) c) 1 2 3 4 5)"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(3))

    expr = "((lambda args args) 1 2 3 4 5)"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)

    assert(value == l(1, 2, 3, 4, 5))

    expr = "((lambda args args))"
    parsed = Parser.parse(expr)
    ret = eval(env, parsed)
    env = car(ret)
    value = cdr(ret)

    assert(value == l())
  }
}

class ParserTest extends FunSuite {
  test("parser") {
    assert(l() == Parser.parse("()").head)
    assert(nil == Parser.parse("nil").head)
    assert(Parser.parse("nil").head == Parser.parse("()").head)

    assert(l(1, 2) == Parser.parse("(1 2)").head)
    assert(Cons(1, 2) == Parser.parse("(1 . 2)").head)
    assert(Cons(1, Cons(2, 3))  == Parser.parse("(1 2 . 3)").head)
    assert(Cons(Cons(1, 2), 3) == Parser.parse("((1 . 2) . 3)").head)
    assert(Cons(Cons(1, 2), Cons(3, nil)) == Parser.parse("((1 . 2) . (3))").head)
    assert(Cons(Cons(1, 2), Cons(3, 4)) == Parser.parse("((1 . 2) 3 . 4)").head)
    assert(Cons(Cons(Integer(1), Integer(2)), Cons(Integer(3), Cons(Integer(4), nil))) == Parser.parse("((1 . 2) 3 4)").head)
    assert(Cons(Cons(Cons(Integer(1),Integer(2)),Integer(3)),Integer(4)) == Parser.parse("(((1 . 2) . 3) . 4)").head)

    assert(Integer(1) == Parser.parse("1").head)
    assert(Sym('+) == Parser.parse("+").head)
    assert(l('+, 1, 2) == Parser.parse("(+ 1 2)").head)
    assert(l('define, 'a, l('-, 1, 2)) == Parser.parse("(define a (- 1 2))").head)
    assert(Sym('t) == Parser.parse("t").head)
    assert(nil == Parser.parse("nil").head)
    assert(l(Sym('quote), l('+, 1, 2)) == Parser.parse("`(+ 1 2)").head)
    assert(l(Sym('quote), Sym('a)) == Parser.parse("'a").head)
  }
}

class ShowTest extends FunSuite {
  test("show") {
    assert(show(Cons(Integer(1), nil)) == "(1)")
    assert(show(Cons(Integer(1), Cons(Integer(2), nil))) == "(1 2)")
    assert(show(Cons(Integer(1), Cons(Integer(2), Cons(Integer(3), nil))))  == "(1 2 3)")

    assert(show(Cons(Integer(1), Integer(2)))  == "(1 . 2)")
    assert(show(Cons(Integer(1), Cons(Integer(2), Integer(3))))  == "(1 2 . 3)")

    assert(show(Cons(Integer(1), Cons(Integer(2), Cons(Integer(3), Cons(Integer(4), nil))))) == "(1 2 3 4)")
    assert(show(Cons(Cons(1, 2), Cons(3, 4))) == "((1 . 2) 3 . 4)")
    assert(show(Cons(Cons(1, 2), Cons(3, Cons(4, 5)))) == "((1 . 2) 3 4 . 5)")

    assert(show(Cons(Cons(Integer(1), nil), nil)) == "((1))")
    assert(show(Cons(Cons(Integer(1), nil), Cons(Integer(2), nil))) == "((1) 2)")

    val lst123 = Cons(Integer(1), Cons(Integer(2), Cons(Integer(3), nil)))
    assert(show(Cons(Sym('foo), lst123)) == "(foo 1 2 3)")
    assert(show(Cons(Sym('foo), Cons(lst123, nil))) == "(foo (1 2 3))")

    assert(show(Cons(Integer(1), Integer(2))) == "(1 . 2)")
    assert(show(Cons(Integer(1), Cons(Integer(2), nil))) == "(1 2)")

    assert(show(Cons(Sym('dun), Cons(Integer(1), nil))) == "(dun 1)")
    assert(show(Cons(Cons(Sym('dun), Integer(1)), Integer(2))) == "((dun . 1) . 2)")

    val a = Cons(Sym('a), Integer(10))
    val b = Cons(Sym('b), Integer(20))
    val c = Cons(Sym('c), Integer(30))
    val d = Cons(Sym('d), Integer(40))

    assert(show(Cons(a, Cons(b, Cons(c, Cons(d, nil))))) == "((a . 10) (b . 20) (c . 30) (d . 40))")
    val lambda = Closure(nil,
      Cons(Sym('a), Cons(Sym('b), nil)),
      Cons(Sym('+), Cons(Sym('a), Cons(Sym('b), nil)))
    )

    assert(show(lambda) == "(lambda (a b) (+ a b))")
    assert(show(BuiltIn{_ => nil}) == "built-in <function1>")

    val lambdaCons =
      Cons(Sym('a), l(Sym('lambda), l(Sym('a)), Sym('a)))

    assert(show(lambdaCons) == "(a lambda (a) a)")

    val varArgs = Closure(nil,
      Cons(Sym('a), Sym('b)),
      Sym('a))

    assert(show(varArgs) == "(lambda (a . b) a)")
  }
}

class HelperTest extends FunSuite {
  test("list") {
    assert(l('a, 'b) == Cons(Sym('a), Cons(Sym('b), nil)))
    assert(l('a, 'b, 'c) == Cons(Sym('a), Cons(Sym('b), Cons(Sym('c), nil))))
  }
}

class BasicTest extends FunSuite {
  test("create symbol") {
    assert(Integer(1).value == 1)
    assert(Sym('dun).value == 'dun)
    assert(nil == nil)

    val sum = BuiltIn { _args: Atom =>
      def sum(args: Atom): Int = args match {
        case Cons(Integer(n), tl) => n + sum(tl)
        case `nil` => 0
      }
      Integer(sum(_args))
    }
  }

  test("predicate") {
    // nil
    assert(nilp(nil))

    // (1 . 2)
    assert(consp(Cons(Integer(1), Integer(2))))

    // (1 2)
    assert(listp(Cons(Integer(1), Integer(2))) == false)

    // ((dun . 1) . 2)
    assert(consp(Cons(Cons('dun, Integer(1)), Integer(2))))
    assert(listp(Cons(Cons('dun, Integer(1)), Integer(2))) == false)
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
    assert(Cons(Cons('c, Integer(100)), nil) == unset(env, 'a))
  }

  test("closure") {
    var env = createEnv()
    var ret: Atom = nil
    var value: Atom = nil

    val add = BuiltIn { _args: Atom =>
      def sum(args: Atom): Int = args match {
        case Cons(Integer(n), tl) => n + sum(tl)
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
}

class EvalTest extends FunSuite {
  test("eval") {
    var env = createEnv()
    var value: Atom = nil
    var ret: Atom = nil
    var str: String = ""
    var exp: Atom = nil
    var fromParser: List[Atom] = List(nil)

    // built-in add
    val binaryAdd = BuiltIn { _ match {
      case Cons(Integer(a), Cons(Integer(b), `nil`)) => Integer(a + b)
    }}
    val binarySub = BuiltIn { _ match {
      case Cons(Integer(a), Cons(Integer(b), `nil`)) => Integer(a - b)
    }}
    val binaryMul = BuiltIn { _ match {
      case Cons(Integer(a), Cons(Integer(b), `nil`)) => Integer(a * b)
    }}

    env = set(env, '+, binaryAdd)
    env = set(env, '-, binarySub)
    env = set(env, '*, binaryMul)

    // make build-in car function
    val builtInCar = BuiltIn { _ match {
      case Cons(hd, tl) => hd match {
        case Cons(hd, tl) => hd
        case _ => nil
      }
      case `nil` => nil
    }}

    val builtInCdr = BuiltIn { _ match {
      case Cons(hd, tl) => cdr(hd)
      case `nil` => nil
    }}

    val builtInCons = BuiltIn { _ match {
      case a@Cons(hd, tl) => Cons(hd, car(tl))
      case `nil` => nil
    }}

    // set built-in functions
    env = set(env, 'car, builtInCar)
    env = set(env, 'cdr, builtInCdr)
    env = set(env, 'cons, builtInCons)

    // (quote a) == a
    str = "(quote a)"
    fromParser = Parser.parse(str)
    exp = l('quote, 'a)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym('a))

    // (quote (1 2 3))
    str = "(quote (1 2 3))"
    fromParser = Parser.parse(str)
    exp = l('quote, l(1, 2, 3))
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == l(1, 2, 3))

    // '(1 2 3)
    str = "'(1 2 3)"
    fromParser = Parser.parse(str)
    exp = l('quote, l(1, 2, 3))
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == l(1, 2, 3))

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

    // (quote (1 2 3))
    exp = l('quote, 'foo)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym('foo))

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

    exp = l(l(l('define, Sym("make-adder2"),
      l('lambda, l('a),
        l('lambda, l('b),
          l('+, 'a, 'b)))), Integer(1)), Integer(2))

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(3))

    str = "(((lambda (a) (lambda (b) (+ a b))) 1) 2)" // == 3
    exp =
      l(
        l(
          l('lambda, l('a),
            l('lambda, l('b),
              l('+, 'a, 'b)
            )),
          1),
        2)

    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(3))

    str = "(car (quote (10 20)))"
    exp = l('car, l('quote, l(10, 20)))

    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(10))

    str = "(cdr (quote (10 20)))"
    exp = l('cdr, l('quote, l(10, 20)))
    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == l(Integer(20)))

    str = "(cdr '(10 20))"
    exp = l('cdr, l(10, 20))
    exp = l('cdr, l('quote, l(10, 20)))

    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == l(Integer(20)))

    exp = Parser.parse("(car '(3 2 1))").head
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(3))

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
    str = "(if t 3 4)"
    exp = l('if, 't, 3, 4)
    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(3))

    str = "(if nil 3 4)"
    exp = l('if, 'nil, 3, 4)
    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    val equal = BuiltIn { args: Atom =>
      if (car(args) == cadr(args)) Sym('t)
      else nil
    }

    str = "(if (= 1 1) 3 4)"
    env = set(env, '=, equal)
    exp = l('if, l('=, 1, 1), 3, 4)
    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(3))

    str = "(if (= 1 2) 3 4)"
    exp = l('if, l('=, 1, 2), 3, 4)
    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(4))

    str = "(* 2 ((lambda (x) (+ x x)) 3))"
    exp = l('*, 2, l(l('lambda, l('x), l('+, 'x, 'x)), 3))
    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(12))

    str = """
    |(define fact
    |  (lambda (x)
    |    (if (= x 0)
    |      1
    |      (* x (fact (- x 1))))))
    """.stripMargin

    exp = l(
      'define,
      'fact,
      l('lambda, l('x),
        l('if, l('=, 'x, 0), 1,
          l('*, 'x, l('fact, l('-, 'x, 1)) ))))

    fromParser = Parser.parse(str)
    assert(exp == fromParser.head)

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Sym('fact))

    exp = l('fact, 5)
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(120))

    str = "(cons (cons 1 2) (cons 3 4))"
    exp = Parser.parse(str).head
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)

    exp = Parser.parse("""
    | (define sum-list
    |   (lambda (xs)
    |     (if xs
    |       (+ (car xs) (sum-list (cdr xs)))
    |       0)))
    """.stripMargin).head

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)

    exp = Parser.parse("(sum-list '(1 2 3))").head
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(6))

    exp = Parser.parse("""
    | (define add
    |   (lambda xs (sum-list xs)))
    """.stripMargin).head

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)

    exp = Parser.parse("""
    | (define sum-list2
    |   (lambda xs
    |     (if xs
    |       (+ (car xs) (sum-list (cdr xs)))
    |       0)))
    """.stripMargin).head

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)

    exp = Parser.parse("(sum-list2 1 2 3)").head
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(6))

    exp = Parser.parse("(add 1 2 3 4 5)").head
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(15))

    exp = Parser.parse("""
    | (define add2
    |   (lambda (x . xs)
    |     (+ (* x x) (sum-list xs))))
    """.stripMargin).head

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)

    exp = Parser.parse("(add2 10 1 2 3 4 5)").head
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(115))

    exp = Parser.parse("""
    | ((define sum-list2
    |   (lambda (xs)
    |     (if xs
    |       (+ (car xs) (sum-list2 (cdr xs)))
    |       0))) '(10 20 30))
    """.stripMargin).head

    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Integer(60))

    exp = Parser.parse("""(define list (lambda xs xs))""".stripMargin).head
    ret = eval(env, exp)
    env = car(ret)

    exp = Parser.parse("""(list 10 20)""".stripMargin).head
    ret = eval(env, exp)
    env = car(ret)
    value = cdr(ret)
    assert(value == Cons(Integer(10), Cons(Integer(20), nil)))
  }
}
