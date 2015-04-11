// reference: http://www.lwh.jp/lisp/

// improper list
// http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_93.html
// http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Lists.html

package com.daewon.sisp

import scala.annotation._
import scala.util._
import scala.util.parsing.combinator._

object Sisp {
  // basic data types
  trait Atom
  case object nil extends Atom
  case class Cons(car: Atom, cdr: Atom) extends Atom
  case class Integer(value: Int) extends Atom
  case class Sym(value: Symbol) extends Atom
  case object Sym { def apply(str: String) = new Sym(Symbol(str)) }
  case class BuiltIn(val call: Atom => Atom) extends Atom
  case class Closure(var env: Atom, args: Atom, body: Atom) extends Atom
  case class Macro(var env: Atom, args: Atom, body: Atom) extends Atom

  // built-in functions
  def cons(car: Atom, cdr: Atom): Atom = Cons(car, cdr)
  def car(cons: Atom) = cons match { case Cons(car, _) => car }
  def cdr(cons: Atom) = cons match { case Cons(_, cdr) => cdr }
  def cadr(cons: Atom) = car(cdr(cons))

  // predicate functions
  def nilp(expr: Atom) = expr == nil
  @tailrec def listp(expr: Atom): Boolean = expr match {
    case Cons(_, cdr) => listp(cdr)
    case `nil` => true
    case _ => false
  }

  def symp(expr: Atom) = expr match {
    case Sym(a) => true
    case _ => false
  }

  // `(a . b)
  def consp(expr: Atom) = expr match {
    case Cons(car, `nil`) => false
    case Cons(car, cdr) if (!listp(cdr)) => true
    case _ => false
  }

  // show
  def show(expr: Atom): String = {
    def paren(s: String) = s"($s)"

    def showCdr(expr: Atom): String = expr match {
      case a@Cons(hd, tl) if consp(a) && !consp(tl) => showCar(hd) + " . "  + showCar(tl)
      case Cons(hd, tl) => showCar(hd) + " " + showCdr(tl)
      case _ => showCar(expr)
    }

    // ((a . 10) (b . 20) (c . 30) (d . 40))
    def showCar(expr: Atom): String = expr match {
      case Cons(Sym('quote), a@Cons(h, _)) => showCar(h)
      case a@Cons(hd, tl) if consp(a) && !consp(tl) => paren( showCar(hd) + " . "  + showCar(tl))
      case Cons(hd, `nil`) => paren( showCar(hd) )
      case Cons(hd, tl) => paren( (showCar(hd) + " " + showCdr(tl)).trim )
      case Integer(n) => n.toString
      case Sym(s) => s.toString.drop(1)
      case BuiltIn(fn) => showCar(Sym(Symbol("built-in"))) + " " + fn.toString
      case Closure(_, args, body) => showCar(Cons(Sym('lambda), Cons(args, Cons(body, nil))))
      case Macro(_, args, body) => showCar(Cons(Sym('Macro), Cons(args, Cons(body, nil))))
      case `nil` => ""
    }

    showCar(expr)
  }

  def sh(value: Atom) = println(show(value))

  // env
  // (nil (foo . 1) (bar . 20)), cons of  list: car is parent
  object Environment {
    def createEnv(parent: Atom = nil): Atom = Cons(parent, nil)
    def set(env: Atom, key: Sym, value: Atom): Cons = {
      val (parent, current) = (car(env), cdr(env))
      Cons(parent, Cons(Cons(key, value), unset(env, key)))
    }

    def unset(env: Atom, target: Sym): Atom = {
      def _unset(env: Atom): Atom = env match {
        case Cons(Cons(s@Sym(_), _), tl) if s == target => _unset(tl)
        case Cons(hd, tl) => Cons(hd, _unset(tl))
        case `nil` => nil
      }
      val (_, current) = (car(env), cdr(env))
      _unset(current)
    }

    def get(env: Atom, symbol: Sym): Atom = {
      @tailrec def find(lst: Atom): Atom = lst match {
        case Cons(Cons(s@Sym(_), v), tl) if s == symbol => v
        case Cons(_, tl) => find(tl)
        case `nil` => nil
      }

      if (env == nil) nil
      else {
        val res = find(cdr(env))
        if (res != nil) res
        else get(car(env), symbol)
      }
    }
  }

  // eval
  import Environment._
  // set env with ((a b) (10 20)) => ((a . 10) (b . 20))
  def bindEnv(env: Atom, names: Atom, values: Atom): Atom = names match {
    case Cons(s@Sym(_), tail) => bindEnv(set(env, s, car(values)), tail, cdr(values))
    case `nil` => env
  }

  // 실 인자 값에 있는 변수를 환경으로 부터 찾아서 실제로 맵핑
  // env((a . 10) (b . 20)), symbols(a b) => (10 20)
  def mapArgs(env: Atom, args: Atom): Atom = args match {
    case Cons(hd, tl) =>
      val res =  eval(env, hd)
      val newEnv = car(res)
      val value = cdr(res)
      Cons(value, mapArgs(newEnv, tl))
    case `nil` => nil
  }

  // (a . b) => (a b)
  def impToCons(imp: Atom): Atom = imp match {
    case Cons(hd, tl) => Cons(hd, impToCons(tl))
    case a => Cons(a, nil)
  }

  // (a b . c), (1 2 3 4 5) => (1 2 (3 4 5))
  def mapArgsImp(names: Atom, args: Atom): Atom = names match {
    case Cons(hd, `nil`) => Cons(args, nil)
    case Cons(hd, tl) => Cons(car(args), mapArgsImp(tl, cdr(args)))
    case `nil` =>  nil
  }

  def eval(env: Atom, program: List[Atom]): Cons = {
    program.foldLeft(Cons(env, nil)) { (acc, curr) =>
      val (env, value) = (car(acc), cdr(acc))
      eval(env, curr)
    }
  }

  def eval(env: Atom, expr: Atom): Cons = expr match {
    case s@Sym('t) => Cons(env, s)
    case s@Sym('nil) => Cons(env, nil)
    case s@Sym(name) => Cons(env, get(env, s))
    case i@Integer(n) => Cons(env, i)
    case c@Closure(env, names, body) => Cons(env, c)
    case m@Macro(env, names, body) => Cons(env, m)
    case b@BuiltIn(fn) => Cons(env, b)
    case Cons(Sym('quote), Cons(v@_, nil)) => Cons(env, v)
    case Cons(Sym('define), Cons(k@Sym(_), Cons(v, nil))) =>
      val ret = eval(env, v)
      val newEnv = car(ret)
      val value = cdr(ret)
      value match {
        case c@Closure(_, _, _) =>
          val resEnv = set(newEnv, k, c)
          c.env = createEnv(resEnv) // re-assign env to closure
          Cons(resEnv, k)
        case c@Macro(_, _, _) =>
          val resEnv = set(newEnv, k, c)
          c.env = createEnv(resEnv) // re-assign env to closure
          Cons(resEnv, k)
        case _ => Cons(set(newEnv, k, value), k)
      }
    case Cons(Sym('lambda), tl) =>
      val paramNames = car(tl)
      val body = cadr(tl)
      Cons(env, Closure(createEnv(env), paramNames, body))

    case Cons(Sym('defmacro), tl) =>
      val paramNames = car(tl)
      val name = car(paramNames).asInstanceOf[Sym]
      val params = cdr(paramNames)
      val body = cadr(tl)
      val exp = Cons(Sym('define), Cons(name, Cons(Macro(createEnv(env), params, body), nil)))
      eval(env, exp)

    case Cons(Sym('if), Cons(cond, Cons(a, b))) =>
      val ret = eval(env, cond)
      val newEnv = car(ret)
      val value = cdr(ret)
      value match {
        case `nil` => eval(newEnv, car(b))
        case _ => eval(newEnv, a)
      }

    case _ =>
      val res = eval(env, car(expr))
      val newEnv = car(res)
      val head = cdr(res)
      val args = cdr(expr)

      val ret = head match {
        case a@Sym(_) => cdr(eval(newEnv, Cons(a, args)))
        case BuiltIn(fn) => fn(mapArgs(newEnv, args))
        case Closure(ev, names, body) =>
          names match {
            case a@Sym(_) =>
              val newNames = Cons(a, nil)
              val newArgs = Cons(mapArgs(newEnv, args), nil)
              cdr(eval(bindEnv(ev, newNames, newArgs), body))
            case Cons(hd, tl) if consp(names) =>
              val newNames = impToCons(names)
              val newArgs = mapArgsImp(newNames, mapArgs(newEnv, args))
              cdr(eval(bindEnv(ev, newNames, newArgs), body))
            case _ =>
              cdr(eval(bindEnv(ev, names, mapArgs(newEnv, args)), body))
          }
        case Macro(ev, names, body) =>
          val expended = cdr(eval(bindEnv(ev, names, args), body))
          cdr(eval(ev, expended))
      }
      Cons(newEnv, ret)
  }

  // helper for test
  object Helpers {
    implicit def toInteger(n: Int): Integer = Integer(n)
    implicit def toSym(s: Symbol): Sym = Sym(s)
    implicit def toCons[T <% Atom](a: Tuple2[T, T]): Cons = Cons(a._1, a._2)

    def l(args: Atom*): Atom = // helper function for test
      if (args.isEmpty || args.head == nil) nil
      else Cons(args.head, l(args.tail:_*))

    implicit class ListToCons[T <: Atom](ls: List[T]) {
      private def makeCons(ls: List[T]): Atom = ls match {
        case h :: tl => Cons(h, makeCons(tl))
        case Nil => nil
      }
      private def makeImproperCons(ls: List[T], last: T): Atom = ls match {
        case h :: tl => Cons(h, makeImproperCons(tl, last))
        case Nil => last
      }

      def toCons = makeCons(ls)
      def toImproperCons(last: T) = makeImproperCons(ls, last)
    }
  }

  class LispParser extends JavaTokenParsers  {
    import Helpers._ // for implicit conversion

    // "((1 . 2) 3 . 4)"
    type A = Atom
    def program: Parser[List[A]] = rep(expr)
    def expr: Parser[A] = "nil" ^^^ nil | quote | cons | impCons | factor
    def quote: Parser[A] = ("`" | "'") ~> expr ^^ {
      case p => Cons('quote, Cons(p, nil))
    }
    def cons: Parser[A] = "(" ~> rep(factor | expr) <~ ")" ^^ (_.toCons)
    def impCons: Parser[A] = ("(" ~> rep(expr)) ~ "." ~ (expr <~ ")") ^^ {
      case a ~ _ ~ c => a.toImproperCons(c)
    }
    def factor: Parser[A] = number | symbol
    def symbol: Parser[A] = "[!@#$%^&_=\\*\\-\\+\\?a-zA-Z]+[0-9]*".r ^^ { case s => Sym(s) }
    def number: Parser[A] = wholeNumber ^^ (_.toInt)
  }

  object Parser extends LispParser {
    def parse(input: String): List[A] = parseAll(program, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }
}

object Main extends App {
  // TODO: REPL(read eval print loop)
}
