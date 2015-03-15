// reference: http://www.lwh.jp/lisp/

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

  // built-in functions
  def cons(car: Atom, cdr: Atom): Atom = Cons(car, cdr)
  def car(pair: Atom) = pair match { case Cons(car, _) => car }
  def cdr(pair: Atom) = pair match { case Cons(_, cdr) => cdr }
  def cadr(pair: Atom) = car(cdr(pair))

  // predicate functions
  def nilp(expr: Atom) = expr == nil
  @tailrec def listp(expr: Atom): Boolean = expr match {
    case Cons(_, cdr) => listp(cdr)
    case `nil` => true
    case _ => false
  }

  // `(a . b)
  def pairp(expr: Atom) = expr match {
    case Cons(car, cdr) if (!listp(cdr)) => true
    case Cons(car, `nil`) => false
    case _ => false
  }

  // show
  def show(expr: Atom): String = {
    def paren(s: String) = "(" + s + ")"
    def showCdr(expr: Atom): String = expr match {
      case Cons(a@Cons(_, _), b@Cons(_, _)) => showCar(a) + " " + showCdr(b)
      case Cons(a@Cons(_, _), tail) => showCar(a) + " " + showCar(tail)
      case Cons(hd, tl) => showCdr(hd) + " " + showCdr(tl)
      case _ => showCar(expr)
    }

    // ((a . 10) (b . 20) (c . 30) (d . 40))
    def showCar(expr: Atom): String = expr match {
      case Cons(s@Sym(n), c@Cons(Sym('lambda), _)) => paren(showCar(s) + " . " + showCar(c)) // special case for lambda
      case Cons(Sym('quote), Cons(h, t)) => paren(showCdr(Cons(h, t)).trim)
      case Cons(hd, Cons(h, t)) => paren((showCar(hd) + " " + showCdr(Cons(h, t))).trim)
      case Cons(hd, `nil`) => paren(showCar(hd))
      case Cons(hd, tl) => paren(showCar(hd) + " . " + showCdr(tl))
      case Integer(n) => n.toString
      case Sym(s) => s.toString.drop(1)
      case BuiltIn(fn) => showCar(Sym(Symbol("built-in"))) + " " + fn.toString
      case Closure(_, args, body) => showCar(Cons(Sym('lambda), Cons(args, Cons(body, nil))))
      case `nil` => ""
    }

    showCar(expr)
  }

  def sh(value: Atom) = println(show(value))

  // env
  // (nil (foo . 1) (bar . 20)), pair of  list: car is parent
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

  // env((a . 10) (b . 20)), symbols(a b) => (10 20)
  def mapArgs(env: Atom, args: Atom): Atom = args match {
    case Cons(hd, tl) =>
      val res =  eval(env, hd)
      val newEnv = car(res)
      val value = cdr(res)
      Cons(value, mapArgs(newEnv, tl))
    case `nil` => nil
  }

  def eval(env: Atom, expr: Atom): Cons = expr match {
    case s@Sym('t) => Cons(env, s)
    case s@Sym('nil) => Cons(env, nil)
    case s@Sym(name) => Cons(env, get(env, s))
    case i@Integer(n) => Cons(env, i)
    case c@Closure(env, names, body) => Cons(env, c)
    case b@BuiltIn(fn) => Cons(env, b)
    case Cons(Sym('quote), v@_) => Cons(env, v)
    case Cons(Sym('define), Cons(k@Sym(_), Cons(v, nil))) =>
      val ret = eval(env, v)
      val newEnv = car(ret)
      val value = cdr(ret)
      value match {
        case c@Closure(_, _, _) =>
          val resEnv = set(newEnv, k, c)
          c.env = createEnv(resEnv) // re-assign env to closure
          Cons(resEnv, k)
        case _ => Cons(set(newEnv, k, value), k)
      }

    case Cons(Sym('lambda), tl) =>
      val names = car(tl)
      val body = cadr(tl)
      Cons(env, Closure(createEnv(env), names, body))
    case Cons(Sym('if), Cons(cond, Cons(a, b))) =>
      val ret = eval(env, cond)
      val newEnv = car(ret)
      val value = cdr(ret)
      value match {
        case Sym('t) => eval(newEnv, a)
        case _ => eval(newEnv, car(b))
      }
    case _ =>
      val res = eval(env, car(expr))
      val newEnv = car(res)
      val head = cdr(res)
      val args = cdr(expr)

      val ret = head match {
        case BuiltIn(fn) => fn(mapArgs(env, args))
        case Closure(ev, names, body) =>
          cdr(eval(bindEnv(ev, names, mapArgs(env, args)), body))
      }

      Cons(env, ret)
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
      def toCons = makeCons(ls)
    }
  }

  class LispParser extends JavaTokenParsers  {
    import Helpers._ // for implicit conversion

    type A = Atom
    def expr: Parser[A] = "nil" ^^^ nil | quote | pair | factor
    def quote: Parser[A] = "`" ~> pair ^^ { case p => Cons('quote, p) }
    def pair: Parser[A] = "(" ~> rep(factor | expr) <~ ")" ^^ (_.toCons)
    def factor: Parser[A] = number | symbol
    def symbol: Parser[A] = "[!@#$%^&_=\\*\\-\\+\\?\\.a-zA-Z]+[0-9]*".r ^^ { case s => Sym(s) }
    def number: Parser[A] = wholeNumber ^^ (_.toInt)
  }

  object Parser extends LispParser {
    def parse(input: String) = parseAll(expr, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }
}

object Main extends App {
  // TODO: REPL(read eval print loop)
}
