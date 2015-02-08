// reference: http://www.lwh.jp/lisp/

package com.daewon.sisp
import scala.annotation._
import scala.util._

object Sisp {
  // basic data types
  trait Atom
  case object nil extends Atom
  case class Pair(car: Atom, cdr: Atom) extends Atom
  case class Integer(value: Int) extends Atom
  case class Sym(value: Symbol) extends Atom
  case class BuiltIn(val call: Atom => Atom) extends Atom
  case class Closure(var env: Atom, args: Atom, body: Atom) extends Atom

  // built-in functions
  def cons(car: Atom, cdr: Atom): Atom = Pair(car, cdr)
  def car(pair: Atom) = pair match { case Pair(car, _) => car }
  def cdr(pair: Atom) = pair match { case Pair(_, cdr) => cdr }
  def cadr(pair: Atom) = car(cdr(pair))
  def l(args: Atom*): Atom = // helper function for test
    if (args.isEmpty || args.head == nil) nil
    else Pair(args.head, l(args.tail:_*))

  // predicate functions
  def nilp(expr: Atom) = expr == nil
  @tailrec def listp(expr: Atom): Boolean = expr match {
    case Pair(_, cdr) => listp(cdr)
    case `nil` => true
    case _ => false
  }

  // `(a . b)
  def pairp(expr: Atom) = expr match {
    case Pair(car, cdr) if (!listp(cdr)) => true
    case Pair(car, `nil`) => false
    case _ => false
  }

  // show
  def show(expr: Atom): String = {
    def paren(s: String) = "(" + s + ")"
    def showCdr(expr: Atom): String = expr match {
      case Pair(a@Pair(_, _), b@Pair(_, _)) => showCar(a) + " " + showCdr(b)
      case Pair(a@Pair(_, _), tail) => showCar(a) + " " + showCar(tail)
      case Pair(hd, tl) => showCdr(hd) + " " + showCdr(tl)
      case _ => showCar(expr)
    }

    // ((a . 10) (b . 20) (c . 30) (d . 40))
    def showCar(expr: Atom): String = expr match {
      // special case for lambda
      case Pair(s@Sym(n), c@Pair(Sym('lambda), _)) => paren(showCar(s) + " . " + showCar(c))
      case Pair(hd, Pair(h, t)) => paren((showCar(hd) + " " + showCdr(Pair(h, t))).trim)
      case Pair(hd, `nil`) => paren(showCar(hd))
      case Pair(hd, tl) => paren(showCar(hd) + " . " + showCdr(tl))
      case Integer(n) => n.toString
      case Sym(s) => s.toString.drop(1)
      case BuiltIn(fn) => showCar(Sym(Symbol("built-in"))) + " " + fn.toString
      case Closure(_, args, body) => showCar(Pair(Sym('lambda), Pair(args, Pair(body, nil))))
      case `nil` => ""
    }

    showCar(expr)
  }

  def sh(value: Atom) = println(show(value))

  // env
  // (nil (foo . 1) (bar . 20)), pair of  list: car is parent
  object Environment {
    def createEnv(parent: Atom = nil): Atom = Pair(parent, nil)
    def set(env: Atom, key: Sym, value: Atom): Pair = {
      val (parent, current) = (car(env), cdr(env))
      Pair(parent, Pair(Pair(key, value), unset(env, key)))
    }

    def unset(env: Atom, target: Sym): Atom = {
      def _unset(env: Atom): Atom = env match {
        case Pair(Pair(s@Sym(_), _), tl) if s == target => _unset(tl)
        case Pair(hd, tl) => Pair(hd, _unset(tl))
        case `nil` => nil
      }
      val (_, current) = (car(env), cdr(env))
      _unset(current)
    }

    def get(env: Atom, symbol: Sym): Atom = {
      @tailrec def find(lst: Atom): Atom = lst match {
        case Pair(Pair(s@Sym(_), v), tl) if s == symbol => v
        case Pair(_, tl) => find(tl)
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
    case Pair(s@Sym(_), tail) => bindEnv(set(env, s, car(values)), tail, cdr(values))
    case `nil` => env
  }

  // env((a . 10) (b . 20)), symbols(a b) => (10 20)
  def mapArgs(env: Atom, args: Atom): Atom = args match {
    case Pair(hd, tl) =>
      val res =  eval(env, hd)
      val newEnv = car(res)
      val value = cdr(res)
      Pair(value, mapArgs(newEnv, tl))
    case `nil` => nil
  }

  def eval(env: Atom, expr: Atom): Pair = expr match {
    case s@Sym('t) => Pair(env, s)
    case s@Sym('nil) => Pair(env, nil)
    case s@Sym(name) => Pair(env, get(env, s))
    case i@Integer(n) => Pair(env, i)
    case c@Closure(env, names, body) => Pair(env, c)
    case b@BuiltIn(fn) => Pair(env, b)
    case Pair(Sym('quote), v@_) => Pair(env, v)
    case Pair(Sym('define), Pair(k@Sym(_), Pair(v, nil))) =>
      val ret = eval(env, v)
      val newEnv = car(ret)
      val value = cdr(ret)

      if (value.isInstanceOf[Closure]) {
        val resEnv = set(newEnv, k, value)
        val cls = get(resEnv, k).asInstanceOf[Closure]
        cls.env = createEnv(resEnv)
        Pair(resEnv, k)
      }
      else {
        Pair(set(newEnv, k, value), k)
      }

    case Pair(Sym('lambda), tl) =>
      val names = car(tl)
      val body = cadr(tl)
      Pair(env, Closure(createEnv(env), names, body))
    case Pair(Sym('if), Pair(cond, Pair(a, b))) =>
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

      Pair(env, ret)
  }

  // helper for test
  object Helpers {
    implicit def toInteger(n: Int): Integer = Integer(n)
    implicit def toSymbol(s: Symbol): Sym = Sym(s)
    implicit def toPair[T <% Atom](a: Tuple2[T, T]): Pair = Pair(a._1, a._2)
  }
}

object Main extends App {
  // TODO: REPL(read eval print loop)
}
