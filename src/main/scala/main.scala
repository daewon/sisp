// reference: http://www.lwh.jp/lisp/

package com.daewon.sisp
import scala.annotation._
import scala.util._

object Sisp {
  // basic datatype
  trait Atom
  case object nil extends Atom
  case class Pair(car: Atom, cdr: Atom) extends Atom
  case class Integer(value: Int) extends Atom
  case class Symbol(value: String) extends Atom
  case class BuiltIn(val call: Atom => Atom) extends Atom
  case class Closure(env: Atom, args: Atom, body: Atom) extends Atom

  // built-in functions
  def cons(car: Atom, cdr: Atom): Atom = Pair(car, cdr)
  def car(pair: Atom) = pair match { case Pair(car, _) => car }
  def cdr(pair: Atom) = pair match { case Pair(_, cdr) => cdr }
  def cadr(pair: Atom) = car(cdr(pair))
  def list(args: Atom*): Atom =
    if (args.isEmpty || args.head == nil) nil
    else Pair(args.head, list(args.tail:_*))

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
      case Pair(s@Symbol(n), c@Pair(Symbol("lambda"), _)) => paren(showCar(s) + " . " + showCar(c))
      case Pair(hd, Pair(h, t)) => paren((showCar(hd) + " " + showCdr(Pair(h, t))).trim)
      case Pair(hd, `nil`) => paren(showCar(hd))
      case Pair(hd, tl) => paren(showCar(hd) + " . " + showCdr(tl))
      case Integer(n) => n.toString
      case Symbol(str) => str
      case BuiltIn(fn) => showCar(Symbol("built-in")) + " " + fn.toString
      case Closure(_, args, body) => showCar(Pair(Symbol("lambda"), Pair(args, Pair(body, nil))))
      case `nil` => ""
    }

    showCar(expr)
  }

  def sh(value: Atom) = println(show(value))

  // env
  // (nil (foo . 1) (bar . 20)), pair of  list: car is parent
  object Environment {
    def createEnv(parent: Atom = nil): Atom = Pair(parent, nil)
    def set(env: Atom, key: Symbol, value: Atom): Pair = {
      val (parent, _) = (car(env), cdr(env))
      Pair(parent, Pair(Pair(key, value), unset(env, key)))
    }

    def unset(env: Atom, target: Symbol): Atom = {
      def _unset(env: Atom): Atom = env match {
        case Pair(Pair(s@Symbol(_), _), tl) if s == target => _unset(tl)
        case Pair(hd, tl) => Pair(hd, _unset(tl))
        case `nil` => nil
      }
      val (_, current) = (car(env), cdr(env))
      _unset(current)
    }

    def get(env: Atom, symbol: Symbol): Atom = {
      @tailrec def _find(lst: Atom): Atom = lst match {
        case Pair(Pair(s@Symbol(_), v), tl) if s == symbol => v
        case Pair(_, tl) => _find(tl)
        case `nil` => nil
      }

      val (parent, current) = (car(env), cdr(env))
      val res = _find(current)

      if (res != nil) res // TODO:: throw
      else _find(parent)
    }
  }

  // eval
  import Environment._
  def bindEnv(env: Atom, names: Atom, values: Atom): Atom = names match {
    case Pair(s@Symbol(n), tail) => bindEnv(set(env, s, car(values)), tail, cdr(values))
    case `nil` => env
  }

  def mapArgs(env: Atom, args: Atom): Atom = args match {
    case Pair(hd, tl) => Pair(cdr(eval(env, hd)), mapArgs(env, tl))
    case `nil` => nil
  }

  def eval(env: Atom, expr: Atom): Pair = expr match {
    case s@Symbol(name) => Pair(env, get(env, s))
    case i@Integer(n) => Pair(env, i)
    case Pair(Symbol("quote"), v@_) => Pair(env, v)
    case Pair(Symbol("define"), Pair(k@Symbol(_), Pair(v, `nil`))) =>
      val ret = eval(env, v)
      val newEnv = car(ret)
      val value = cdr(ret)
      Pair(set(newEnv, k, value), k)
    case Pair(Symbol("lambda"), tl) => tl match {
      case Pair(args@Pair(_, nil), Pair(body, `nil`)) => Pair(env, Closure(env, args, body))
    }
    case Pair(s@Symbol(n), args) => get(env, s) match { // Apply
      case BuiltIn(fn) => Pair(env, fn(mapArgs(env, args)))
      case Closure(env, names, body) => eval(bindEnv(env, names, args), body)
    }
    case Pair(Closure(env, names, body), args) =>
      // (lambda (a) (lambda (b) (+ a b))) 1)
      eval(bindEnv(env, names, args), body)
    case _ =>
      // (((lambda (a) (lambda (b) (+ a b))) 1) 2) == 3
      val fn = car(expr)
      val args = cdr(expr)
      val ret = eval(env, fn)
      eval(car(ret), Pair(cdr(ret), args))
  }
}

object Main extends App {
  // TODO: REPL(read eval print loop)
  // import Sisp._
  // val str = show(Pair(Symbol("daewon"), Symbol("jeong")))
  // println(str)
}
