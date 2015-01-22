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
  case class Closure(env: Atom, name: Symbol, args: Atom, body: Atom) extends Atom

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
      case Pair(hd, Pair(h, t)) => paren((showCar(hd) + " " + showCdr(Pair(h, t))).trim)
      case Pair(hd, `nil`) => paren(showCar(hd))
      case Pair(hd, tl) => paren(showCar(hd) + " . " + showCdr(tl))
      case Integer(n) => n.toString
      case Symbol(str) => str
      case BuiltIn(fn) => showCar(Symbol("built-in"))
      case Closure(_, Symbol(name), _, _) => showCar(Pair(Symbol(name), nil))
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
      val (parent, current) = (car(env), cdr(env))
      Pair(parent, _unset(current))
    }

    def get(env: Atom, symbol: Atom): Atom = {
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
  def eval(env: Atom, expr: Atom): Pair = expr match {
    case Pair(Symbol(n), v@_) if n == "quote" => Pair(env, v)
    case Pair(Symbol(n), Pair(k@Symbol(_), v)) if n == "define" => Pair(set(env, k, v), k)
    case s@Symbol(name) => Pair(env, s)
    case i@Integer(n) => Pair(env, i)
    case Pair(s@Symbol(n), args) => get(env, s) match {
      case BuiltIn(fn) => Pair(env, fn(args))
      case _ => Pair(env, nil)
    }
    case x@_ => {
      println("not match")
      Pair(env, x)
    }
  }
}

object Main extends App {
  // TODO: REPL(read eval print loop)
  // import Sisp._
  // val str = show(Pair(Symbol("daewon"), Symbol("jeong")))
  // println(str)
}
