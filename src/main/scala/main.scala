// reference: http://www.lwh.jp/lisp/

package com.daewon.sisp
import scala.annotation._

object Sisp {
  // basic datatype
  trait Atom
  case object nil extends Atom
  case class Pair(car: Atom, cdr: Atom) extends Atom
  case class Integer(value: Int) extends Atom
  case class Symbol(value: String) extends Atom

  // default functions
  def cons(car: Atom, cdr: Atom): Atom = Pair(car, cdr)
  def car(expr: Atom) = expr match { case Pair(car, _) => car }
  def cdr(expr: Atom) = expr match { case Pair(_, cdr) => cdr }

  // predicate functions

  // nil
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
      case Pair(Pair(hd, tl), tail) => showCar(Pair(hd, tl)) + " " + showCar(tail)
      case Pair(hd, tl) => showCdr(hd) + " " + showCdr(tl)
      case _ => showCar(expr)
    }

    def showCar(expr: Atom): String = expr match {
      case Pair(hd, `nil`) => paren(showCar(hd))
      case Pair(hd, Pair(h, t)) => paren((showCar(hd) + " " + showCdr(Pair(h, t))).trim)
      case Pair(hd, tl) => paren(showCar(hd) + " . " + showCdr(tl) )
      case Integer(n) => n.toString
      case Symbol(str) => str
      case `nil` => ""
    }

    showCar(expr)
  }

  // env
  // ((foo . 1) (bar . 20)), pair of  list
  object Environment {
    def createEnv = nil
    def remove(symbol: Symbol, env: Atom): Atom = env match {
      case Pair(Pair(Symbol(p), value), tl) if Symbol(p) == symbol => tl
      case Pair(hd, tl) => Pair(hd, remove(symbol, tl))
      case `nil` => nil
      case _ => env
    }

    def set(symbol: Symbol, value: Atom, env: Atom): Atom =
      Pair(Pair(symbol, value), remove(symbol, env))

    def get(symbol: Symbol, env: Atom, parent: Atom=nil): Atom = env match {
      case Pair(Pair(p, value), tl) if p == symbol => value
      case Pair(Pair(p, value), tl) => get(symbol, tl)
      case `nil` => nil
    }
  }

  // eval
  import Environment._
  def eval(expr: Atom, env: Atom): (Atom, Atom) = expr match {
    case Pair(Symbol(sym), Pair(Symbol(name), tl)) if sym == "define" =>
      val newEnv = set(Symbol(name), tl, env)
      (Symbol(name), newEnv)

    case Pair(Symbol(name), _)  =>
      (get(Symbol(name), env), env)
    case Symbol(name) => (get(Symbol(name), env), env)
    case `nil` => (nil, env)
    case _ => (expr, expr)
  }
}

object Main extends App {
  // TODO: REPL(read eval print loop)
  // import Sisp._
  // val str = show(Pair(Symbol("daewon"), Symbol("jeong")))
  // println(str)
}
