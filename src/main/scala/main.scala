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
      case Pair(Pair(a, b), Pair(c, d)) => showCar(Pair(a, b)) + " " + showCdr(Pair(c, d))
      case Pair(Pair(hd, tl), tail) => showCar(Pair(hd, tl)) + " " + showCar(tail)
      case Pair(hd, tl) => showCdr(hd) + " " + showCdr(tl)
      case _ => showCar(expr)
    }

    def showCar(expr: Atom): String = expr match {
      case Pair(hd, Pair(h, t)) => paren((showCar(hd) + " " + showCdr(Pair(h, t))).trim)
      case Pair(hd, `nil`) => paren(showCar(hd))
      case Pair(hd, tl) => paren(showCar(hd) + " . " + showCdr(tl))
      case Integer(n) => n.toString
      case Symbol(str) => str
      case `nil` => ""
    }

    showCar(expr)
  }

  def sh(value: Atom) = println(show(value))

  // env
  // (nil (foo . 1) (bar . 20)), pair of  list: car is parent
  object Environment {
    def unset(env: Atom, target: Symbol): Atom = env match {
      case Pair(`nil`, tl) => unset(tl, target)
      case Pair(Pair(Symbol(a), _), tl) if Symbol(a) == target => {
        unset(tl, target)
      }
      case Pair(hd, tl) =>  {
        Pair(hd, unset(tl, target))
      }
      case `nil` => nil
    }

    def createEnv(parent: Atom = nil): Atom = Pair(parent, nil)
    def set(env: Atom, name: Symbol, value: Atom): Pair =  ???
  }

  // eval
  import Environment._
  def eval(expr: Atom, env: Atom): Pair = ???
}

object Main extends App {
  // TODO: REPL(read eval print loop)
  // import Sisp._
  // val str = show(Pair(Symbol("daewon"), Symbol("jeong")))
  // println(str)
}
