// reference: http://www.lwh.jp/lisp/

package com.daewon.sisp

object Sisp {
  object Error extends Enumeration {
    type Error = Value
    val OK, Syntax, Unbound, Argument, Type = Value
  }

  // app status
  trait Atom
  case object nil extends Atom
  case class Pair(car: Atom, cdr: Atom) extends Atom
  class Symbol(val value: String) extends Atom {
    override def toString: String = value
  }
  object Symbol {
    var table: Atom = nil
    private def find(v: String): Atom = {
      def _find(lst: Atom): Atom = lst match {
        case Pair(Symbol(s), _) if s == v => car(lst)
        case `nil` => nil
        case _ => _find(cdr(lst))
      }
      _find(table)
    }

    def apply(value: String, env: Atom = nil): Symbol = {
      val old = find(value)
      if (nilp(old)) {
        table = cons(new Symbol(value), table)
        car(table).asInstanceOf[Symbol]
      } else {
        old.asInstanceOf[Symbol]
      }
    }

    def unapply(s: Symbol): Option[String] = Some(s.value)
  }

  case class Builtin[T]() extends Atom
  case class Integer(n: Int) extends Atom

  // default function
  def cons(car: Atom, cdr: Atom): Atom = Pair(car, cdr)
  def car(value: Atom) = value match { case Pair(car, _) => car }
  def cdr(value: Atom) = value match { case Pair(_, cdr) => cdr }
  def nilp(value: Atom) = value == nil
  def listp(expr: Atom): Boolean = expr match {
    case Pair(car, cdr) => listp(cdr)
    case `nil` => true
    case _ => false
  }

  def show(expr: Atom): String = {
    var out = ""
    def printExpr(expr: Atom): Unit = expr match {
      case Pair(car, cdr) => {
        out += "("
        printExpr(car)
        var atom = cdr
        while (!nilp(atom)) {
          atom match {
            case Pair(car, cdr) => {
              out += " "
              printExpr(car)
              atom = cdr
            }
            case _ => {
              out += " . "
              printExpr(atom)
              out += ")"
              return
            }
          }
        }
        out += ")"
      }
      case Integer(n) =>  out += n
      case Symbol(str) => out += str
      case `nil` => out += " NIL"
    }
    printExpr(expr)
    out
  }
}

object Main extends App {
  import Sisp._
  show( Pair(Symbol("daewon"), Symbol("jeong")) )
}
