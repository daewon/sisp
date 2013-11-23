package com.daewon.sisp

object Sisp {
  object Error extends Enumeration {
    type Error = Value
    val OK, Syntax, Unbound, Argument, Type = Value
  }

  object Lexer {
  }

  object Parser {
  }

  // app status
  trait Atom
  case object nil extends Atom
  case class Pair(car: Atom, cdr: Atom) extends Atom
  class Symbol(val value: String) extends Atom {
    override def toString(): String = value
  }
  object Symbol {
    var table: Atom = nil
    def apply(value: String, env: Atom = nil): Atom = {
      val old = find(value)
      if (nilp(old)) {
        table = cons(new Symbol(value), table)
        car(table)
      } else {
        old
      }
    }
    def unapply(s: Symbol): Option[String] = Some(s.value)
    private def find(v: String): Atom = {
      def _find(lst: Atom): Atom = lst match {
        case Symbol(s) if s == v => car(lst)
        case `nil` => nil
        case _ => _find(cdr(lst))
      }
      _find(table)
    }
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
    case _ if nilp(expr) => true
    case _ => false
  }

  def show(expr: Atom) {
    def printExpr(expr: Atom): Unit = expr match {
      case Pair(car, cdr) => {
        print("(")
        printExpr(car)
        var atom = cdr
        while (!nilp(atom)) {
          atom match {
            case Pair(car, cdr) => {
              print(" ")
              printExpr(car)
              atom = cdr
            }
            case _ => {
              print(" . ")
              printExpr(atom)
              print(")")
              return
            }
          }
        }
        print(")")
      }
      case Integer(n) => print(n)
      case Symbol(str) => print(str)
      case _ if nilp(expr) => print(" NIL")
    }

    printExpr(expr)
    println("")
  }
}