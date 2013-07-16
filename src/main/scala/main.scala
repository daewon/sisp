package com.sisp

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
  var symbolTable: Atom = nil

  trait Atom
  case object nil extends Atom
  case class Pair(car: Atom, cdr: Atom) extends Atom
  case class Symbol(value: String) extends Atom
  object Symbol {
    def apply(value: String, env: Atom): Atom = {
      val symbol = Symbol(value)
      symbolTable = Pair(symbol, symbolTable)
      symbol
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

  def printExpr(expr: Atom): Unit = expr match {
    case Pair(car, cdr) => {
      print("( ")
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
            print(" )")
            return
          }
        }
      }

      print(" )")
    }
    case Integer(n) => print(n)
    case Symbol(str) => print(str)
    case _ if nilp(expr) => print(" NIL")
  }
}

object main extends App {
  import Sisp._

  val t = cons(Integer(1), cons(Integer(2), nil))
  printExpr(t)
  println()
  // println(listp(t))
  // println(listp(Integer(10)))

  printExpr(cons(Symbol("X", symbolTable), Symbol("Y", symbolTable)))
  // printExpr(symbolTable)
  
}

