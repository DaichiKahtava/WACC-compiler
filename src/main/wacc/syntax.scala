package wacc

import parsley.generic
import parsley.token.errors._

// Types
sealed trait Type
sealed trait PairElemType

case class AnyT() extends Type
case class AnyPet() extends PairElemType

case class IntT() extends Type with PairElemType
case class BoolT() extends Type with PairElemType
case class CharT() extends Type with PairElemType
case class StringT() extends Type with PairElemType
case class ArrayT(tp: Type) extends Type with PairElemType

case class Pair(pe1: PairElemType, pe2: PairElemType) extends Type

case class ErasedPair() extends PairElemType


// Expressions
sealed trait UnOp extends Expr
case class Not(x: Expr) extends UnOp
case class Neg(x: Expr) extends UnOp
case class Len(x: Expr) extends UnOp
case class Ord(x: Expr) extends UnOp
case class Chr(x: Expr) extends UnOp

sealed trait  BinOp extends Expr
case class Mul(x: Expr, y: Expr) extends BinOp
case class Div(x: Expr, y: Expr) extends BinOp
case class Mod(x: Expr, y: Expr) extends BinOp
case class Add(x: Expr, y: Expr) extends BinOp
case class Minus(x: Expr, y: Expr) extends BinOp
case class GrT(x: Expr, y: Expr) extends BinOp
case class GrEqT(x: Expr, y: Expr) extends BinOp
case class LsT(x: Expr, y: Expr) extends BinOp
case class LsEqT(x: Expr, y: Expr) extends BinOp
case class Eq(x: Expr, y: Expr) extends BinOp
case class NEq(x: Expr, y: Expr) extends BinOp
case class And(x: Expr, y: Expr) extends BinOp
case class Or(x: Expr, y: Expr) extends BinOp

sealed trait Expr
case class UnExpr(op: UnOp, x: Expr) extends Expr
case class BinExpr(x1: Expr, op: BinOp, x2: Expr) extends Expr

sealed trait Atom extends Expr
case class IntL(n: Int) extends Atom
case class BoolL(b: Boolean) extends Atom
case class CharL(c: Char) extends Atom
case class StrL(s: String) extends Atom
case class PairL() extends Atom
case class Ident(id: String) extends Atom
case class ArrElem(id: String, xs: List[Expr]) extends Atom with LValue

// Statements
case class Program(funcs: List[Func], stmt: Stmt)
case class Func(tp: Type, id: String, params: List[Param], stmt: Stmt)
case class Param(tp: Type, id: String)

sealed trait Stmt
case class Skip() extends Stmt
case class Decl(tp: Type, id: String, rv: RValue) extends Stmt
case class Asgn(lv: LValue, rv: RValue) extends Stmt
case class Read(lv: LValue) extends Stmt
case class Free(x: Expr) extends Stmt
case class Return(x: Expr) extends Stmt
case class Exit(x: Expr) extends Stmt
case class Print(x: Expr) extends Stmt // May be able to combine print and println
case class Println(x: Expr) extends Stmt
case class Cond(x: Expr, s1: Stmt, s2: Stmt) extends Stmt
case class Loop(x: Expr, stmt: Stmt) extends Stmt
case class Body(stmt: Stmt) extends Stmt
case class Delimit(s1: Stmt, s2: Stmt) extends Stmt

sealed trait LValue
case class LIdent(id: String) extends LValue

sealed trait RValue
case class RExpr(x: Expr) extends RValue
case class ArrL(xs: List[Expr]) extends RValue
case class NewPair(x1: Expr, x2: Expr) extends RValue
case class Call(id: String, xs: List[Expr]) extends RValue

sealed trait PairElem extends RValue with LValue
case class First(lv: LValue) extends PairElem
case class Second(lv: LValue) extends PairElem
// TODO [for parser]: RArrL and Call's List[Expr] have different min # of elements



// Example bridges from PPT

// object Prog extends generic.ParserBridge2[List[Asgn], Expr, Prog]
// object Asgn extends generic.ParserBridge2[String, Expr, Asgn] {
//     override def labels: List[String] = List("assignment")
// }

// object Add extends generic.ParserBridge2[Expr, Expr, Expr]
// object Mul extends generic.ParserBridge2[Expr, Expr, Expr]
// object Val extends generic.ParserBridge1[BigInt, Expr]
// object Var extends generic.ParserBridge1[String, Expr]
