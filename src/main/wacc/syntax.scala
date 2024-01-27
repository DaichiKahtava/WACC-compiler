import parsley.generic
import parsley.token.errors._

sealed trait Type


sealed trait UnOp
case class Not() extends UnOp
sealed trait  BinOp
case class Add() extends BinOp

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

case class Progam(funcs: List[Func], s: Stmt)
case class Func(tp: Type, id: String, params: List[Param], s: Stmt)
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
case class Loop(x: Expr, s: Stmt)
case class Body(s: Stmt)
case class Delimit(s1: Stmt, s2: Stmt) extends Stmt

sealed trait LValue
case class LIdent(id: String) extends LValue
case class ArrElem(id: String, xs: List[Expr]) extends LValue
case class LPairElem(e: RPairElem) extends LValue

sealed trait RValue
// case class RExpr(x: Expr) extends RValue
// case class RArrL(arrL: ArrL) extends RValue
// case class NewPair(x1: Expr, x2: Expr) extends RValue
case class RPairElem(e: LPairElem) extends RValue
// case class Call(id: String, xs: List[Expr])



// object Prog extends generic.ParserBridge2[List[Asgn], Expr, Prog]
// object Asgn extends generic.ParserBridge2[String, Expr, Asgn] {
//     override def labels: List[String] = List("assignment")
// }

// object Add extends generic.ParserBridge2[Expr, Expr, Expr]
// object Mul extends generic.ParserBridge2[Expr, Expr, Expr]
// object Val extends generic.ParserBridge1[BigInt, Expr]
// object Var extends generic.ParserBridge1[String, Expr]
