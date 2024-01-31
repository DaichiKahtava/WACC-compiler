package wacc

import parsley.generic._
import parsley.token.errors._

// Types
sealed trait Type
sealed trait PairElemType

// TODO: Implement inheriting from 2 bridges
case object IntT extends Type with PairElemType
case object BoolT extends Type with PairElemType
case object CharT extends Type with PairElemType
case object StringT extends Type with PairElemType
case class ArrayT(tp: Type) extends Type with PairElemType

case class Pair(pe1: PairElemType, pe2: PairElemType) extends Type

case object ErasedPair extends PairElemType with ParserBridge0[PairElemType]


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

sealed trait Expr extends RValue
case class UnExpr(op: UnOp, x: Expr) extends Expr
case class BinExpr(x1: Expr, op: BinOp, x2: Expr) extends Expr

sealed trait Atom extends Expr
case class IntL(n: Int) extends Atom
case class BoolL(b: Boolean) extends Atom
case class CharL(c: Char) extends Atom
case class StrL(s: String) extends Atom
case object PairL extends Atom with ParserBridge0[Atom]
case class Ident(id: String) extends Atom
case class ArrElem(id: String, xs: List[Expr]) extends Atom with LValue

// Statements
case class Program(funcs: List[Func], s: Stmt)
case class Func(tp: Type, id: String, params: List[Param], s: Stmt)
case class Param(tp: Type, id: String)

sealed trait Stmt
case object Skip extends Stmt with ParserBridge0[Stmt]
case class Decl(tp: Type, id: String, rv: RValue) extends Stmt
case class Asgn(lv: LValue, rv: RValue) extends Stmt
case class Read(lv: LValue) extends Stmt
case class Free(x: Expr) extends Stmt
case class Return(x: Expr) extends Stmt
case class Exit(x: Expr) extends Stmt
case class Print(x: Expr) extends Stmt // May be able to combine print and println
case class Println(x: Expr) extends Stmt
case class Cond(x: Expr, s1: Stmt, s2: Stmt) extends Stmt
case class Loop(x: Expr, s: Stmt) extends Stmt
case class Body(s: Stmt) extends Stmt
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


/// Companion objects for each AST node ///

// Types
object Pair extends ParserBridge2[PairElemType, PairElemType, Type]

// Expressions
object Not extends ParserBridge1[Expr, UnOp]
object Neg extends ParserBridge1[Expr, UnOp]
object Len extends ParserBridge1[Expr, UnOp]
object Ord extends ParserBridge1[Expr, UnOp]
object Chr extends ParserBridge1[Expr, UnOp]

object Mul extends ParserBridge2[Expr, Expr, BinOp]
object Div extends ParserBridge2[Expr, Expr, BinOp]
object Mod extends ParserBridge2[Expr, Expr, BinOp]
object Add extends ParserBridge2[Expr, Expr, BinOp]
object Minus extends ParserBridge2[Expr, Expr, BinOp]
object GrT extends ParserBridge2[Expr, Expr, BinOp]
object GrEqT extends ParserBridge2[Expr, Expr, BinOp]
object LsT extends ParserBridge2[Expr, Expr, BinOp]
object LsEqT extends ParserBridge2[Expr, Expr, BinOp]
object Eq extends ParserBridge2[Expr, Expr, BinOp]
object NEq extends ParserBridge2[Expr, Expr, BinOp]
object And extends ParserBridge2[Expr, Expr, BinOp]
object Or extends ParserBridge2[Expr, Expr, BinOp]

object UnExpr extends ParserBridge2[UnOp, Expr, Expr]
object BinExpr extends ParserBridge3[Expr, BinOp, Expr, Expr]

object IntL extends ParserBridge1[Int, Atom]
object BoolL extends ParserBridge1[Boolean, Atom]
object CharL extends ParserBridge1[Char, Atom]
object StrL extends ParserBridge1[String, Atom]
object Ident extends ParserBridge1[String, Atom]
// TODO: find a way to extend from two bridges
// object ArrElem extends ParserBridge2[String, List[Expr], Atom] with ParserBridge2[String, List[Expr], LValue]

// Statements
object Program extends ParserBridge2[List[Func], Stmt, Program]
object Func extends ParserBridge4[Type, String, List[Param], Stmt, Func]
object Param extends ParserBridge2[Type, String, Param]

object Decl extends ParserBridge3[Type, String, RValue, Stmt]
object Asgn extends ParserBridge2[LValue, RValue, Stmt]
object Read extends ParserBridge1[LValue, Stmt]
object Free extends ParserBridge1[Expr, Stmt]
object Return extends ParserBridge1[Expr, Stmt]
object Exit extends ParserBridge1[Expr, Stmt]
object Print extends ParserBridge1[Expr, Stmt]
object Println extends ParserBridge1[Expr, Stmt]
object Cond extends ParserBridge3[Expr, Stmt, Stmt, Stmt]
object Loop extends ParserBridge2[Expr, Stmt, Stmt]
object Body extends ParserBridge1[Stmt, Stmt]
object Delimit extends ParserBridge2[Stmt, Stmt, Stmt]

object LIdent extends ParserBridge1[String, LValue]

object RExpr extends ParserBridge1[Expr, RValue]
object ArrL extends ParserBridge1[List[Expr], RValue]
object NewPair extends ParserBridge2[Expr, Expr, RValue]
object Call extends ParserBridge2[String, List[Expr], RValue]

object First extends ParserBridge1[LValue, PairElem]
object Second extends ParserBridge1[LValue, PairElem]