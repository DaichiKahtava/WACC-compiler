package wacc

import parsley.token.errors._
import parsley.generic._
import parsley.position.pos
import parsley.character._
import lexer.implicits.implicitSymbol
import parsley.Parsley
import parsley.syntax._

// Types
sealed trait Type
sealed trait PairElemType

case class IntT()(val pos: (Int, Int)) extends Type with PairElemType
case class BoolT()(val pos: (Int, Int)) extends Type with PairElemType
case class CharT()(val pos: (Int, Int)) extends Type with PairElemType
case class StringT()(val pos: (Int, Int)) extends Type with PairElemType
case class ArrayT(tp: Type)(val pos: (Int, Int)) extends Type with PairElemType

case class Pair(pe1: PairElemType, pe2: PairElemType)(val pos: (Int, Int)) extends Type

case class ErasedPair()(val pos: (Int, Int)) extends PairElemType with ParserBridge0[PairElemType]


// Expressions

sealed trait Expr

sealed trait UnOp extends Expr
case class Not(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Neg(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Len(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Ord(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Chr(x: Expr)(val pos: (Int, Int)) extends UnOp

sealed trait  BinOp extends Expr
case class Mul(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class Div(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class Mod(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class Add(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class Minus(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class GrT(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class GrEqT(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class LsT(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class LsEqT(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class Eq(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class NEq(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class And(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp
case class Or(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinOp

sealed trait Atom extends Expr
case class IntL(n: Int)(val pos: (Int, Int)) extends Atom
case class BoolL(b: Boolean)(val pos: (Int, Int)) extends Atom
case class CharL(c: Char)(val pos: (Int, Int)) extends Atom
case class StrL(s: String)(val pos: (Int, Int)) extends Atom
case class PairL()(val pos: (Int, Int)) extends Atom
case class Ident(id: String)(val pos: (Int, Int)) extends Atom
case class ArrElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends Atom

// Statements
case class Program(funcs: List[Func], s: Stmt)(val pos: (Int, Int))
case class Func(tp: Type, id: String, params: List[Param], s: Stmt)(val pos: (Int, Int))
case class Param(tp: Type, id: String)(val pos: (Int, Int))

sealed trait Stmt
case object Skip extends Stmt with ParserBridge0[Stmt]
case class Decl(tp: Type, id: String, rv: RValue)(val pos: (Int, Int)) extends Stmt
case class Asgn(lv: LValue, rv: RValue)(val pos: (Int, Int)) extends Stmt
case class Read(lv: LValue)(val pos: (Int, Int)) extends Stmt
case class Free(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Return(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Exit(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Print(x: Expr)(val pos: (Int, Int)) extends Stmt // May be able to combine print and println
case class Println(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Cond(x: Expr, s1: Stmt, s2: Stmt)(val pos: (Int, Int)) extends Stmt
case class Loop(x: Expr, s: Stmt)(val pos: (Int, Int)) extends Stmt
case class Body(s: Stmt)(val pos: (Int, Int)) extends Stmt
case class Delimit(s1: Stmt, s2: Stmt)/*(val pos: (Int, Int))*/ extends Stmt

sealed trait LValue
case class LIdent(id: String)(val pos: (Int, Int)) extends LValue
case class LArrElem(id: String, xs: List[Expr])(val pos: (Int, Int)) extends LValue

sealed trait RValue
case class RExpr(x: Expr)(val pos: (Int, Int)) extends RValue
case class ArrL(xs: List[Expr])/*(val pos: (Int, Int))*/ extends RValue // Could use the position of the first value!
case class NewPair(x1: Expr, x2: Expr)(val pos: (Int, Int)) extends RValue
case class Call(id: String, xs: List[Expr])(val pos: (Int, Int)) extends RValue

sealed trait PairElem extends RValue with LValue
case class First(lv: LValue)(val pos: (Int, Int)) extends PairElem
case class Second(lv: LValue)(val pos: (Int, Int)) extends PairElem
// TODO [for parser]: RArrL and Call's List[Expr] have different min # of elements


/// Companion objects - bridges for each AST node ///

// Types

// *** The following stuff could not be applied where an | is needed. So I implemented them
//     Directly without bridges

// object IntT extends {
//   def apply: Parsley[Type] = (pos <~ "int").map((p) => IntT()(p))
// }

// object BoolT {
//   def apply: Parsley[Type] = (pos <~ "bool").map((p) => BoolT()(p))
// }

// object CharT {
//   def apply: Parsley[Type] = (pos <~ "char").map((p) => CharT()(p))
// }

// object StringT {
//   def apply: Parsley[Type] = (pos <~ "string").map((p) => StringT()(p))
// }

object ArrayT extends ParserBridgePos1[Type, Type with PairElemType] // TODO: Needs review; not needed?
object Pair extends ParserBridgePos2[PairElemType, PairElemType, Type]

// Expressions
object Not extends ParserBridgePos1[Expr, UnOp]
object Neg extends ParserBridgePos1[Expr, UnOp]
object Len extends ParserBridgePos1[Expr, UnOp]
object Ord extends ParserBridgePos1[Expr, UnOp]
object Chr extends ParserBridgePos1[Expr, UnOp]

object Mul extends ParserBridgePos2[Expr, Expr, BinOp]
object Div extends ParserBridgePos2[Expr, Expr, BinOp]
object Mod extends ParserBridgePos2[Expr, Expr, BinOp]
object Add extends ParserBridgePos2[Expr, Expr, BinOp]
object Minus extends ParserBridgePos2[Expr, Expr, BinOp]
object GrT extends ParserBridgePos2[Expr, Expr, BinOp]
object GrEqT extends ParserBridgePos2[Expr, Expr, BinOp]
object LsT extends ParserBridgePos2[Expr, Expr, BinOp]
object LsEqT extends ParserBridgePos2[Expr, Expr, BinOp]
object Eq extends ParserBridgePos2[Expr, Expr, BinOp]
object NEq extends ParserBridgePos2[Expr, Expr, BinOp]
object And extends ParserBridgePos2[Expr, Expr, BinOp]
object Or extends ParserBridgePos2[Expr, Expr, BinOp]

object IntL extends ParserBridgePos1[Int, Atom]
object BoolL extends ParserBridgePos1[Boolean, Atom]
object CharL extends ParserBridgePos1[Char, Atom]
object StrL extends ParserBridgePos1[String, Atom]
object Ident extends ParserBridgePos1[String, Atom]
object ArrElem extends ParserBridgePos2[String, List[Expr], Atom]

// Statements
object Program extends ParserBridgePos2[List[Func], Stmt, Program]
object Func extends ParserBridgePos4[Type, String, List[Param], Stmt, Func]
object Param extends ParserBridgePos2[Type, String, Param]

object Decl extends ParserBridgePos3[Type, String, RValue, Stmt]
object Asgn extends ParserBridgePos2[LValue, RValue, Stmt]
object Read extends ParserBridgePos1[LValue, Stmt]
object Free extends ParserBridgePos1[Expr, Stmt]
object Return extends ParserBridgePos1[Expr, Stmt]
object Exit extends ParserBridgePos1[Expr, Stmt]
object Print extends ParserBridgePos1[Expr, Stmt]
object Println extends ParserBridgePos1[Expr, Stmt]
object Cond extends ParserBridgePos3[Expr, Stmt, Stmt, Stmt]
object Loop extends ParserBridgePos2[Expr, Stmt, Stmt]
object Body extends ParserBridgePos1[Stmt, Stmt]
object Delimit extends ParserBridge2[Stmt, Stmt, Stmt] // Not using position

object LIdent extends ParserBridgePos1[String, LValue]
object LArrElem extends ParserBridgePos2[String, List[Expr], LValue]

object RExpr extends ParserBridgePos1[Expr, RValue]
object ArrL extends ParserBridge1[List[Expr], RValue] // Not using position (we will use tthe elements instead)
object NewPair extends ParserBridgePos2[Expr, Expr, RValue]
object Call extends ParserBridgePos2[String, List[Expr], RValue]

object First extends ParserBridgePos1[LValue, PairElem]
object Second extends ParserBridgePos1[LValue, PairElem]
