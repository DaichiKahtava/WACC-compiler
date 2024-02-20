package wacc

import parsley.token.errors._
import parsley.generic._
import parsley.position.pos
import parsley.character._
import lexer.implicits.implicitSymbol
import parsley.Parsley
import parsley.syntax._
import parsley.generic

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

sealed trait Expr {
  val pos: (Int, Int)
}

sealed trait UnOp extends Expr
case class Not(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Neg(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Len(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Ord(x: Expr)(val pos: (Int, Int)) extends UnOp
case class Chr(x: Expr)(val pos: (Int, Int)) extends UnOp

sealed trait BinOp extends Expr
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
case class Skip()(val pos: (Int, Int)) extends Stmt
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

sealed trait LValue {
    val pos: (Int, Int)
}
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

trait UnaryOperator extends ParserBridgePos1[Expr, UnOp] {
    override def labels: List[String] = List("Unary operator")
}

trait BinaryOperator extends ParserBridgePos2[Expr, Expr, BinOp] {
    override def labels: List[String] = List("Binary operator")
}

// Expressions
object Not extends UnaryOperator
object Neg extends UnaryOperator
object Len extends UnaryOperator
object Ord extends UnaryOperator
object Chr extends UnaryOperator

object Mul extends BinaryOperator
object Div extends BinaryOperator
object Mod extends BinaryOperator
object Add extends BinaryOperator
object Minus extends BinaryOperator
object GrT extends BinaryOperator
object GrEqT extends BinaryOperator
object LsT extends BinaryOperator
object LsEqT extends BinaryOperator
object Eq extends BinaryOperator
object NEq extends BinaryOperator
object And extends BinaryOperator
object Or extends BinaryOperator

object IntL extends ParserBridgePos1[Int, Atom]
object BoolL extends ParserBridgePos1[Boolean, Atom]
object CharL extends ParserBridgePos1[Char, Atom]
object StrL extends ParserBridgePos1[String, Atom]
object Ident extends ParserBridgePos1[String, Atom]
object ArrElem extends ParserBridgePos2[String, List[Expr], Atom]

// Statements
object Program extends ParserBridgePos2[List[Func], Stmt, Program] {
    override def labels: List[String] = List("program body")
}
object Func extends ParserBridgePos4[Type, String, List[Param], Stmt, Func] {
    override def labels: List[String] = List("function declaration")
}
object Param extends ParserBridgePos2[Type, String, Param]

object Decl extends ParserBridgePos3[Type, String, RValue, Stmt] {
    // override def labels: List[String] = List("varaible declaration")
}
object Asgn extends ParserBridgePos2[LValue, RValue, Stmt] {
    // override def labels: List[String] = List("assignment")
}
object Read extends ParserBridgePos1[LValue, Stmt] {
    // override def labels: List[String] = List("read statement")
}
object Free extends ParserBridgePos1[Expr, Stmt] {
    // override def labels: List[String] = List("free statement")
}
object Return extends ParserBridgePos1[Expr, Stmt] {
    // override def labels: List[String] = List("return statement")
}
object Exit extends ParserBridgePos1[Expr, Stmt] {
    // override def labels: List[String] = List("exit statement")
}
object Print extends ParserBridgePos1[Expr, Stmt] {
    // override def labels: List[String] = List("print statement")
}
object Println extends ParserBridgePos1[Expr, Stmt] {
    // override def labels: List[String] = List("print statement")
}
object Cond extends ParserBridgePos3[Expr, Stmt, Stmt, Stmt]
object Loop extends ParserBridgePos2[Expr, Stmt, Stmt]
object Body extends ParserBridgePos1[Stmt, Stmt]
object Delimit extends ParserBridge2[Stmt, Stmt, Stmt] // Not using position

object LIdent extends ParserBridgePos1[String, LValue]
object LArrElem extends ParserBridgePos2[String, List[Expr], LValue]

object RExpr extends ParserBridgePos1[Expr, RValue]
object ArrL extends ParserBridge1[List[Expr], RValue] {
    override def labels: List[String] = List("array literal")
} // Not using position (we will use tthe elements instead)
object NewPair extends ParserBridgePos2[Expr, Expr, RValue] {
    override def labels: List[String] = List("newpair")
}
object Call extends ParserBridgePos2[String, List[Expr], RValue]

object First extends ParserBridgePos1[LValue, PairElem] {
    override def labels: List[String] = List("pair member")
}
object Second extends ParserBridgePos1[LValue, PairElem]  {
    override def labels: List[String] = List("pair member")
}
