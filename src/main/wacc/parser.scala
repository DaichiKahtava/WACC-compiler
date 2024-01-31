package wacc

import parsley.Parsley._
import parsley.{Parsley, Result}
import parsley.syntax._
import parsley.syntax.zipped._
import parsley.combinator._
import parsley.character._
import parsley.expr.{chain, precedence, Ops, InfixL, InfixN, InfixR, Prefix}

import lexer.implicits.implicitSymbol
import lexer.{ident, intLit, charLit, strLit, fully}
import scala.collection.immutable.IntMap

object parser {
    def parse(input: String): Result[String, BigInt] = ??? // parser.parse(input)
    private lazy val parser = fully(program)
    
    // Types
    private lazy val typep: Parsley[Type] = baseType | arrayType | pairType

    private lazy val baseType = ("int" #> IntT()) | ("bool" #> BoolT()) | ("char" #> CharT()) | ("string" #> StringT())
    private lazy val arrayType = (typep <~ "[" <~ "]").map((t) => ArrayT(t))
    private lazy val pairType = (string("pair"), char('('), pairElemType, char(','), pairElemType, char(')')).zipped((_, _, pe1, _, pe2, _) => (Pair(pe1, pe2)))
    private lazy val pairElemType: Parsley[PairElemType] = baseType | arrayType | ("pair" #> ErasedPair())

    // Statments
    
    private lazy val program = Program("begin" ~> many(func), stmt <~ "end")
    private lazy val func = Func(typep, ident, "(" ~> paramList <~ ")", "is" ~> stmt <~ "end")
    private lazy val paramList = sepBy(param, ",")
    private lazy val param = Param(typep, ident)
    private lazy val stmt: Parsley[Stmt] = (
        ("skip" #> Skip())
        | Read("read" ~> lvalue)
        | Free("free" ~> expr)
        | Return("return" ~> expr)
        | Exit("exit" ~> expr)
        | Print("print" ~> expr)
        | Println("println" ~> expr)
        | Cond("if" ~> expr, "then" ~> stmt, "else" ~> stmt <~ "fi")
        | Loop("while" ~> expr, "do" ~> stmt <~ "done")
        | Body("begin" ~> stmt <~ "end")
        | Delimit(stmt <~ ";", stmt)
    )
    
    private lazy val lvalue: Parsley[LValue] = arrayElem | pairElem | LIdent(ident)
    private lazy val rvalue: Parsley[RValue] = (
        expr 
        | arrayLiter 
        | NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
        | pairElem 
        | Call("call" ~> ident, "(" ~> argList <~ ")")
    )

    private lazy val argList = sepBy(expr, ",")
    private lazy val pairElem = First("fst" ~> lvalue) | Second("snd" ~> lvalue)
    private lazy val arrayLiter = ArrL("[" ~> sepBy1(expr, ",") <~ "]") | (("[" <~> "]") #> ArrL(List.empty))

    // Expressions

    //TODO implement extending from 2 bridges
    private lazy val arrayElem = (ident, some("[" ~> expr <~ "]")).zipped((id, xs) => (ArrElem(id, xs)))

    private lazy val boolLit = "true" #> BoolL(true) | "false" #> BoolL(false)
    private lazy val pairLit = "null" #> PairL()
    private lazy val expr: Parsley[Expr] = 
        precedence(
            IntL(intLit),
            boolLit,
            CharL(charLit),
            StrL(strLit),
            pairLit,
            Ident(ident),
            arrayElem,
            "(" ~> expr <~ ")"
        )(
            Ops(Prefix)(Not from "!", Neg from "-", Len from "len", Ord from "ord", Chr from "chr"),
            Ops(InfixL)(Mul from "*", Mod from "%", Div from "/"),
            Ops(InfixL)(Add from "+", Minus from "-"),
            Ops(InfixN)(GrT from ">", GrEqT from ">=", LsT from "<", LsEqT from "<="),
            Ops(InfixN)(Eq from "==", NEq from "!="),
            Ops(InfixR)(And from "&&"),
            Ops(InfixR)(Or from "||")
        )

}
