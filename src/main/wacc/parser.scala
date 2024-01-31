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
    
    private lazy val program = ("begin" ~> many(func), stmt <~ "end").zipped(Program(_, _))
    private lazy val func = (typep, ident, "(" ~> paramList <~ ")", "is" ~> stmt <~ "end").zipped(Func(_, _, _, _))
    private lazy val paramList = sepBy(param, ",")
    private lazy val param = (typep, ident).zipped(Param(_, _))
    private lazy val stmt: Parsley[Stmt] = (
        ("skip" #> Skip())
        | ("read" ~> lvalue).map(Read(_))
        | ("free" ~> expr).map(Free(_))
        | ("return" ~> expr).map(Return(_))
        | ("exit" ~> expr).map(Exit(_))
        | ("print" ~> expr).map(Print(_))
        | ("println" ~> expr).map(Println(_))
        | (("if" ~> expr), ("then" ~> stmt), ("else" ~> stmt <~ "fi")).zipped(Cond(_, _, _))
        | (("while" ~> expr), ("do" ~> stmt <~ "done")).zipped(Loop(_, _))
        | ("begin" ~> stmt <~ "end").map(Body(_))
        | ((stmt <~ ";"), stmt).zipped(Delimit(_, _))
    )
    
    private lazy val lvalue: Parsley[LValue] = arrayElem | pairElem | ident.map(LIdent(_))
    private lazy val rvalue: Parsley[RValue] = (
        expr 
        | arrayLiter 
        | ((string("newpair"), char('\"'), expr, char('\"'), expr, string(")")).zipped(constrNewPair)) 
        | pairElem 
        | (string("call"), ident, char('('), argList, char(')')).zipped((_, id, _, xs, _) => (Call(id, xs)))
    )
    
    private val constrNewPair = (_: String, _: Char, x1: Expr, _: Char, x2: Expr, _: String) => (NewPair(x1, x2))

    private lazy val argList = sepBy(expr, ",")
    private lazy val pairElem = (("fst" ~> lvalue).map(First(_))) | (("snd" ~> lvalue).map(Second(_)))
    private lazy val arrayLiter = ("[" ~> sepBy1(expr, ",") <~ "]").map(ArrL(_)) | (("[" <~> "]") #> ArrL(List.empty))

    // Expressions

    private lazy val arrayElem = (ident, some("[" ~> expr <~ "]")).zipped((id, xs) => (ArrElem(id, xs)))

    private lazy val boolLit = ("true" #> BoolL(true) | "false" #> BoolL(false))
    private lazy val pairLit = "null" #> PairL()
    private lazy val expr: Parsley[Expr] = 
        precedence(
            intLit.map(IntL(_)),
            boolLit,
            charLit.map(CharL(_)),
            strLit.map(StrL(_)),
            pairLit, ident.map(Ident(_)),
            arrayElem,
            "(" ~> expr <~ ")"
        )(
            Ops(Prefix)("!" as Not, "-" as Neg, "len" as Len, "ord" as Ord, "chr" as Chr),
            Ops(InfixL)("*" as Mul, "%" as Mod, "/" as Div),
            Ops(InfixL)("+" as Add, "-" as Minus),
            Ops(InfixN)(">" as GrT, ">=" as GrEqT, "<" as LsT, "<=" as LsEqT),
            Ops(InfixN)("==" as Eq, "!=" as NEq),
            Ops(InfixR)("&&" as And),
            Ops(InfixR)("||" as Or)
        )


    // private lazy val atom = integer | lexer.character
    // private lazy val atom = ???
    // private lazy val unOp = ???
    // private lazy val binOp = ???
    // private lazy val arrElem = ???
}
