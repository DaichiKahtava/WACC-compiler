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

object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    private lazy val parser = fully(program)
    
    // Statments
    
    private lazy val program = ???
    // private lazy val func = ???
    // private lazy val paramList = ???
    // private lazy val param = ???
    // private lazy val stmt = ("skip" #> Skip()) | ("read" ~> expr).map((x => Read(x)))
    
    private lazy val lvalue = ???
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

    
    private val add = (x: Int, y: Int) => x + y
    private val sub = (x: Int, y: Int) => x - y

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
