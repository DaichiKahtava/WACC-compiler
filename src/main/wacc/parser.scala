package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.Parsley._
import parsley.expr.chain
import parsley.syntax._
import parsley.syntax.zipped._
import parsley.combinator._
import parsley.character._


import lexer.implicits.implicitSymbol
import lexer.{ident, integer, char_liter, str_liter, fully}

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
        // expr 
        arrayLiter 
        // | ((string("newpair"), char('\"'), expr, char('\"'), expr, string(")")).zipped(constrNewPair)) 
        // | pairElem 
        // | (string("call"), ident, char('('), argList, char(')')).zipped((_, id, _, xs, _) => (Call(id, xs)))
    )
    
    private val constrNewPair = (_: String, _: Char, x1: Expr, _: Char, x2: Expr, _: String) => (NewPair(x1, x2))

    private lazy val argList = sepBy(expr, ",")
    private lazy val pairElem = (("fst" ~> lvalue).map(First(_))) | (("snd" ~> lvalue).map(Second(_)))
    private lazy val arrayLiter = ("[" ~> sepBy1(expr, ",") <~ "]").map(ArrL(_)) | (("[" <~> "]") #> ArrL(List.empty))

    
    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y

    private lazy val expr: Parsley[Expr] = ???

    private lazy val atom = integer | bool_liter | char_liter | str_liter | pair_liter | ident | array_elem
    private lazy val bool_liter = ("true" #> BoolL(true))| ("false" #> BoolL(false))
    private lazy val pair_liter = "null" #> PairL()
    private lazy val array_elem = (ident, some("[" ~> expr <~ "]")).zipped((id, xs) => (ArrElem(id, xs)))
    // private lazy val atom = ???
    // private lazy val unOp = ???
    // private lazy val binOp = ???
    // private lazy val arrElem = ???
}
