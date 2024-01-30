package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.Parsley._
import parsley.expr.chain
import parsley.syntax._
import parsley.combinator._


import lexer.implicits.implicitSymbol
import lexer.{integer, fully}

object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    private lazy val parser = fully(program)
    private lazy val program = ???
    // private lazy val func = ???
    // private lazy val paramList = ???
    // private lazy val param = ???
    // private lazy val stmt = ("skip" #> Skip()) | ("read" ~> expr).map((x => Read(x)))
    // private lazy val lvalue = ???
    // private lazy val rvalue = ???
    // private lazy val argList = sepBy1(expr, ",")
    // private lazy val pairElem = (("fst" ~> lvalue).map(First(_))) | (("snd" ~> rvalue).map(Second(_)))
    // private lazy val arrayLiter = ("[" ~> sepBy1(expr, ",") <~ "]") | (("[" <~> "]") #> List.empty)

    
    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y

    private lazy val expr: Parsley[BigInt] =
        chain.left1(integer | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )


    private lazy val atom = integer | lexer.character
    // private lazy val atom = ???
    // private lazy val unOp = ???
    // private lazy val binOp = ???
    // private lazy val arrElem = ???
}
