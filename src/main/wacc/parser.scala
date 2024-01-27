package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain

import lexer.implicits.implicitSymbol
import lexer.{integer, fully}

object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    private lazy val parser = fully(expr)
    private lazy val program = ???
    private lazy val func = ???
    private lazy val paramList = ???
    private lazy val param = ???
    private lazy val stmt = ???
    private lazy val lvalue = ???
    private lazy val rvalue = ???
    private lazy val argList = ???
    private lazy val pairElem = ???
    private lazy val arrayLiter = ???

    
    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y

    private lazy val expr: Parsley[BigInt] =
        chain.left1(integer | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )

    // private lazy val atom = ???
    // private lazy val unOp = ???
    // private lazy val binOp = ???
    // private lazy val arrElem = ???
}
