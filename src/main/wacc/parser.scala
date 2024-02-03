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
    def parse(input: String): Result[String, Program] = parser.parse(input)
    private lazy val parser = fully(program)
    
    // Types
    private lazy val typep: Parsley[Type] = baseType | arrayType | pairType // TODO: ARRAY TYPE RECURSIVE ERROR

    private lazy val baseType = IntT <# "int" | BoolT <# "bool" | CharT <# "char" | StringT <# "string"
    private lazy val arrayType = ArrayT(typep <~ "[" <~ "]")
    private lazy val pairType = Pair("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
    private lazy val pairElemType: Parsley[PairElemType] = baseType | arrayType | ErasedPair <# "pair"

    // Statments
    
    private lazy val program = Program("begin" ~> many(func), stmt <~ "end")
    private lazy val func = Func(typep, ident, "(" ~> paramList <~ ")", "is" ~> stmt.filter(funcEnd) <~ "end")
    private lazy val paramList = sepBy(param, ",")
    lazy val param = Param(typep, ident) // TODO: RETURN IT TO PRIVATE ONCE TESTING IS DONE

    def funcEnd(stmt:Stmt):Boolean = stmt match {
        case Skip => false
        case Decl(_,_,_) => false
        case Asgn(_, _) => false
        case Read(_) => false
        case Free(_) => false
        case Return(_) => true
        case Exit(_) => true
        case Print(_) => false
        case Println(_) => false
        case Cond(_, s1: Stmt, s2: Stmt) => funcEnd(s1) && funcEnd(s2)
        case Loop(_,_) => false
        case Body(s:Stmt) => funcEnd(s) 
        case Delimit(_, s: Stmt) => funcEnd(s)
        
    }

    lazy val stmt: Parsley[Stmt] = (
        Skip <# "skip"
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

    
    
    lazy val lvalue: Parsley[LValue] = LIdent(ident) | leftArrayElem | pairElem // TODO: BACKTRACKING
    private lazy val rvalue: Parsley[RValue] = (
        expr 
        | arrayLiter 
        | NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
        | pairElem 
        | Call("call" ~> ident, "(" ~> argList <~ ")")
    )

    private lazy val argList = sepBy(expr, ",")
    private lazy val pairElem = First("fst" ~> lvalue) | Second("snd" ~> lvalue)
    private lazy val arrayLiter = ArrL("[" ~> sepBy1(expr, ",") <~ "]") | ("[" <~> "]") #> ArrL(List.empty)

    // Expressions

    private lazy val arrayElem = ArrElem(ident, some("[" ~> expr <~ "]"))
    private lazy val leftArrayElem = LArrElem(ident, some("[" ~> expr <~ "]"))

    private lazy val boolLit = "true" #> BoolL(true) | "false" #> BoolL(false)
    private lazy val pairLit = PairL <# "null"
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
            Ops(Prefix)(Not <# "!", Neg <# "-", Len <# "len", Ord <# "ord", Chr <# "chr"),
            Ops(InfixL)(Mul <# "*", Mod <# "%", Div <# "/"),
            Ops(InfixL)(Add <# "+", Minus <# "-"),
            Ops(InfixN)(GrT <# ">", GrEqT <# ">=", LsT <# "<", LsEqT <# "<="),
            Ops(InfixN)(Eq <# "==", NEq <# "!="),
            Ops(InfixR)(And <# "&&"),
            Ops(InfixR)(Or <# "||")
        )

}
