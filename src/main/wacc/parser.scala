package wacc

import parsley.Parsley._
import parsley.{Parsley, Result}
import parsley.syntax._
import parsley.syntax.zipped._
import parsley.combinator._
import parsley.character._
import parsley.expr.{chain, precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import parsley.position.pos
import parsley.{Success, Failure}
import parsley.debug._
import parsley.errors.combinator._

import lexer.implicits.implicitSymbol
import lexer.{ident, intLit, charLit, strLit, fully}
import scala.collection.immutable.IntMap


object parser {
    def parse(input: String): Result[String, Program] = {
        val result = parser.parse(input)
        result match {
            case Success(_) => result
            case Failure(message) => {
                println(s"Parser error: $message")
                result
            }
        }
    }
    // def parse(input: String): Result[String, Program] = parser.parse(input)
    protected [wacc] lazy val parser = fully(program)

    /* Types

    The following system had been applied:
        <type> -> (baseType | "pair" "(" <pair-elem-type> "," <pair-elem-type> ")") ('[' ']')+
        |                                                                             ^^^^^^
        |                                                      if present it is an <array-type> === arrayT

        <pair-elem-type> -> (baseType | "pair" | type {iff arrayT})

        <arrayType> -> <type> {iff arrayT}
    */

    protected [wacc] lazy val typep: Parsley[Type] = 
        (pos, (baseType | pairType), many(atomic("[" ~> "]"))).zipped((p, t, as) => {
        as.foldRight(t)((_, ts) => ArrayT(ts)(p)) 
        // TODO: Use bridges?
    })

    protected [wacc] lazy val baseType = (        
        (pos <~ "int").map(IntT()(_)) | (pos <~ "bool").map(BoolT()(_))
        | (pos <~ "char").map(CharT()(_)) | (pos <~ "string").map(StringT()(_))
    )

    protected [wacc] lazy val arrayType: Parsley[ArrayT] 
      = typep.filter(isArray).map(_.asInstanceOf[ArrayT])

    protected [wacc] lazy val pairType = Pair("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
    protected [wacc] lazy val pairElemType: Parsley[PairElemType] = (
        atomic(arrayType) 
        | baseType
        | (pos <~ "pair").map(ErasedPair()(_))
    )
    
    private def isArray(t: Type): Boolean = t match {
        case ArrayT(tp) => true
        case _ => false
    }

    // Statments
    
    protected [wacc] lazy val program = Program("begin" ~> many(func), stmt.label("program body") <~ "end")
    protected [wacc] lazy val func = atomic(Func(typep, ident, "(" ~> paramList <~ ")", "is" ~> stmt.filter(funcEnd) <~ "end"))
    protected [wacc] lazy val paramList = sepBy(param, ",")
    protected [wacc] lazy val param = Param(typep, ident) // TODO: RETURN IT TO PRIVATE ONCE TESTING IS DONE

    protected [wacc] def funcEnd(stmt:Stmt):Boolean = stmt match {
        case Return(_) => true
        case Exit(_) => true
        case Cond(_, s1, s2) => funcEnd(s1) && funcEnd(s2)
        case Body(s) => funcEnd(s) 
        case Delimit(_, s) => funcEnd(s)
        case _ => false
    }

    protected [wacc] lazy val stmt: Parsley[Stmt] = 
        chain.left1(
            (pos <~ "skip").map(Skip()(_))
            | Decl(typep, ident, "=" ~> rvalue)
            | Asgn(atomic(lvalue <~ "="), rvalue)
            | Read("read" ~> lvalue)
            | Free("free" ~> expr)
            | Return("return" ~> expr)
            | Exit("exit" ~> expr)
            | Print("print" ~> expr)
            | Println("println" ~> expr)
            | Cond("if" ~> expr, "then" ~> stmt, "else" ~> stmt <~ "fi")
            | Loop("while" ~> expr, "do" ~> stmt <~ "done")
            | Body("begin" ~> stmt <~ "end")
        )(Delimit <# ";")

    protected [wacc] lazy val lvalue: Parsley[LValue] = atomic(leftArrayElem) | LIdent(ident) | pairElem // TODO: BACKTRACKING
    protected [wacc]lazy val rvalue: Parsley[RValue] = (
        atomic(arrayLiter)
        | atomic(emptyArrayLiter)
        | NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
        | pairElem
        | atomic(Call("call" ~> ident, "(" ~> argList <~ ")"))
        | RExpr(expr)
    )

    protected [wacc] lazy val argList = sepBy(expr, ",")
    protected [wacc] lazy val pairElem = First("fst" ~> lvalue) | Second("snd" ~> lvalue)
    protected [wacc] lazy val emptyArrayLiter = (("[" <~> "]") #> ArrL(List.empty)).label("empty array")
    protected [wacc] lazy val arrayLiter = ArrL("[" ~> sepBy1(expr, ",") <~ "]")

    // Expressions

    protected [wacc] lazy val arrayElem = ArrElem(ident, some("[" ~> expr <~ "]"))
    lazy val leftArrayElem = LArrElem(ident, some("[" ~> expr <~ "]"))

    protected [wacc] lazy val boolLit = (
        (pos <~ "true").map(BoolL(true)(_)) 
        | (pos <~ "false").map(BoolL(false)(_))) 
    protected [wacc] lazy val pairLit = (pos <~ "null").map(PairL()(_))
    protected [wacc] lazy val expr: Parsley[Expr] = (
        precedence(
            IntL(intLit),
            boolLit,
            CharL(charLit),
            StrL(strLit),
            pairLit,
            atomic(arrayElem),
            Ident(ident),
            "(" ~> expr <~ ")"
        )(
            Ops(Prefix)(Not <# "!", Len <# "len", Ord <# "ord", Chr <# "chr"),
            Ops(InfixL)(Mul <# "*", Mod <# "%", Div <# "/"),
            Ops(InfixL)(Add <# "+", Minus <# "-"),
            Ops(InfixN)(GrT <# ">", GrEqT <# ">=", LsT <# "<", LsEqT <# "<="),
            Ops(InfixN)(Eq <# "==", NEq <# "!="),
            Ops(InfixR)(And <# "&&"),
            Ops(InfixR)(Or <# "||")
         )
         | precedence(
            Ident(ident),
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
    ).label("full expression").explain("This expression is missing an operand")
}
