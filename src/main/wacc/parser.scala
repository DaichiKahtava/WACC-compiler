package wacc

import parsley.Parsley._
import parsley.{Parsley, Result}
import parsley.syntax._
import parsley.syntax.zipped._
import parsley.combinator._
import parsley.character._
import parsley.expr.{chain, precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import parsley.position.pos

import lexer.implicits.implicitSymbol
import lexer.{ident, intLit, charLit, strLit, fully}
import scala.collection.immutable.IntMap

object parser {
    def parse(input: String): Result[String, Program] = parser.parse(input)
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
    
    protected [wacc] lazy val program = Program("begin" ~> many(func), stmt <~ "end")
    protected [wacc] lazy val func = Func(typep, ident, "(" ~> paramList <~ ")", "is" ~> stmt.filter(funcEnd) <~ "end")
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

    protected [wacc] lazy val stmt: Parsley[Stmt] = (
        Skip <# "skip"
        | Read("read" ~> lvalue)
        | Free("free" ~> expr)
        | Return("return" ~> expr)
        | Exit("exit" ~> expr)
        | Print("print" ~> expr)
        | Println("println" ~> expr)
        | Cond("if" ~> expr, "then" ~> stmts, "else" ~> stmts <~ "fi")
        | Loop("while" ~> expr, "do" ~> stmts <~ "done")
        | Body("begin" ~> stmts <~ "end")
    )

    protected [wacc] lazy val stmts: Parsley[Stmt] = chain.left1(stmt)(";".as(Delimit(_, _)))

    
    
    protected [wacc] lazy val lvalue: Parsley[LValue] = atomic(leftArrayElem) | LIdent(ident) | pairElem // TODO: BACKTRACKING
    protected [wacc]lazy val rvalue: Parsley[RValue] = (
        expr 
        | arrayLiter 
        | NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
        | pairElem 
        | Call("call" ~> ident, "(" ~> argList <~ ")")
    )

    protected [wacc] lazy val argList = sepBy(expr, ",")
    protected [wacc] lazy val pairElem = First("fst" ~> lvalue) | Second("snd" ~> lvalue)
    protected [wacc] lazy val arrayLiter = ArrL("[" ~> sepBy1(expr, ",") <~ "]") | ("[" <~> "]") #> ArrL(List.empty)

    // Expressions

    protected [wacc] lazy val arrayElem = ArrElem(ident, some("[" ~> expr <~ "]"))
    lazy val leftArrayElem = LArrElem(ident, some("[" ~> expr <~ "]"))

    protected [wacc] lazy val boolLit = (
        (pos <~ "true").map(BoolL(true)(_)) 
        | (pos <~ "false").map(BoolL(false)(_))) 
    protected [wacc] lazy val pairLit = (pos, string("null")).zipped((p, _) => PairL()(p))
    protected [wacc] lazy val expr: Parsley[Expr] = 
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
