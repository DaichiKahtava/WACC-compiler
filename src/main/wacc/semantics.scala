package wacc

object semantics {

    /// Global pointer/reference to current symTable
    var symTable = new SymTable(None)

    /// Expressions ///
    def getType(e: Expr): S_TYPE = {
        return e match {
            case IntL(_) => S_INT()
            case BoolL(_) => S_BOOL()
            case CharL(_) => S_CHAR()
            case StrL(_) => S_STRING()
            case PairL() => S_PAIR(S_ANY(), S_ANY())

            case Not(_) => S_BOOL()
            case Neg(_) => S_INT()
            case Len(_) => S_INT()
            case Ord(_) => S_INT()
            case Chr(_) => S_CHAR()
        
            case Mul(_, _) => S_INT()
            case Div(_, _) => S_INT()
            case Mod(_, _) => S_INT()
            case Add(_, _) => S_INT()
            case Minus(_, _) => S_INT()

            case GrT(_, _) => S_BOOL()
            case GrEqT(_, _) => S_BOOL()
            case LsT(_, _) => S_BOOL()
            case LsEqT(_, _) => S_BOOL()

            case Eq(_, _) => S_BOOL()
            case NEq(_, _) => S_BOOL()

            case And(_, _) => S_BOOL()
            case Or(_, _) => S_BOOL()
            
            case ArrElem(_, List(x))  => getType(x)
        }
    }

    def canWeakenTo(t1: S_TYPE, t2: S_TYPE): Boolean = ???
    // Note, we can use canWeakenTo to get for any type.
    // TODO: We probably need to introduce an S_ERASED() for use in Pair semantic checking!

    def toSemanticType(t1: Type): S_TYPE = ???

    def equalType(e1: Expr, e2: Expr) = getType(e1) == getType(e2)

    def isSemCorrect(e: Expr): Boolean = {
        return e match {
            case IntL(_) => true
            case BoolL(_) => true
            case CharL(_) => true
            case StrL(_) => true
            case PairL() => true

            case Ident(id) => symTable.definedGlobal(id)

            case Not(x) => isSemCorrect(x) && getType(x) == S_BOOL()
            case Neg(x) => isSemCorrect(x) && getType(x) == S_INT()
            case Len(x) => isSemCorrect(x) && getType(x) == S_ARRAY(S_ANY()) // TODO: Use canWeakenTo
            case Ord(x) => isSemCorrect(x) && getType(x) == S_CHAR()
            case Chr(x) => isSemCorrect(x) && getType(x) == S_INT()

            case Mul(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT()) && (isSemCorrect(x2) && getType(x2) == S_INT())
            case Div(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT()) && (isSemCorrect(x2) && getType(x2) == S_INT())
            case Mod(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT()) && (isSemCorrect(x2) && getType(x2) == S_INT())
            case Add(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT()) && (isSemCorrect(x2) && getType(x2) == S_INT())
            case Minus(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT()) && (isSemCorrect(x2) && getType(x2) == S_INT())
            
            case GrT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == S_INT() || getType(x1) == S_CHAR())) && (isSemCorrect(x2) && (getType(x2) == S_INT() || getType(x2) == S_CHAR()))
            case GrEqT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == S_INT() || getType(x1) == S_CHAR())) && (isSemCorrect(x2) && (getType(x2) == S_INT() || getType(x2) == S_CHAR()))
            case LsT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == S_INT() || getType(x1) == S_CHAR())) && (isSemCorrect(x2) && (getType(x2) == S_INT() || getType(x2) == S_CHAR()))
            case LsEqT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == S_INT() || getType(x1) == S_CHAR())) && (isSemCorrect(x2) && (getType(x2) == S_INT() || getType(x2) == S_CHAR()))
            
            // TODO: Rewrite using Use canWeakenTo
            case Eq(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_ANY()) && (isSemCorrect(x2) && getType(x2) == S_ANY())
            case NEq(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_ANY()) && (isSemCorrect(x2) && getType(x2) == S_ANY())
            
            case And(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_BOOL()) && (isSemCorrect(x2) && getType(x2) == S_BOOL())
            case Or(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_BOOL()) && (isSemCorrect(x2) && getType(x2) == S_BOOL())

            case ArrElem(_, xs)  => isSemCorrect(xs)
        }
    }

    // Semantics of a list of expressions
    def isSemCorrect(list: List[Expr]): Boolean = list match {
        case Nil => true
        case (e: Expr) :: es => isSemCorrect(e) && isSemCorrect(es)
    }


    
    /// Statements ///
    def isSemCorrect(program: Program): Boolean = {
        false
        // if (program.funcs.isEmpty) isSemCorrect(program.funcs) else isSemCorrect(program.funcs) && isSemCorrect(program.stmt)
    }

    // TODO: check recursively for a return statment and make sure type is correct
    def isSemCorrect(func: Func): Boolean = {
        false
        // func.tp == statementType(func.stmt) && isSemCorrect(func.stmt)
    }

    def isSemCorrect(param: Param): Boolean = ???
        
    def isSemCorrect(stmt: Stmt): Boolean = stmt match {
            case Skip() => true
            case Decl(tp, id, rv) => toSemanticType(tp) == getType(rv) && isSemCorrect(rv)
        }
  
    // LValue and check
    def isSemCorrect(lv: LValue): Boolean = lv match {
        case LIdent(_) => true
        case ArrElem(_, xs) => isSemCorrect(xs)
        case pe: PairElem => isSemCorrect(pe)
    }

    def listType(list: List[Expr]): S_TYPE = ???

    // RValue type and check
    def getType(rv: RValue): S_TYPE = rv match {
        case RExpr(e) => getType(e)
        case ArrL(xs) => listType(xs)
        case NewPair(e1, e2) => S_PAIR(getType(e1), getType(e2))
        case pe: PairElem => ???
        case Call(id, xs) => ???
    }

    def isSemCorrect(rv: RValue): Boolean = rv match {
        case RExpr(e) => isSemCorrect(e)
        case ArrL(xs) => isSemCorrect(xs)
        case NewPair(e1, e2) => isSemCorrect(e1) && isSemCorrect(e2)
        case pe: PairElem => isSemCorrect(pe)
        case Call(_, xs) => isSemCorrect(xs)
    }

    // PairElem check 
    def isSemCorrect(pe: PairElem): Boolean = pe match {
        case First(lv) => isSemCorrect(lv)
        case Second(lv) => isSemCorrect(lv)
    }
}
