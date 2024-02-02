package wacc

object semantics {

    /// Global pointer/reference to current symTable
    var symTable = new SymTable(None)

    /// Expressions ///
    def getType(e: Expr): Type = {
        return e match {
            case IntL(_) => IntT()
            case BoolL(_) => BoolT()
            case CharL(_) => CharT()
            case StrL(_) => StringT()
            case PairL() => Pair(AnyPet(), AnyPet())

            case Not(_) => BoolT()
            case Neg(_) => IntT()
            case Len(_) => IntT()
            case Ord(_) => IntT()
            case Chr(_) => CharT()
        
            case Mul(_, _) => IntT()
            case Div(_, _) => IntT()
            case Mod(_, _) => IntT()
            case Add(_, _) => IntT()
            case Minus(_, _) => IntT()

            case GrT(_, _) => BoolT()
            case GrEqT(_, _) => BoolT()
            case LsT(_, _) => BoolT()
            case LsEqT(_, _) => BoolT()

            case Eq(_, _) => BoolT()
            case NEq(_, _) => BoolT()

            case And(_, _) => BoolT()
            case Or(_, _) => BoolT()
            
            case ArrElem(_, List(x))  => getType(x)
        }
    }


    def equalType(e1: Expr, e2: Expr) = getType(e1) == getType(e2)

    def isSemCorrect(e: Expr): Boolean = {
        return e match {
            case IntL(_) => true
            case BoolL(_) => true
            case CharL(_) => true
            case StrL(_) => true
            case PairL() => true


            case Not(x) => isSemCorrect(x) && getType(x) == BoolT()
            case Neg(x) => isSemCorrect(x) && getType(x) == IntT()
            case Len(x) => isSemCorrect(x) && getType(x) == ArrayT(AnyT())
            case Ord(x) => isSemCorrect(x) && getType(x) == CharT()
            case Chr(x) => isSemCorrect(x) && getType(x) == IntT()

            case Mul(x1, x2) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
            case Div(x1, x2) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
            case Mod(x1, x2) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
            case Add(x1, x2) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
            case Minus(x1, x2) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
            
            case GrT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == IntT() || getType(x1) == CharT())) && (isSemCorrect(x2) && (getType(x2) == IntT() || getType(x2) == CharT()))
            case GrEqT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == IntT() || getType(x1) == CharT())) && (isSemCorrect(x2) && (getType(x2) == IntT() || getType(x2) == CharT()))
            case LsT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == IntT() || getType(x1) == CharT())) && (isSemCorrect(x2) && (getType(x2) == IntT() || getType(x2) == CharT()))
            case LsEqT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == IntT() || getType(x1) == CharT())) && (isSemCorrect(x2) && (getType(x2) == IntT() || getType(x2) == CharT()))
            
            case Eq(x1, x2) => (isSemCorrect(x1) && getType(x1) == AnyT()) && (isSemCorrect(x2) && getType(x2) == AnyT())
            case NEq(x1, x2) => (isSemCorrect(x1) && getType(x1) == AnyT()) && (isSemCorrect(x2) && getType(x2) == AnyT())
            
            case And(x1, x2) => (isSemCorrect(x1) && getType(x1) == BoolT()) && (isSemCorrect(x2) && getType(x2) == BoolT())
            case Or(x1, x2) => (isSemCorrect(x1) && getType(x1) == BoolT()) && (isSemCorrect(x2) && getType(x2) == BoolT())

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
            case Decl(tp, id, rv) => tp == getType(rv) && isSemCorrect(rv)
        }
  
    // LValue and check
    def isSemCorrect(lv: LValue): Boolean = lv match {
        case LIdent(_) => true
        case ArrElem(_, xs) => isSemCorrect(xs)
        case pe: PairElem => isSemCorrect(pe)
    }

    def listType(list: List[Expr]): Type = ???

    // RValue type and check
    def getType(rv: RValue): Type = rv match {
        case RExpr(e) => getType(e)
        case ArrL(xs) => listType(xs)
        case NewPair(e1, e2) => Pair(AnyPet(), AnyPet())
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
