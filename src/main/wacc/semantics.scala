package wacc

object semantics {

    /// Expressions ///
    def getType(e: Expr): Type = {
        return e match {
            case IntL(_) => IntT()
            case BoolL(_) => BoolT()
            case CharL(_) => CharT()
            case StrL(_) => StringT()
            case PairL() => Pair(AnyPet(), AnyPet())
            case UnExpr(op, x) => op match {
                case Not(_) => BoolT()
                case Neg(_) => IntT()
                case Len(_) => IntT()
                case Ord(_) => IntT()
                case Chr(_) => CharT()
            }
            case BinExpr(x1, op, x2) => op match {
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
            }
            case ArrElem(_, List(x))  => getType(x)
            case BinExpr(x1, op, x2) => getType(x1)
            case BinExpr(x1, op, x2) => getType(x2)
        }
    }

    def isSemCorrect(e: Expr): Boolean = {
        return e match {
            case IntL(_) => true
            case BoolL(_) => true
            case CharL(_) => true
            case StrL(_) => true
            case PairL() => true
            case UnExpr(op, x) => op match {
                case Not(_) => isSemCorrect(x) && getType(x) == BoolT()
                case Neg(_) => isSemCorrect(x) && getType(x) == IntT()
                case Len(_) => isSemCorrect(x) && getType(x) == ArrayT(AnyT())
                case Ord(_) => isSemCorrect(x) && getType(x) == CharT()
                case Chr(_) => isSemCorrect(x) && getType(x) == IntT()
            }
            case BinExpr(x1, op, x2) => op match {
                case Mul(_, _) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
                case Div(_, _) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
                case Mod(_, _) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
                case Add(_, _) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
                case Minus(_, _) => (isSemCorrect(x1) && getType(x1) == IntT()) && (isSemCorrect(x2) && getType(x2) == IntT())
                case GrT(_, _) => (isSemCorrect(x1) && (getType(x1) == IntT() || getType(x1) == CharT())) && (isSemCorrect(x2) && (getType(x2) == IntT() || getType(x2) == CharT()))
                case GrEqT(_, _) => (isSemCorrect(x1) && (getType(x1) == IntT() || getType(x1) == CharT())) && (isSemCorrect(x2) && (getType(x2) == IntT() || getType(x2) == CharT()))
                case LsT(_, _) => (isSemCorrect(x1) && (getType(x1) == IntT() || getType(x1) == CharT())) && (isSemCorrect(x2) && (getType(x2) == IntT() || getType(x2) == CharT()))
                case LsEqT(_, _) => (isSemCorrect(x1) && (getType(x1) == IntT() || getType(x1) == CharT())) && (isSemCorrect(x2) && (getType(x2) == IntT() || getType(x2) == CharT()))
                case Eq(_, _) => (isSemCorrect(x1) && getType(x1) == AnyT()) && (isSemCorrect(x2) && getType(x2) == AnyT())
                case NEq(_, _) => (isSemCorrect(x1) && getType(x1) == AnyT()) && (isSemCorrect(x2) && getType(x2) == AnyT())
                case And(_, _) => (isSemCorrect(x1) && getType(x1) == BoolT()) && (isSemCorrect(x2) && getType(x2) == BoolT())
                case Or(_, _) => (isSemCorrect(x1) && getType(x1) == BoolT()) && (isSemCorrect(x2) && getType(x2) == BoolT())
            }
            case ArrElem(_, xs)  => isSemCorrect(xs)
            case BinExpr(x1, op, x2) => isSemCorrect(x1)
            case BinExpr(x1, op, x2) => isSemCorrect(x2)
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
