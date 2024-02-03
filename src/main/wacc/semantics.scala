package wacc

object semantics {

    /// Global pointer/reference to current symTable
    var symTable = new SymTable(None)

    /// Expressions ///
    def getType(e: Expr): S_TYPE = {
        return e match {
            case IntL(_) => S_INT
            case BoolL(_) => S_BOOL
            case CharL(_) => S_CHAR
            case StrL(_) => S_STRING
            case PairL() => S_PAIR(S_ANY, S_ANY)

            case Not(_) => S_BOOL
            case Neg(_) => S_INT
            case Len(_) => S_INT
            case Ord(_) => S_INT
            case Chr(_) => S_CHAR
        
            case Mul(_, _) => S_INT
            case Div(_, _) => S_INT
            case Mod(_, _) => S_INT
            case Add(_, _) => S_INT
            case Minus(_, _) => S_INT

            case GrT(_, _) => S_BOOL
            case GrEqT(_, _) => S_BOOL
            case LsT(_, _) => S_BOOL
            case LsEqT(_, _) => S_BOOL

            case Eq(_, _) => S_BOOL
            case NEq(_, _) => S_BOOL

            case And(_, _) => S_BOOL
            case Or(_, _) => S_BOOL
            
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

            case Not(x) => isSemCorrect(x) && getType(x) == S_BOOL
            case Neg(x) => isSemCorrect(x) && getType(x) == S_INT
            case Len(x) => isSemCorrect(x) && getType(x) == S_ARRAY(S_ANY) // TODO: Use canWeakenTo
            case Ord(x) => isSemCorrect(x) && getType(x) == S_CHAR
            case Chr(x) => isSemCorrect(x) && getType(x) == S_INT

            case Mul(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT) && (isSemCorrect(x2) && getType(x2) == S_INT)
            case Div(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT) && (isSemCorrect(x2) && getType(x2) == S_INT)
            case Mod(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT) && (isSemCorrect(x2) && getType(x2) == S_INT)
            case Add(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT) && (isSemCorrect(x2) && getType(x2) == S_INT)
            case Minus(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_INT) && (isSemCorrect(x2) && getType(x2) == S_INT)
            
            case GrT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == S_INT || getType(x1) == S_CHAR)) && (isSemCorrect(x2) && (getType(x2) == S_INT || getType(x2) == S_CHAR))
            case GrEqT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == S_INT || getType(x1) == S_CHAR)) && (isSemCorrect(x2) && (getType(x2) == S_INT || getType(x2) == S_CHAR))
            case LsT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == S_INT || getType(x1) == S_CHAR)) && (isSemCorrect(x2) && (getType(x2) == S_INT || getType(x2) == S_CHAR))
            case LsEqT(x1, x2) => (isSemCorrect(x1) && (getType(x1) == S_INT || getType(x1) == S_CHAR)) && (isSemCorrect(x2) && (getType(x2) == S_INT || getType(x2) == S_CHAR))
            
            // TODO: Rewrite using Use canWeakenTo
            case Eq(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_ANY) && (isSemCorrect(x2) && getType(x2) == S_ANY)
            case NEq(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_ANY) && (isSemCorrect(x2) && getType(x2) == S_ANY)
            
            case And(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_BOOL) && (isSemCorrect(x2) && getType(x2) == S_BOOL)
            case Or(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_BOOL) && (isSemCorrect(x2) && getType(x2) == S_BOOL)

            case ArrElem(_, xs)  => isSemCorrect(xs)
        }
    }

    // Semantics of a list of AST nodes..... don't know how to use generics so is a list of Any for now :(
    def isSemCorrect(list: List[Any]): Boolean = list match {
        case Nil => true
        case (e: Expr) :: es => isSemCorrect(e) && isSemCorrect(es)
        case (e: Func) :: es => isSemCorrect(e) && isSemCorrect(es)
        case _ => false
    }


    
    /// Statements ///
    def isSemCorrect(program: Program): Boolean = isSemCorrect(program.funcs) && isSemCorrect(program.stmt)

    // TODO: check recursively for a return statment and make sure type is correct
    def isSemCorrect(func: Func): Boolean = ???
        
    def isSemCorrect(stmt: Stmt): Boolean = stmt match {
            case Skip() => true
            case Decl(tp, id, rv) => toSemanticType(tp) == getType(rv) && isSemCorrect(rv)
            case Asgn(lv, rv) => isSemCorrect(lv) && isSemCorrect(rv) && getType(lv) == getType(rv)
            case Read(lv) => getType(lv) == S_CHAR || getType(lv) == S_INT
            case Free(x) => getType(x) == S_ARRAY || getType(x) == S_PAIR
            case Return(x) => isSemCorrect(x) // Need to check with the given return type of the function?
            case Exit(x) => getType(x) == S_INT
            case Print(x) => isSemCorrect(x)
            case Println(x) => isSemCorrect(x)
            case Cond(x, s1, s2) => isSemCorrect(x) && isSemCorrect(s1) && isSemCorrect(s2)
            case Loop(x, stmt) => isSemCorrect(x) && isSemCorrect(stmt)
            case Body(stmt) => isSemCorrect(stmt)
            case Delimit(s1, s2) => isSemCorrect(s1) && isSemCorrect(s2)
        }
  
    // LValue and check
    def getType(lv: LValue): S_TYPE = lv match {
        case LIdent(id) => ??? // Need to find where it is defined
        case ArrElem(_, List(x)) => getType(x)
        case pe: PairElem => getType(pe)
    }

    def isSemCorrect(lv: LValue): Boolean = lv match {
        case LIdent(_) => true
        case ArrElem(_, xs) => isSemCorrect(xs)
        case pe: PairElem => isSemCorrect(pe)
    }

    // Type converter for arrays
    def getType(arr: List[Expr]): S_TYPE = getType(arr.head)

    // RValue type and check
    def getType(rv: RValue): S_TYPE = rv match {
        case RExpr(e) => getType(e)
        case ArrL(xs) => getType(xs)
        case NewPair(e1, e2) => S_PAIR(getType(e1), getType(e2))
        case pe: PairElem => getType(pe)
        case Call(_, xs) => ??? // Not sure
    }

    def isSemCorrect(rv: RValue): Boolean = rv match {
        case RExpr(e) => isSemCorrect(e)
        case ArrL(xs) => isSemCorrect(xs)
        case NewPair(e1, e2) => isSemCorrect(e1) && isSemCorrect(e2)
        case pe: PairElem => isSemCorrect(pe)
        case Call(_, xs) => isSemCorrect(xs)
    }

    // PairElem check 
    def getType(pe: PairElem): S_TYPE = pe match {
        case First(lv) => getType(lv)
        case Second(lv) => getType(lv)
    }
    
    def isSemCorrect(pe: PairElem): Boolean = pe match {
        case First(lv) => isSemCorrect(lv)
        case Second(lv) => isSemCorrect(lv)
    }
}
