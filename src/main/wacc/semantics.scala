package wacc

class Semantics(fileName: String) {

    /// Global pointer/reference to current SymTable
    var curSymTable = new SymTable(None)
    var errorRep = new SemErrorReporter(fileName)

    /// Expressions ///
    def getType(e: Expr): S_TYPE = {
        return e match {
            case IntL(_) => S_INT
            case BoolL(_) => S_BOOL
            case CharL(_) => S_CHAR
            case StrL(_) => S_STRING
            case PairL() => S_PAIR(S_ANY, S_ANY)
            case Ident(id) => curSymTable.findVarGlobal(id).get match {
                case VARIABLE(tp) => tp
            }

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

            case ArrElem(_, xs)  => getType(xs)
        }
    }

    // LValue and check
    def getType(lv: LValue): S_TYPE = lv match {
        case LIdent(id) => curSymTable.findVarGlobal(id).get match {
            case VARIABLE(tp) => tp
        }
        case LArrElem(_, xs) => getType(xs)
        case pe: PairElem => getType(pe)
    }

        // RValue type and check
    def getType(rv: RValue): S_TYPE = rv match {
        case RExpr(e) => getType(e)
        case ArrL(elements) => 
            elements match {
              case Nil => S_ARRAY(S_ANY) // If the array is empty, return S_ARRAY(S_ANY).
              case head :: _ => // Otherwise, return the type of the first element.
                getType(head) match {
                  case S_CHAR => S_STRING // The string weakening rule. TODO: REVIEW!
                  case elementType => S_ARRAY(elementType)
                }
            }
        case NewPair(e1, e2) => S_PAIR(getType(e1), getType(e2))
        case pe: PairElem => getType(pe)
        case Call(id, _) => curSymTable.findFunGlobal(id).get match {
            case FUNCTION(tp) => tp
        }
    }

    // Type converter for arrays
    def getType(arr: List[Expr]): S_TYPE = getType(arr.head)

    def getType(pe: PairElem): S_TYPE = pe match {
        case First(lv) => getType(lv)
        case Second(lv) => getType(lv)
    }
    
    def canWeakenTo(t1: S_TYPE, t2: S_TYPE): Boolean = t1 match {
        case S_ANY  => true
        case S_ERASED => t2.isInstanceOf[S_PAIR]
        case S_PAIR(tp1, tp2) => t2 == S_ERASED
        case S_ARRAY(tp) => tp match {
            case S_CHAR => t2 == S_STRING
            case _ => tp == t2.asInstanceOf[S_ARRAY].tp
        }
        case _ => t1 == t2
    }
    // TODO: We might need to introduce an S_ERASED() for use in Pair semantic checking!

    def toSemanticType(t: Type): S_TYPE = t match {
        case IntT() => S_INT
        case BoolT() => S_BOOL
        case StringT() => S_STRING
        case CharT() => S_CHAR
        case ArrayT(tp) => S_ARRAY(toSemanticType(tp))
        case Pair(pe1, pe2) => S_PAIR(toSemanticType(pe1), toSemanticType(pe2))
    }

    def toSemanticType(t: PairElemType): S_TYPE = t match {
        case IntT() => S_INT
        case BoolT() => S_BOOL
        case StringT() => S_STRING
        case CharT() => S_CHAR
        case ArrayT(tp) => S_ARRAY(toSemanticType(tp))
        case ErasedPair() => S_PAIR(S_ANY, S_ANY)
    }

    def equalType(e1: Expr, e2: Expr) = getType(e1) == getType(e2)

    def isSemCorrect(e: Expr): Boolean = {
        return e match {
            case IntL(_) => true
            case BoolL(_) => true
            case CharL(_) => true
            case StrL(_) => true
            case PairL() => true

            case Ident(id) => curSymTable.varDefinedGlobal(id)

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
            case Eq(x1, x2) => isSemCorrect(x1) && isSemCorrect(x2) && getType(x1) == getType(x2)
            case NEq(x1, x2) => isSemCorrect(x1) && isSemCorrect(x2) && getType(x1) != getType(x2)
            
            case And(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_BOOL) && (isSemCorrect(x2) && getType(x2) == S_BOOL)
            case Or(x1, x2) => (isSemCorrect(x1) && getType(x1) == S_BOOL) && (isSemCorrect(x2) && getType(x2) == S_BOOL)

            case ArrElem(_, xs)  => isSemCorrect(xs)

            case _ => false // Default case due to compiler warning.
        }
    }

    // Semantics of a list of AST nodes..... don't know how to use generics so is a list of Any for now :(
    def isSemCorrect(list: List[Any]): Boolean = list match {
        case Nil => true
        case (e: Expr) :: es => isSemCorrect(e) && isSemCorrect(es)
        case _ => false
    }


    
    /// Statements ///
    def isSemCorrect(program: Program): Boolean = {
        // // Populate symbol table for ALL functions first
        // program.funcs.foreach((f) =>
        //     {
        //         var existingF = curSymTable.findGlobal(f.id)
        //         if (existingF.isDefined) {
        //             errorRep.addError("Function \""+f.id+"\" is already defined more than once!", (0,0))
        //             if (getType(f.tp) != )
        //             redefineSymbol(f.id, existingF.get.)
        //         }
        //     }
        // )
        // program.funcs.foreach((f) => {
        //     curSymTable = curSymTable.findGlobal(f.id).get.asInstanceOf[FUNCTION].st
        //     f.params.foreach((p) => {
        //         if (!curSymTable.addSymbol(p.id, VARIABLE(toSemanticType(p.tp)))) {
        //             errorRep.addError("Semantic error: Parameter \""+p.id+"\" is already defined!", (0,0))
        //         }
        //     })
        //     isSemCorrect(f.stmt)
        //     if (toSemanticType(f.tp) != getReturnType(f.stmt).get) {
        //         return false
        //     }
        //     curSymTable = curSymTable.parent().get
        // })
        // return isSemCorrect(program.stmt)
        return true
    }
    


    def getReturnType(stmt: Stmt): Option[S_TYPE] = stmt match {
        case Return(e) => Some(getType(e))
        case Cond(_, s1, s2) => getReturnType(s1).orElse(getReturnType(s2))
        case Loop(_, s) => getReturnType(s)
        case Body(s) => getReturnType(s)
        case Delimit(_, s) => getReturnType(s)
        case _ => None
    }

    // TODO: Param using symbol table
        
    def isSemCorrect(stmt: Stmt): Boolean = stmt match {
            case Skip => true
            case Decl(tp, id, rv) => {
                if (!curSymTable.addSymbol(id, VARIABLE(toSemanticType(tp)))) {
                    println("Semantic error: Parameter \""+id+"\" is already a used identifier!")
                    return false 
                }
                return canWeakenTo(getType(rv), toSemanticType(tp)) && isSemCorrect(rv)
            }
            case Asgn(lv, rv) => isSemCorrect(lv) && isSemCorrect(rv) && canWeakenTo(getType(rv), getType(lv))
            case Read(lv) => getType(lv) == S_CHAR || getType(lv) == S_INT

            case Free(ArrElem(_, xs)) => isSemCorrect(xs)
            case Free(x: PairElem) => isSemCorrect(x.asInstanceOf[PairElem])
            case Free (_) => false

            case Return(x) => isSemCorrect(x)
            case Exit(x) => getType(x) == S_INT
            case Print(x) => isSemCorrect(x)
            case Println(x) => isSemCorrect(x)
            case Cond(x, s1, s2) => getType(x) == S_BOOL && isSemCorrect(s1) && isSemCorrect(s2)
            case Loop(x, stmt) => getType(x) == S_BOOL && isSemCorrect(stmt)
            case Body(stmt) => {
                curSymTable = curSymTable.newUnamedScope()
                val res = isSemCorrect(stmt)
                curSymTable = curSymTable.parent().get
                return res
            }
            case Delimit(s1, s2) => isSemCorrect(s1) && isSemCorrect(s2)
        }
  

    def isSemCorrect(lv: LValue): Boolean = lv match {
        case LIdent(_) => true
        case LArrElem(_, xs) => isSemCorrect(xs)
        case pe: PairElem => isSemCorrect(pe)
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

    // Finds the lowest common ancestor in a list of S_TYPEs.
    def getLowestCommonAncestor(types: List[S_TYPE]): S_TYPE = {
    
      // Finds the common ancestor between 2 items. // CA = Common Ancestor.
      def findCommonAncestor(t1: S_TYPE, t2: S_TYPE): S_TYPE = (t1, t2) match {
        case (S_ANY, _) | (_, S_ANY) => S_ANY // If either is S_ANY, the CA is S_ANY.#
        // Case for two pairs, where we recursively find their CA.
        case (S_PAIR(tp11, tp12), S_PAIR(tp21, tp22)) => 
              S_PAIR(findCommonAncestor(tp11, tp21), findCommonAncestor(tp12, tp22))
        // Case for when one type is a pair, and the other is not.
        case (S_PAIR(_, _), _) | (_, S_PAIR(_, _)) => S_ANY
        // If either is a string, the CA is a string.
        case (S_STRING, S_ARRAY(S_CHAR)) | (S_ARRAY(S_CHAR), S_STRING) => S_STRING
        // Recusively finds the CA if they are both arrays.
        case (S_ARRAY(ta1), S_ARRAY(ta2)) => S_ARRAY(findCommonAncestor(ta1, ta2))
        case (_, _) if t1 == t2 => t1 // If they are equal, the type is either.
        case (_, _) => S_ANY // The default case if no match.
      }
      
      // Option is used in the case the list is empty, where then S_ANY is given.
      types.reduceLeftOption((t1, t2) => findCommonAncestor(t1, t2)).getOrElse(S_ANY)
    }
}
