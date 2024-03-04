package wacc

class Semantics(fileName: String) {

    /// Global pointer/reference to current SymTable
    var curSymTable = new SymTable(None, None, false)
    var errorRep = new SemErrorReporter(fileName)

    /// Expressions ///
    def getType(e: Expr): S_TYPE = {
        return e match {
            case IntL(_) => S_INT
            case BoolL(_) => S_BOOL
            case CharL(_) => S_CHAR
            case StrL(_) => S_STRING
            case PairL() => S_ERASED
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

            // This works for now but breaks the assumption that the variable must exist when getType is invoked
            case ArrElem(id, _)  => curSymTable.findVarGlobal(id) match {
                case Some(VARIABLE(S_ARRAY(tp))) => {
                    var returnTp = tp
                    while (returnTp.isInstanceOf[S_ARRAY]) {
                        returnTp = returnTp.asInstanceOf[S_ARRAY].tp
                    }
                    returnTp
                }
                case _ => {
                    errorRep.addError("Variable \""+id+"\" is not an array!", e.pos)
                    S_ANY
                }
            }
            // asInstanceOf[S_ARRAY].tp
        }
    }

    // LValue and check
    def getType(lv: LValue): S_TYPE = lv match {
        // This assumes that lv has been semantically checked first.
        // So the idenftifiers should be in the current symbolTable
        case LIdent(id) => curSymTable.findVarGlobal(id).get match { // id always a variable
            case VARIABLE(tp) => tp
        }
        case LArrElem(id, _) => curSymTable.findVarGlobal(id).get.tp.asInstanceOf[S_ARRAY].tp
        case pe: PairElem => getType(pe)
    }

    // RValue type and check
    def getType(rv: RValue): S_TYPE = {
        rv.tp = rv match {
            case RExpr(e) => getType(e)
            case ArrL(elements) =>
                elements match {
                case Nil => S_EMPTYARR // If the array is empty, return S_ARRAY(S_ANY).
                case es => // Otherwise, return the type of the first element.
                    // getType(head) match {
                    //   case S_CHAR => {S_STRING} // The string weakening rule. TODO: REVIEW!
                    //   case elementType => S_ARRAY(elementType)
                    // }
                    S_ARRAY(getLowestCommonAncestor(elements.map(getType(_))))
                }
            case NewPair(e1, e2) => S_PAIR(getType(e1), getType(e2))
            case pe: PairElem => getType(pe)
            case Call(id, _) => curSymTable.findFunGlobal(id).get match {
                case FUNCTION(tp) => tp
            }
        }
        rv.tp
    }

    // Type converter for arrays
    def getType(arr: List[Expr]): S_TYPE = getType(arr.head)

    // Assume pe is recursively accessing a pair (other types are not considered)
    def getType(pe: PairElem): S_TYPE = pe match {
        case First(lv: LIdent) => curSymTable.findVarGlobal(lv.id).get.tp.asInstanceOf[S_PAIR].tp1
        case First(lv: LArrElem) => curSymTable.findVarGlobal(lv.id).get.tp.asInstanceOf[S_ARRAY].tp.asInstanceOf[S_PAIR].tp1
        case First(lv: PairElem) => getType(lv) match {
            case S_PAIR(tp1, tp2) => tp1
            case S_ERASED => S_ANY
            case tp => tp  // TODO: Should never happen!
        }
        
        case Second(lv: LIdent) => curSymTable.findVarGlobal(lv.id).get.tp.asInstanceOf[S_PAIR].tp2
        case Second(lv: LArrElem) => curSymTable.findVarGlobal(lv.id).get.tp.asInstanceOf[S_ARRAY].tp.asInstanceOf[S_PAIR].tp2
        case Second(lv: PairElem) => getType(lv) match {
            case S_PAIR(tp1, tp2) => tp2
            case S_ERASED => S_ANY
            case tp => tp
        }
    }
    
    // Target has a more general type
    def canWeakenTo(source: S_TYPE, target: S_TYPE): Boolean = target match {
        case S_ANY  => true
        case S_ERASED => source.isInstanceOf[S_PAIR]
        // case S_PAIR(S_ERASED, ttp2) => source match {
        //     case S_PAIR(S_PAIR(_, _), stp2) => stp2 == ttp2 
        //     case S_PAIR(stp1, stp2) => stp1 == S_ERASED && stp2 == ttp2 
        //     case S_ERASED => true
        //     case _ => false
        // }
        // case S_PAIR(ttp1, S_ERASED) => source match {
        //     case S_PAIR(stp1, S_PAIR(_, _)) => stp1 == ttp1 
        //     case S_PAIR(stp1, stp2) => stp2 == S_ERASED && stp1 == ttp1 
        //     case S_ERASED => true
        //     case _ => false
        // }
        case S_PAIR(ttp1, ttp2) => source match {
            //pairs invariant in type parameters (no use of canWeakenTo here!)
            case S_ERASED => true
            case S_PAIR(stp1, stp2) => {
                var fstwk = ttp1 == stp1
                var sndwk = ttp2 == stp2
                if(ttp1 == S_ERASED) {fstwk = (stp1 == S_ERASED || stp1.isInstanceOf[S_PAIR])}
                if(ttp2 == S_ERASED) {sndwk = (stp2 == S_ERASED || stp2.isInstanceOf[S_PAIR])}
                if(ttp1.isInstanceOf[S_PAIR]) {fstwk = (stp1 == S_ERASED || stp1.isInstanceOf[S_PAIR])} // TODO: Review?
                if(ttp2.isInstanceOf[S_PAIR]) {sndwk = (stp2 == S_ERASED || stp2.isInstanceOf[S_PAIR])}
                fstwk && sndwk
            }
            case _ => false
        }
        case S_ARRAY(ttp) => source match {
            case S_ARRAY(stp) => {
                if(ttp == S_ERASED) {stp == S_ERASED || stp.isInstanceOf[S_PAIR]}
                if(ttp.isInstanceOf[S_PAIR]) {stp == S_ERASED || stp.isInstanceOf[S_PAIR]}
                else {stp == ttp}
            }
            case S_EMPTYARR => true
            case _ => false
        }
        case S_STRING => source == S_STRING || source == S_ARRAY(S_CHAR)
        case _ => target == source
    }
    // TODO: We might need to introduce an S_ERASED() for use in Pair semantic checking!

    def checkCompatible(source: S_TYPE, target: S_TYPE, pos: (Int, Int)): Boolean = {
        val res = canWeakenTo(source, target)
        if (!res) {
            errorRep.addIncompTypes(source: S_TYPE, target: S_TYPE, pos: (Int, Int))
        }
        return res
    }

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
        case ErasedPair() => S_ERASED
    }

    def equalType(got: S_TYPE, expected: S_TYPE, pos: (Int, Int)): Boolean = {
        if (got != expected) {
            errorRep.addTypeMismatch(got, expected, pos)
            return false
        } else {
            return true
        }
    }

    def isComparable(t1: S_TYPE, t2: S_TYPE): Boolean = {
        // t1 or t2 are not any
        t1 match {
            case S_INT => t2 == S_INT
            case S_STRING => t2 == S_STRING
            case S_CHAR => t2 == S_CHAR
            case S_BOOL => t2 == S_BOOL
            case S_ERASED => t2.isInstanceOf[S_PAIR] || t2 == S_ERASED
            case S_EMPTYARR => t2.isInstanceOf[S_ARRAY] || t2 == S_EMPTYARR
            case S_ARRAY(tp) => t2 match {
                case S_ARRAY(tp1) => isComparable(tp, tp1)
                case S_EMPTYARR => true
                case _ => false
            }
            case S_PAIR(tp1, tp2) => t2 match {
                case S_ERASED => true
                case S_PAIR(tp11, tp21) => isComparable(tp1, tp11) && isComparable(tp2, tp21)
                case _ => false
            }
            case S_ANY => false
        }
    }

    def comparable(t1: S_TYPE, t2: S_TYPE, pos: (Int, Int)): Boolean = {
        // t1 or t2 are not any
        if (!isComparable(t1, t2)) {
            errorRep.addUncomparable(t1, t2, pos)
            return false
        } else {
            return true
        }
    }

    def isOneFrom(got: S_TYPE, expected: List[S_TYPE], pos: (Int, Int)): Boolean = {
        if (!expected.contains(got)) {
            errorRep.addTypeMismatch(got, expected, pos)
            return false
        } else {
            return true
        }
    }

    def isSemCorrect(e: Expr): Boolean = {
        return e match {
            case IntL(_) => true
            case BoolL(_) => true
            case CharL(_) => true
            case StrL(_) => true
            case PairL() => true

            case Ident(id) => {
                // println(curSymTable.varDefinedGlobal(id))
                if (curSymTable.varDefinedGlobal(id)) {
                    return true
                } else {
                    errorRep.addError("Variable \""+id+"\" is not defined!", e.pos)
                    return false
                }
            
            }

            case Not(x) => isSemCorrect(x) && equalType(getType(x), S_BOOL, x.pos)
            case Neg(x) => isSemCorrect(x) && equalType(getType(x), S_INT, x.pos)
            case Len(x) => {
                val isc = isSemCorrect(x)
                println(getType(x))
                isc && getType(x).isInstanceOf[S_ARRAY]
            } // TODO: Use canWeakenTo
            case Ord(x) => isSemCorrect(x) && equalType(getType(x), S_CHAR, x.pos)
            case Chr(x) => isSemCorrect(x) && equalType(getType(x), S_INT, x.pos)

            // TODO: Better way to combine these? <- DUPLICATE CODE!
            case Mul(x1, x2) => (isSemCorrect(x1) && equalType(getType(x1), S_INT, x1.pos)) && (isSemCorrect(x2) && equalType(getType(x2), S_INT, x2.pos))
            case Div(x1, x2) => (isSemCorrect(x1) && equalType(getType(x1), S_INT, x1.pos)) && (isSemCorrect(x2) && equalType(getType(x2), S_INT, x2.pos))
            case Mod(x1, x2) => (isSemCorrect(x1) && equalType(getType(x1), S_INT, x1.pos)) && (isSemCorrect(x2) && equalType(getType(x2), S_INT, x2.pos))
            case Add(x1, x2) => (isSemCorrect(x1) && equalType(getType(x1), S_INT, x1.pos)) && (isSemCorrect(x2) && equalType(getType(x2), S_INT, x2.pos))
            case Minus(x1, x2) => (isSemCorrect(x1) && equalType(getType(x1), S_INT, x1.pos)) && (isSemCorrect(x2) && equalType(getType(x2), S_INT, x2.pos))

            case GrT(x1, x2)   => isSemCorrect(x1) && isSemCorrect(x2) && (isOneFrom(getType(x1), List(S_INT, S_CHAR), x1.pos)) && equalType(getType(x2), getType(x1), x2.pos)
            case GrEqT(x1, x2) => isSemCorrect(x1) && isSemCorrect(x2) && (isOneFrom(getType(x1), List(S_INT, S_CHAR), x1.pos)) && equalType(getType(x2), getType(x1), x2.pos)
            case LsT(x1, x2)   => isSemCorrect(x1) && isSemCorrect(x2) && (isOneFrom(getType(x1), List(S_INT, S_CHAR), x1.pos)) && equalType(getType(x2), getType(x1), x2.pos)
            case LsEqT(x1, x2) => isSemCorrect(x1) && isSemCorrect(x2) && (isOneFrom(getType(x1), List(S_INT, S_CHAR), x1.pos)) && equalType(getType(x2), getType(x1), x2.pos)
            
            // TODO: Rewrite using Use canWeakenTo
            case ex@Eq(x1, x2) => isSemCorrect(x1) && isSemCorrect(x2) && comparable(getType(x2), getType(x1), x2.pos)
            case ex@NEq(x1, x2) => isSemCorrect(x1) && isSemCorrect(x2) && comparable(getType(x2), getType(x1), x2.pos)
            
            case And(x1, x2) => (isSemCorrect(x1) && equalType(getType(x1), S_BOOL, x1.pos)) && (isSemCorrect(x2) && equalType(getType(x2), S_BOOL, x2.pos))
            case Or(x1, x2) => (isSemCorrect(x1) && equalType(getType(x1), S_BOOL, x1.pos)) && (isSemCorrect(x2) && equalType(getType(x2), S_BOOL, x2.pos))

            case ArrElem(_, xs)  => isSemCorrect(xs) && (xs.foldLeft(true)((b, x) => b && equalType(getType(x), S_INT, x.pos)))
        }
    }

    // Semantics of a list of AST nodes..... don't know how to use generics so is a list of Any for now :(
    def isSemCorrect(list: List[Any]): Boolean = list match {
        case Nil => true
        case (e: Expr) :: es => isSemCorrect(e) && isSemCorrect(es)
        case _ => false
    }


    
    /// Statements ///
    protected [wacc] def isSemCorrect(program: Program): Boolean = {
        // Populate symbol table for ALL functions first
        program.funcs.foreach((f) =>
            {
                var existingF = curSymTable.findFunGlobal(f.id)
                var fType = toSemanticType(f.tp)
                if (existingF.isDefined) {
                    errorRep.addError("Function \""+f.id+"\" is already defined more than once!", f.pos)
                    if (existingF.get.tp != toSemanticType(f.tp)) {
                        curSymTable.redefineSymbol(f.id, FUNCTION(S_ANY)(new SymTable(Some(curSymTable), Some(S_ANY), false)))
                    }

                } else {
                    val fSynT = new SymTable(Some(curSymTable), Some(fType), false)
                    f.params.foreach((p) => {
                        if (!fSynT.addParam(p.id, VARIABLE(toSemanticType(p.tp)))) {
                            errorRep.addError("Parameter \""+p.id+"\" is already defined!", p.pos)
                        }
                    })                
                    curSymTable.addSymbol(f.id, FUNCTION(fType)(fSynT))
                }
            }
        )
        program.funcs.foreach((f) => {
            curSymTable = curSymTable.findFunGlobal(f.id).get.st // We are in the local symbolTable
            isSemCorrect(f.s)
            checkReturning(f.s, f.pos) // Needed to check the existence of a return statement
            curSymTable = curSymTable.parent().get // We are in the parent/global symbolTable
        })
        val res = isSemCorrect(program.s)
        printAccumulatedErrors()
        return (errorRep.numErrors == 0) && res
    }
    


    def checkReturning(stmt: Stmt, pos:(Int, Int)): Unit = stmt match {
        case Return(e) => {}
        case c@Cond(_, s1, s2) => {
            checkReturning(s1, c.pos)
            checkReturning(s2, c.pos)}
        case l@Loop(_, s) => checkReturning(s, l.pos)
        case b@Body(s) => checkReturning(s, b.pos)
        case Delimit(_, s) => checkReturning(s, pos)
        case _ => errorRep.addError("Statement is not returning", pos)
    }

    // TODO: Param using symbol table
        
    def isSemCorrect(stmt: Stmt): Boolean = stmt match {
        case Skip() => true
        case d@Decl(tp, id, rv) => {
            if (!curSymTable.addSymbol(id, VARIABLE(toSemanticType(tp)))) {
                errorRep.addError("Variable \""+id+"\" is already defined previously!\n" +
                    "(Subsequent checks will assume an arbitrary type for "+id+")", d.pos)
                curSymTable.redefineSymbol(id, VARIABLE(S_ANY))
                isSemCorrect(rv)
                return false 
            }
            if (isSemCorrect(rv)) {
                val curType = getType(rv)
                rv match {
                    case RExpr(ArrElem(id, xs)) => checkCompatible(curType, toSemanticType(tp), d.pos)
                    case pe: PairElem if curType == S_ANY => true
                    case _ => checkCompatible(curType, toSemanticType(tp), d.pos)
                }
            } else {
                false
            }
            
        }

        case Asgn(lv: LIdent, rv) => isSemCorrect(lv) && isSemCorrect(rv) &&
            checkCompatible(getType(rv), curSymTable.findVarGlobal(lv.id).get.tp, lv.pos)
        case Asgn(lv: LArrElem, rv) => isSemCorrect(lv) && isSemCorrect(rv) && 
            checkCompatible(getType(rv), getType(lv), lv.pos)
        case Asgn(lv: PairElem, rv) => {
            if (getType(lv) == S_ANY && getType(rv) == S_ANY) {
                errorRep.addError("Both sides of an assignment cannot be of unknown type.", lv.pos)
                false
            } else {
                isSemCorrect(lv) && isSemCorrect(rv) && checkCompatible(getType(rv), getType(lv), lv.pos)
            }
        }

        case Read(lv: LIdent) => isOneFrom(curSymTable.findVarGlobal(lv.id).get.tp, List(S_CHAR, S_INT), lv.pos)
        case Read(lv: LArrElem) => isOneFrom(getType(lv), List(S_CHAR, S_INT), lv.pos)
        case Read(lv: PairElem) => isOneFrom(getType(lv), List(S_CHAR, S_INT), lv.pos)

        // case Free(ArrElem(_, xs)) => isSemCorrect(xs)
        // case Free(x: PairElem) => isSemCorrect(x.asInstanceOf[PairElem])
        // case Free (_) => false

        case f@Free(x) => getType(x) match {
            case S_ERASED | S_PAIR(_, _) | S_ARRAY(_) => true
            case _ => {
                errorRep.addError("A free statement can only accept arrays or pairs", f.pos)
                return false
            }
        }

        case r@Return(x) => {
            if(isSemCorrect(x)){
            curSymTable.getReturnType() match {
                case Some(tp) => {            
                    if (!canWeakenTo(getType(x), tp)) {
                    // TODO: Add error message for return type inconsistencies in the reporter
                        errorRep.addError("Return expression type does not match function's return type.", r.pos)
                        return false
                    }
                    return true
                }
                case None => {
                    errorRep.addError("Return statement outside of function", r.pos)
                    return false
                }
            }
        }
        else{
            return false
        }
        }
        case Exit(x) => equalType(getType(x), S_INT, x.pos)
        case Print(x) => isSemCorrect(x)
        case Println(x) => isSemCorrect(x)
        case Cond(x, s1, s2) => {
            if(isSemCorrect(x)){
                var res = equalType(getType(x), S_BOOL, x.pos)
                curSymTable = curSymTable.newUnamedScope()
                res = res && isSemCorrect(s1) 
                curSymTable = curSymTable.parent().get
                curSymTable = curSymTable.newUnamedScope()
                res = res && isSemCorrect(s2) 
                curSymTable = curSymTable.parent().get
                return res
            }
            return false
        }
        case Loop(x, stmt) => {
            if(isSemCorrect(x)){
                var res = equalType(getType(x), S_BOOL, x.pos)
                curSymTable = curSymTable.newUnamedScope()
                res = res && isSemCorrect(stmt) 
                curSymTable = curSymTable.parent().get
                return res
            }
            return false
            
        }
        case Body(stmt) => {
            curSymTable = curSymTable.newUnamedScope()
            val res = isSemCorrect(stmt)
            curSymTable = curSymTable.parent().get
            return res
        }
        case Delimit(s1, s2) => {
            val cs1 = isSemCorrect(s1)
            val cs2 = isSemCorrect(s2)
            cs1 && cs2
        }
    }
  

    def isSemCorrect(lv: LValue): Boolean = lv match {
        case lv@LIdent(id) => {
            if (curSymTable.findVarGlobal(lv.id).isEmpty) {
                errorRep.addError("Unrecognised variable: ", lv.pos)
                false
            }
            else {
                true
            }        
        }
        case l@LArrElem(id, xs) => isSemCorrect(xs) && (curSymTable.findVarGlobal(id) match {
            case Some(VARIABLE(S_ARRAY(_))) => true 
            case Some(VARIABLE(_)) => {
                errorRep.addError("Not an array: \"" + id + "\"", l.pos)
                false
            }
            case None => {
                errorRep.addError("Array does not exist: \"" + id + "\"", l.pos)
                false
            }
        })
        case pe: PairElem => isSemCorrect(pe)
    }


    def isSemCorrect(rv: RValue): Boolean = rv match {
        case RExpr(e) => isSemCorrect(e)
        case ArrL(xs) => isSemCorrect(xs)
        case NewPair(e1, e2) => isSemCorrect(e1) && isSemCorrect(e2)
        case pe: PairElem => isSemCorrect(pe)
        case c@Call(id, xs) => {
            val f = curSymTable.findFunGlobal(id)
            isSemCorrect(xs)
            if (f.isEmpty) {
                errorRep.addError("Function \""+id+"\" is not defined!", c.pos)
                return false
            } else {
                // return false          
                val fType = f.get.tp
                val fSymTable = f.get.st
                val fParams = fSymTable.parDict.values.toList
                if (xs.length != fParams.length) {
                    errorRep.addError("Function \""+id+"\" expects "+fParams.length+" parameters, but "+xs.length+" were given!", c.pos)
                    return false
                }
                val res = xs.zip(fParams).foldLeft(true)((b, p) => b && checkCompatible(getType(p._1), p._2.tp, p._1.pos))
                return res      
            }
            
            
            
            
            
        } // TODO: Fully saturated!s
    }

    // PairElem check
    def isSemCorrect(pe: PairElem): Boolean = pe match {
        case First(lv: LIdent) => curSymTable.findVarGlobal(lv.id) match {
            case Some(VARIABLE(tp)) => tp.isInstanceOf[S_PAIR]
            case _ => false
        }
        case First(lv: LArrElem) => {
            isSemCorrect(lv) && 
            getType(lv).isInstanceOf[S_PAIR] && 
            (curSymTable.findVarGlobal(lv.id) match {
                case Some(VARIABLE(tp)) => tp.isInstanceOf[S_ARRAY] && tp.asInstanceOf[S_ARRAY].tp.isInstanceOf[S_PAIR]
                case _ => false
            })
        }
        case First(lv: PairElem) => isSemCorrect(lv)

        case Second(lv: LIdent) => curSymTable.findVarGlobal(lv.id) match {
            case Some(VARIABLE(tp)) => tp.isInstanceOf[S_PAIR]
            case _ => false
        }
        case Second(lv: LArrElem) => isSemCorrect(lv) && getType(lv.xs).isInstanceOf[S_PAIR] && 
            (curSymTable.findVarGlobal(lv.id) match {
                case Some(VARIABLE(tp)) => tp.isInstanceOf[S_ARRAY] && tp.asInstanceOf[S_ARRAY].tp.isInstanceOf[S_PAIR]
                case _ => false
            })
        case Second(lv: PairElem) => isSemCorrect(lv)
    }

    // Finds the lowest common ancestor in a list of S_TYPEs.
    def getLowestCommonAncestor(types: List[S_TYPE]): S_TYPE = {
        
        // Finds the common ancestor between 2 items. // CA = Common Ancestor.
        def findCommonAncestor(t1: S_TYPE, t2: S_TYPE): S_TYPE = (t1, t2) match {
            case (S_ANY, _) | (_, S_ANY) => S_ANY // If either is S_ANY, the CA is S_ANY.
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

    def printAccumulatedErrors() = {
        print(errorRep.toString())
    }
}
