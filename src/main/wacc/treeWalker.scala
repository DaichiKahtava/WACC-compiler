package wacc

import scala.collection.mutable.ListBuffer

class TreeWalker(var sem: Semantics, formatter: Aarch64_formatter) {
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    var instructionList = List[Instruction]()



    // Conventions for translate: 
    //  regs contains a list of all available general purpose registers
    //  and the first register is used as the `return` register for the
    //  specific instruction

    def translate(e: Expr, regs: List[Int]): List[Instruction] = {
        // Convention: the result after each expression `translate` will be in regs(0)
        //             that is the primary scratch 

        // regs should be the scratchRegs from the formatter (at least 3!)
        // Although we have registers 8 to 15, we will be using just two 
        // (8 <primary> and 9 <secondary>) 
        // [Potential point of optimisation]

        // KEY CONSIDERATION:
        // With the current arrangment the regs args is not strictly necessary, but is left
        // here for extension purposes.

        val primary   = regs(0)
        val secondary = regs(1)
        val tertiary = regs(2)

        e match {

            // UnOp expressions.
            case Not(x) => translate(x, regs) ++ 
                List(
                    Move(ImmNum(1), RegisterX(secondary)),
                    Compare(RegisterX(primary), RegisterX(secondary)), 
                    SetCond(RegisterX(primary), NeI)
                )

            case Neg(x) => 
                formatter.includeFx(new errorOverFlowFx(formatter))
                translate(x, regs) ++
                List(
                    Move(RegisterX(primary), RegisterX(secondary)),
                    Move(ImmNum(0), RegisterX(primary)), 
                    SubI(RegisterW(secondary), RegisterW(primary)),
                    BranchCond("_errOverflow", VsI)
                )

            case Len(x) => 
                translate(x, regs) ++ 
                List(
                    Move(ImmNum(-4), RegisterX(secondary)),
                    LoadWord(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary))
                )
            
            case Ord(x) => translate(x, regs) 
            case Chr(x) => translate(x, regs) 

            // BinOp expressions.
            


            // TODO: Add over/underflow checks for add and mul
            // TODO: Duplicate code --> Abstraction of binary operators
            case Add(x, y) =>
                formatter.includeFx(new errorOverFlowFx(formatter))
                translateTwoExpr(x, y, regs) ++
                List(
                    AddI(RegisterW(secondary), RegisterW(primary)),
                    BranchCond("_errOverflow", VsI)
                )
            
            case Minus(x, y) =>
                formatter.includeFx(new errorOverFlowFx(formatter))
                translateTwoExpr(x, y, regs) ++
                List(
                    SubI(RegisterW(secondary), RegisterW(primary)),
                    BranchCond("_errOverflow", VsI)
                )
            
            case Mul(x, y) =>
                formatter.includeFx(new errorOverFlowFx(formatter))
                translateTwoExpr(x, y, regs) ++
                List(
                    MulI(RegisterX(secondary), RegisterX(primary)),
                    BranchCond("_errOverflow", NeI)
                )

            case Div(x, y) => {
                translateTwoExpr(x, y, regs) ++
                List(
                    Compare(RegisterXZR, RegisterX(secondary)),
                    BranchCond(formatter.includeFx(new errorDivZeroFx(formatter)), EqI),
                    DivI(RegisterX(secondary), RegisterX(primary))
                )
            }
            case Mod(x, y) => {
                translateTwoExpr(x, y, regs) ++
                List(
                    Compare(RegisterXZR, RegisterX(secondary)),
                    BranchCond(formatter.includeFx(new errorDivZeroFx(formatter)), EqI),
                    Move(RegisterX(primary), RegisterX(tertiary)),  // Store x into another register.
                    DivI(RegisterX(secondary), RegisterX(tertiary)),  // Divide x by y.
                    MulI(RegisterX(secondary), RegisterX(tertiary)),  // Multiply (y * quotient).
                    SubI(RegisterX(tertiary), RegisterX(primary))   // Calculate remainder.
                )
            }

            case GrT(x, y) => 
                translateTwoExpr(x, y, regs) ++
                List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), GtI))

            case GrEqT(x, y) =>
                translateTwoExpr(x, y, regs) ++
                List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), GeI))

            case LsT(x, y) =>
                translateTwoExpr(x, y, regs) ++
                List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), LtI))

            case LsEqT(x, y) =>
                translateTwoExpr(x, y, regs) ++
                List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), LeI))

            case Eq(x, y) =>
                translateTwoExpr(x, y, regs) ++
                List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), EqI))

            case NEq(x, y) =>
                translateTwoExpr(x, y, regs) ++
                List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), NeI))
                
            case And(x, y) =>
                val curLabel = formatter.generateGeneralLabel()
                translateTwoExpr(x, y, regs) ++
                // TODO: ImmNum magic number for true!
                List(
                    Move(ImmNum(1), RegisterX(tertiary)),
                    Compare(RegisterX(primary), RegisterX(tertiary)),
                    BranchCond(curLabel, NeI),
                    Compare(RegisterX(secondary), RegisterX(tertiary)),
                    Label(curLabel),
                    SetCond(RegisterX(primary), EqI)
                )

            case Or(x, y) => 
                val curLabel = formatter.generateGeneralLabel()
                translateTwoExpr(x, y, regs) ++
                List(
                    Move(ImmNum(1), RegisterX(tertiary)),
                    Compare(RegisterX(primary), RegisterX(tertiary)),
                    BranchCond(curLabel, EqI),
                    Compare(RegisterX(secondary), RegisterX(tertiary)),
                    Label(curLabel),
                    SetCond(RegisterX(primary), EqI)
                )

            // Atom expressions.

            // Load the integer directly.
            case IntL(n) => List(Move(ImmNum(n), RegisterX(primary)))

            // Load the boolean value using integers.
            case BoolL(b) => {
                val value = b match {
                    case true  => 1
                    case false => 0 
                }
                List(Move(ImmNum(value), RegisterX(primary)))
            }

            // Obtain the integer value of a character.
            case CharL(c) => List(Move(ImmNum(c.toInt), RegisterX(primary)))
            
            case StrL(s) => {
                val label = formatter.includeString(s)
                return List(Address(label, RegisterX(regs.head)))
            }

            case PairL() => 
                List(Move(ImmNum(0), RegisterX(primary))) // Treat as null pointer?

            // Pattern match for the identifier.
            case Ident(id) => loadContentsFromIdentifier(id, regs)

            // Todo: magic numbers for registers 7, 17
            case ArrElem(id, xs) => {     
                // Finds the pointer to the array
                val extractPtr = sem.curSymTable.findVarGlobal(id).get.pos match {
                    case InRegister(r) => List(Move(RegisterX(r), RegisterX(7)))
                    case OnStack(offset) => List() // Todo
                    case OnTempStack(regNum) => List() // Todo
                    case Undefined => ??? // Should not come here
                }

                var elemType = sem.curSymTable.findVarGlobal(id).get.tp.asInstanceOf[S_ARRAY].tp
                var elemSize = formatter.getSize(elemType)
                println(elemType)
                extractPtr ++ 
                (for (x <- xs) yield {
                    elemSize = formatter.getSize(elemType)
                    formatter.includeFx(new ArrayLoadFx(formatter, elemSize))
                    val res = translate(x, regs) ++ 
                    List(
                        Move(RegisterWR, RegisterW(17)),
                        BranchLink(s"_arrLoad$elemSize")
                        )
                    if (elemType.isInstanceOf[S_ARRAY])
                        elemType = elemType.asInstanceOf[S_ARRAY].tp
                    res
                }).flatten ++ (if (elemSize == 8) List(Move(RegisterX(7), RegisterXR)) else List(Move(RegisterX(7), RegisterXR)))
            }
        }
    }

    // Translates y pushes the value of y into the stack, then translates x and pops
    // the value of y to the secodary scrarch register
    // regs are the scrach registers in similar fashion to translate(e: Expr)
    // The primary regs(0) and secondary regs(1) will store the results. 
    def translateTwoExpr(x: Expr, y: Expr, regs: List[Int]): List[Instruction] = {
        val primary   = regs(0)
        val secondary = regs(1)
        // Note that the second value is calculated first so that it is poped to the secondary
        // while the first value is calculated directly to the primary value
        translate(y, regs) ++ 
        // TODO: Function for pushing and poping
        List(Push(RegisterX(primary), RegisterXZR)) ++
        translate(x, regs) ++
        List(Pop(RegisterX(secondary), RegisterXZR))
    }

    def translate(list: List[Any], regs: List[Int]): List[Instruction] = list match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined list.")
    }

    def storeContentsToIdentifier(id: String, regs: List[Int]): List[Instruction] = {
        // stores the contents from the primary register to where an identifier
        // is considered to be stored
        val primary = regs(0)
        val secondary = regs(1)
        val v = sem.curSymTable.findVarGlobal(id).get
        val destInstr = v.pos match {
        case InRegister(r) => List(Move(RegisterX(primary), RegisterX(r)))
            case OnTempStack(r) => List(
                    Comment("TEMP STACK!!!"),
                    Move(ImmNum(-(formatter.getSize(S_ANY) * (r + 1))), RegisterX(secondary)),
                    Load(BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary)), RegisterX(primary))                        )
            case OnStack(offset) => List(
                    Move(ImmNum(offset), RegisterX(secondary)),
                    Store(RegisterX(primary), BaseOfsRA(RegisterX(formatter.regConf.framePReg), RegisterX(secondary)))
                )                
            case Undefined => ???
            // sem.curSymTable.redefineSymbol(id, VARIABLE(v.tp, InRegister(regs.head)))
            // translate(rv, regs)
        }
        destInstr
    }

    def loadContentsFromIdentifier(id: String, regs: List[Int]): List[Instruction] = {
        val primary = regs(0)
        val secondary = regs(1)        
        sem.curSymTable.findVarGlobal(id).get.pos match {
            case InRegister(r) => List(Move(RegisterX(r), RegisterX(primary)))
            case OnTempStack(r) => List(
                    Comment("TEMP STACK!!!"),
                    Move(ImmNum(-(formatter.getSize(S_ANY) * (r + 1))), RegisterX(secondary)),
                    Load(BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary)), RegisterX(primary))
                )
            case OnStack(offset) => ???
            case Undefined => ??? // Should not get here
        }
    }

    def translate(program: Program): List[Instruction] = {
        val instructionList = ListBuffer.empty[Instruction]
        program.funcs.foreach((f) => {
            // TODO: More idiomatic way of accessing the symbol table?
            sem.curSymTable = sem.curSymTable.findFunGlobal(f.id).get.st // We are in the local symbolTable.
            
            instructionList.addAll(translate(f))
            sem.curSymTable = sem.curSymTable.parent().get // We are in the parent/global symbolTable.
        })
        // TODO: Use blocks of sorts...
        sem.curSymTable.assignPositions(formatter)
        instructionList.addAll(List(Label("main"), Push(RegisterFP, RegisterLR)))
        instructionList ++= translate(program.s)
        instructionList.addAll(List(
            Pop(RegisterFP, RegisterLR),
            Move(ImmNum(0), RegisterX(0)),
            ReturnI
        ))
        instructionList.toList
    }

    def translate(func: Func): List[Instruction] = {
        // Callee saves and callee restore must go here.
        // Labels must be ensured unique
        val instructionList = ListBuffer.empty[Instruction]
        val varAlloc = sem.curSymTable.assignPositions(formatter) // TODO: extra space...
        val primary = formatter.regConf.scratchRegs.head
        instructionList.addAll(
            List(
                Label(formatter.regConf.funcLabel + func.id),
                Push(RegisterFP, RegisterLR),
                Move(RegisterSP, RegisterFP),
                Move(ImmNum(varAlloc), RegisterX(primary)),
                AddI(RegisterX(primary), RegisterSP) // varAlloc is negative
            )
        )
        instructionList.addAll(calleeSave())
        instructionList.addAll(translate(func.s))
        instructionList.toList
    } 

    def translate(stmt: Stmt): List[Instruction] = {
        val scratchRegs = formatter.regConf.scratchRegs
        val primary = scratchRegs(0)
        val secondary = scratchRegs(1)
        stmt match {
            case Skip() => Nil
            case Decl(tp, id, rv) => {
                var charCheck = List[Instruction]()
                if (tp.isInstanceOf[CharT]) {
                    formatter.includeFx(new errorBadCharFx(formatter))
                    charCheck = List(isChar(RegisterX(primary)))
                }
                translate(rv, scratchRegs) ++ charCheck ++ storeContentsToIdentifier(id, scratchRegs)
            }

            case Asgn(LIdent(id), rv) => {
                translate(rv, scratchRegs) ++ storeContentsToIdentifier(id, scratchRegs)
            }
            case Asgn(LArrElem(id, xs), rv) => { // TODO: Nested array extractoin
                /*
                    - use branch link here (as an exception)
                    - arr load/str uses variable registers with callee saves/restore
                    - arr load/str should save result inside X8 <primary>
                    - use the scratch to store arguments
                    ->Considered an an extention to this code.
                */

                // Finds the pointer to the array
                val extractPtr = sem.curSymTable.findVarGlobal(id).get.pos match {
                    case InRegister(r) => List(Move(RegisterX(r), RegisterX(7)))
                    case OnStack(offset) => List() // Todo
                    case OnTempStack(regNum) => List() // Todo
                    case Undefined => ??? // Should not come here
                }

                val elemType = rv.tp
                val elemSize = formatter.getSize(elemType)
                formatter.includeFx(new ArrayStoreFx(formatter, elemSize))

                extractPtr ++
                (xs match {
                    case x :: Nil =>
                        translate(x, scratchRegs) ++ // Index in X8
                        List(Move(RegisterW(primary), RegisterW(17))) ++ // Index in W17
                        translate(rv, scratchRegs) ++ // Value to store in X8
                        List(BranchLink(s"_arrStore$elemSize")) 
                    case x :: xs => Nil // TODO
                    case Nil => ??? // Should not be empty
                })
            }
            case Asgn(lv, rv) => ??? 
            case Read(lv) => ???
            case Free(x) => {
                // Include the errorNullFx internal function in the formatter.
                formatter.includeFx(new errorNullFx(formatter))
                translate(x, scratchRegs) ++ 
                List(
                    // Compare the value in the primary scratch register with zero.
                    Compare(RegisterX(primary), RegisterXZR),
                    // If they are equal (i.e., the pointer is null), branch to the '_errNull' label.
                    BranchCond("_errNull", EqI),

                    /* Move the memory location from the primary scratch register to RegisterX(0).
                       'free' function expects the memory location in RegisterX(0). */
                    Move(RegisterX(0), RegisterX(primary)),
                    // Call the 'free' function to free the memory at the specified location.
                    BranchLink("free")
                    )
            }
            case Return(x) => translate(x, formatter.regConf.scratchRegs) ++ 
                List(
                    Move(RegisterX(formatter.regConf.scratchRegs(0)),
                    RegisterX(formatter.regConf.resultRegister))
                ) ++
                calleeRestore() ++ 
                List(
                    // DEALLOC ALL VARIABLES!!!
                    Move(ImmNum(sem.curSymTable.stackAllocVars), RegisterX(formatter.regConf.scratchRegs.head)),
                    SubI(RegisterX(formatter.regConf.scratchRegs.head), RegisterSP),
                    Pop(RegisterFP, RegisterLR), ReturnI
                )
            case Exit(x) => callFx("exit", formatter.regConf.scratchRegs, Right(List(x)), List(S_INT))
            case Print(x) => callFx(determinePrint(x), formatter.regConf.scratchRegs, Right(List(x)), List(S_ANY))
            case Println(x) => {
                callFx(determinePrint(x), formatter.regConf.scratchRegs, Right(List(x)), List(S_ANY)) ++
                callFx(formatter.includeFx(new printLineFx(formatter)), formatter.regConf.scratchRegs, Left(List()), List())
            }
            case Cond(x, s1, s2) => { // TODO: Defintely need to change that
                val s1Label = formatter.generateGeneralLabel()
                val s2Label = formatter.generateGeneralLabel()
                val instrs = ListBuffer.empty[Instruction]
                instrs.addAll(translate(x, scratchRegs)) 
                instrs.addAll(List(
                    Compare(RegisterX(primary), RegisterXZR), // TODO: Change ResiterXZR
                    BranchCond(s1Label, EqI),
                ))
                // TODO: Make sure that s1 and s2 have access to the parent symbol table but dont have access to each other's
                sem.curSymTable = sem.curSymTable.getNextChildSymbolTable()
                instrs.addAll(calleeSave())
                instrs.addAll(translate(s1))
                instrs.addAll(calleeRestoreLocal())
                sem.curSymTable = sem.curSymTable.parent().get
                instrs.addAll(List(Branch(s2Label), Label(s1Label)))
                sem.curSymTable = sem.curSymTable.getNextChildSymbolTable()
                instrs.addAll(calleeSave())
                instrs.addAll(translate(s2))
                instrs.addAll(calleeRestoreLocal())
                sem.curSymTable = sem.curSymTable.parent().get
                instrs.addOne(Label(s2Label))
                instrs.toList
            }
            case Loop(x, s) => {
                val loopLabel = formatter.generateGeneralLabel()
                val condLabel = formatter.generateGeneralLabel()
                val instrs = ListBuffer.empty[Instruction]
                instrs.addAll(List(
                    Branch(condLabel),
                    Label(loopLabel)
                ))
                sem.curSymTable = sem.curSymTable.getNextChildSymbolTable()
                instrs.addAll(calleeSave())
                instrs.addAll(translate(s))
                instrs.addAll(calleeRestoreLocal())
                sem.curSymTable = sem.curSymTable.parent().get
                instrs.addOne(Label(condLabel))
                instrs.addAll(translate(x, scratchRegs))
                instrs.addAll(List(
                    Compare(RegisterX(primary), RegisterXZR), // TODO: Change ResiterXZR
                    BranchCond(loopLabel, NeI),
                ))
                instrs.toList
            }
            case Body(s) => {
                val instrs = ListBuffer.empty[Instruction]
                sem.curSymTable = sem.curSymTable.getNextChildSymbolTable()
                instrs.addAll(calleeSave())
                instrs.addAll(translate(s))
                instrs.addAll(calleeRestoreLocal())
                sem.curSymTable = sem.curSymTable.parent().get
                instrs.toList
            }

            case Delimit(s1, s2) => translate(s1) ++ translate(s2)
        }
    }

    def translate(lv: LValue, regs: List[Int]): List[Instruction] = lv match {
        // This is responsible for getting the *address* of the lvalue
        case LArrElem(id, xs) => ??? 
        
        case LIdent(id) => loadContentsFromIdentifier(id, regs)
        // NOTE: We get here ONLY when we need to find the address of the id
        // I.e. when the id is a pair or an array.
        // So, we just return their contents
        
        case pe: PairElem => {
            // This gets you the location of the specific pair element.
            val instrs = new ListBuffer[Instruction]

            pe match {
                case First(lv) => {
                    instrs.addAll(translate(lv, regs)) // Address of the pair itself
                }
                case Second(lv) => {
                    instrs.addAll(translate(lv, regs)) // Address of the pair itself 
                    instrs.addAll(List(
                        Move(ImmNum(formatter.getSize(S_ANY)), RegisterX(regs(1))),
                        AddI(RegisterX(regs(1)), RegisterX(regs.head))
                    ))// Address of the offset
                }
            }
            instrs.toList
        }
    }

    def translate(rv: RValue, regs: List[Int]): List[Instruction] = {
        val primary = regs(0)
        val secondary = regs(1)
        rv match {
            case ArrL(xs) => {
                val arrLen = xs.length
                val elemSize = if (arrLen > 0) formatter.getSize(rv.tp.asInstanceOf[S_ARRAY].tp) else 0
                val arrSize = arrLen * elemSize
                var arrHead = 0
                List(Comment(s"$arrLen element array"),
                    Move(ImmNum(arrSize + 4), RegisterX(primary))                
                ) ++
                callFx(formatter.includeFx(new mallocFx(formatter)), formatter.regConf.scratchRegs, Left(List(RegisterX(primary))), List(S_ANY)) ++
                List(Move(RegisterX(formatter.regConf.resultRegister), RegisterX(formatter.regConf.pointerReg)),
                    Move(ImmNum(formatter.getSize(S_INT)), RegisterX(primary)),
                    AddI(RegisterX(primary), RegisterX(formatter.regConf.pointerReg)),
                    Move(ImmNum(xs.length), RegisterX(primary)),
                    Move(ImmNum(-(formatter.getSize(S_INT))), RegisterX(secondary)),
                    Store(RegisterW(primary), BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary)))
                ) ++ (for (x <- xs) yield {
                    val res = translate(x, regs) ++ // translate stores the result in primary by convention
                    List(
                        Move(ImmNum(arrHead), RegisterX(secondary)),
                        Store(
                            if (elemSize < 8) RegisterW(primary) else RegisterX(primary), 
                            BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary))
                        )
                    )
                    arrHead += elemSize
                    res
                }).flatten ++ List(Move(RegisterX(formatter.regConf.pointerReg), RegisterX(primary)))
            }
            case Call(id, xs) => callFx(formatter.regConf.funcLabel ++ id, formatter.regConf.scratchRegs, Right(xs), 
                sem.curSymTable.findFunGlobal(id).get.st.parDict.values.toList.map((v)=> v.tp))
            case RExpr(e) => translate(e, regs)
            
            case NewPair(e1, e2) => {
                val pOfs1 = 0
                val pOfs2 = formatter.getSize(S_ANY)
                    Move(ImmNum(2 * formatter.getSize(S_ANY)), RegisterX(primary))::
                    callFx(formatter.includeFx(new mallocFx(formatter)), formatter.regConf.scratchRegs, Left(List(RegisterX(primary))), List(S_ANY)) ++ 
                    List(Move(RegisterX(primary), RegisterX(formatter.regConf.pointerReg))) ++
                    translate(e1, regs) ++
                    List(
                        Move(ImmNum(pOfs1), RegisterX(secondary)),
                        Store(RegisterX(primary), BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary)))
                        ) ++
                    translate(e2, regs) ++ 
                    List(
                        Move(ImmNum(pOfs2), RegisterX(secondary)),
                        Store(RegisterX(primary), BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary))),
                        Move(RegisterX(formatter.regConf.pointerReg), RegisterX(primary))
                    )
                    // Potential issue: X16 overwritten when dealing with array?
            }
            
            case pe: PairElem => {
                val instrs = new ListBuffer[Instruction]
                // This loads the contents of the pair from either location
                // For the address of the pair for each location, you use the lvalue instance of PairElem.
                var loadPosition = 0
                pe match {
                    case First(lv) => {
                        instrs.addAll(translate(lv, regs))
                        instrs.addOne(Move(ImmNum(0), RegisterX(secondary)))
                    }
                    case Second(lv) => {
                        instrs.addAll(translate(lv, regs))
                        loadPosition = 1
                        instrs.addOne(Move(ImmNum(1), RegisterX(secondary)))
                    }
                }
                
                instrs.addOne(Move(RegisterX(primary), RegisterX(formatter.regConf.argRegs(0))))
                instrs.addAll(callFx(formatter.includeFx(new PairLoadFx(formatter)), regs, Left(List(RegisterX(primary), (RegisterX(secondary)))), List()))

                instrs.toList
            }
        }
    }

    // Gives the correct print label for the expression
    // And adds the required dependencies
    def determinePrint(x: Expr): String = sem.getType(x) match {
        case S_STRING | S_ARRAY(S_CHAR) => formatter.includeFx(new printStringFx(formatter))
        case S_BOOL => formatter.includeFx(new printBoolFx(formatter))
        case S_CHAR => formatter.includeFx(new printCharFx(formatter))
        case S_INT =>  formatter.includeFx(new printIntFx(formatter))
        case S_PAIR(_, _) | S_ERASED => formatter.includeFx(new printPointerFx(formatter)) 
        case S_ARRAY(tp) => formatter.includeFx(new printPointerFx(formatter))
    }

    def pushRegs(regs: List[Int]): List[Instruction] = {
        Comment("Saving registers") :: (for {
            List(r1, r2) <- regs.grouped(2).toList // [em422]
        } yield (Push(RegisterX(r2), RegisterX(r1)))) ++ 
        // This can probably be compacted into above but idk how
        (if (regs.size % 2 != 0) List(Push(RegisterXZR, RegisterX(regs.last))) else Nil) ++
        List(Comment("Saving registers END"))
    }

    def popRegs(regs: List[Int]): List[Instruction] = {
        Comment("Restoring registers") :: 
        (if (regs.size % 2 != 0) List(Pop(RegisterXZR, RegisterX(regs.last))) else Nil) ++ 
        (for {
            List(r1, r2) <- regs.grouped(2).toList.reverse
        } yield (Pop(RegisterX(r2), RegisterX(r1)))) ++ List(Comment("Restoring registers END"))
        // This can probably be compacted into above but idk how
    }

    def callFx(label: String, regs: List[Int], args: Either[List[RegisterX], List[Expr]], parTypes:List[S_TYPE]): List[Instruction] = {
        // It is expected that all arguments will be translated and stored in the register
        // or on the stack with an offset relative to the frame pointer. 

        // CAUTION: THE LIST OF RegisterX MUST NOT CONTAIN ANY ARGUMENT REGISTERS AS THIS MIGHT LEAD
        //          TO OVERWRITTEN REGISTERS
        //          Cannot mix both registers and expressions to avoid ovewriting expressions

        val instrs = ListBuffer.empty[Instruction]

        val primary = regs(0)
        val secondary = regs(1)

        instrs.addOne(Move(RegisterSP, RegisterX(formatter.regConf.stackAlign)))

        instrs.addAll(callerSave())
        // This will save all arguments (if they exist)
        
        var regArgs = List.empty[RegisterX]
        var exprArgs = List.empty[Expr]
        var arglength = 0

        args match {
            case Left(regArgs) => {
                arglength = regArgs.length

                for (i <- 0 to Math.min(7, arglength - 1)) {
                    instrs.addOne(Move(regArgs(i), RegisterX(i)))  
                }
        
                if (arglength > 8) {
                    var ofs = 0
                    for (i <- (arglength - 1) to 8) {
                        instrs.addOne(Push(regArgs(i), RegisterXZR))
                    }
                }

            }
            case Right(exprArgs) => {
                arglength = exprArgs.length
                
                for (i <- 0 to Math.min(7, arglength - 1)) {
                    instrs.addAll(translate(exprArgs(i), formatter.regConf.scratchRegs)) 
                    instrs.addOne(Move(RegisterX(formatter.regConf.scratchRegs.head), RegisterX(i))) 
                }
        
                if (arglength > 8) {
                    var ofs = 0
                    for (i <- (arglength - 1) to 8) {
                        instrs.addAll(translate(exprArgs(i), formatter.regConf.scratchRegs)) 
                        instrs.addOne(Push(RegisterX(formatter.regConf.scratchRegs(0)), RegisterXZR))
                    }
                }
            }
        }

        // To avoid accidentally overwriting registers, we put everything in stack and
        // Then load anything we need from the stack using symbolTable     
        
        

        instrs.addAll(List(
            BranchLink(label),
            // The result will be saved on the x8.
            // TODO: use scratch regs...
            Move(RegisterX(0), RegisterX(primary))
        ))
        if (arglength >= 8) {
            instrs.addAll(List(
                Move(ImmNum((arglength - 8) * 16), RegisterX(secondary)),
                AddI(RegisterX(secondary), RegisterSP)
            ))
        }
        instrs.addAll(callerRestore())

        instrs.toList
    }



    // calle functions save/restore registers, the frame pointer as well as the link register

    def callerSave(): List[Instruction] = {
        // Anonymous functions have NO parameters BUT they
        // need to save the parent's parameters if they have any...

        if (sem.curSymTable.anonymous) {
            var parentStuff = List.empty[Instruction] 
            val childTable = sem.curSymTable
            sem.curSymTable = sem.curSymTable.parent().get // Anonymous symbol tables always have parents
            parentStuff = callerSave()
            sem.curSymTable = childTable
            return parentStuff
        } else {
            val regs = ListBuffer.empty[Int] 
            sem.curSymTable.parDict.foreachEntry((s, v) => {
                v.pos match {
                    case InRegister(r) => {
                        regs.addOne(r)
                        v.pos = OnTempStack(r)
                    }
                    case OnStack(offset) => {} //Nothing :)
                    case OnTempStack(r) => ??? // Invariant: No nested caller/callee saves in the same scope
                    case Undefined => ???
                }
            })
            return pushRegs(regs.toList) // We rely on the preservation of order for LinkedHashMap
        }
    }
    def callerRestore(): List[Instruction] = {
        // Anonymous functions have NO parameters BUT they
        // need to save the parent's parameters if they have any...

        if (sem.curSymTable.anonymous) {
            var parentStuff = List.empty[Instruction] 
            val childTable = sem.curSymTable
            sem.curSymTable = sem.curSymTable.parent().get // Anonymous symbol tables always have parents
            parentStuff = callerRestore()
            sem.curSymTable = childTable
            return parentStuff
        } else {
            val regs = ListBuffer.empty[Int] 
            sem.curSymTable.parDict.foreachEntry((s, v) => {
                v.pos match {
                    case InRegister(r) => ??? // Nothing should be saved in register here!
                    case OnStack(offset) => {} //Nothing :)
                    case OnTempStack(r) => {
                        regs.addOne(r)
                        v.pos = InRegister(r)
                    }
                    case Undefined => ???
                }
            })
            return popRegs(regs.toList)
        }
    }
    
    def calleeSave(): List[Instruction] = {
        // This should be invoked every time we enter a new symbol table
        // I.e. this is LOCAL ONLY!

        // Note that this uses the current symbol table for the variables to be saved
        val regs = ListBuffer.empty[Int] 
        sem.curSymTable.varDict.values.foreach((v) => {
            v.pos match {
                case InRegister(r) => regs.addOne(r)
                case OnStack(offset) => {} //Nothing :)
                case OnTempStack(offset) => ???
                case Undefined => ???
            }
        })
        pushRegs(regs.toList)
    }
    def calleeRestore(): List[Instruction] = {
        // This should be invoked only on returns:
        // It will go to the parent symbol table if anonymous and
        // will do the parent symbol table's callee restore as well.

        //Note that this uses the current symbol table for the variables to be saved
        var parentStuff = List.empty[Instruction] 
        if (sem.curSymTable.anonymous) {
            val childTable = sem.curSymTable
            sem.curSymTable = sem.curSymTable.parent().get // Anonymous symbol tables always have parents
            parentStuff = calleeRestore()
            sem.curSymTable = childTable
        }
        calleeRestoreLocal() ++ parentStuff
        // TODO: Can avoid recalculating registers or abstract that into the funtion translation.
    }

    def calleeRestoreLocal(): List[Instruction] = {
        // This must be used at the end of any anonymous scopes
        // It makes sure that the stuff saves for the anonymous scopes are saved.
        val regs = ListBuffer.empty[Int] 
        sem.curSymTable.varDict.values.foreach((v) => {
            v.pos match {
                case InRegister(r) => regs.addOne(r)
                case OnStack(offset) => {} //Nothing :)
                case OnTempStack(offset) => ???
                case Undefined => ???
            }
        })
        popRegs(regs.toList)
    }


}