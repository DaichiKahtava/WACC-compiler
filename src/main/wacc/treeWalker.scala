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
        val instructionList = ListBuffer.empty[Instruction]

        e match {

            // UnOp expressions.
            case Not(x) => {
                instructionList ++= translate(x, regs)
                instructionList ++= List(
                    Move(ImmNum(1), RegisterX(secondary)),
                    Compare(RegisterX(primary), RegisterX(secondary)), 
                    SetCond(RegisterX(primary), NeI)
                )
            }

            case Neg(x) => {
                formatter.includeFx(new errorOverFlowFx(formatter))
                instructionList ++= translate(x, regs)
                instructionList ++= List(
                    Move(RegisterX(primary), RegisterX(secondary)),
                    Move(ImmNum(0), RegisterX(primary)), 
                    SubI(RegisterW(secondary), RegisterW(primary)),
                    BranchCond("_errOverflow", VsI)
                )
            }

            case Len(x) => {
                instructionList ++= translate(x, regs)
                instructionList ++= List(
                    Move(ImmNum(-(formatter.getSize(S_INT))), RegisterX(secondary)),
                    LoadWord(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary))
                )
            }
            
            case Ord(x) => instructionList ++= translate(x, regs) 
            case Chr(x) => instructionList ++= translate(x, regs) 

            // BinOp expressions.
            


            // TODO: Add over/underflow checks for add and mul
            // TODO: Duplicate code --> Abstraction of biinary operators
            case Add(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(AddI(RegisterX(secondary), RegisterX(primary)))
            }
            
            case Minus(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(SubI(RegisterX(secondary), RegisterX(primary)))
            }
            
            case Mul(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(MulI(RegisterX(secondary), RegisterX(primary)))
            }

            case Div(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= divMod(false, regs)
            }

            case Mod(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= divMod(true, regs)
                instructionList ++= List(
                    // Msub in aarch64
                    MulI(RegisterX(secondary), RegisterX(tertiary)),  // Multiply (y * quotient).
                    SubI(RegisterX(tertiary), RegisterX(primary))   // Calculate remainder.
                )
            }

            case GrT(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), GtI))
            }

            case GrEqT(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), GeI))
            }

            case LsT(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), LtI))
            }

            case LsEqT(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), LeI))
            }

            case Eq(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), EqI))
            }

            case NEq(x, y) => {
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(Compare(RegisterX(primary), RegisterX(secondary)), SetCond(RegisterX(primary), NeI))
            }
                
            case And(x, y) => {
                val curLabel = formatter.generateGeneralLabel()
                instructionList ++= translateTwoExpr(x, y, regs)
                // TODO: ImmNum magic number for true!
                instructionList ++= List(
                    Move(ImmNum(1), RegisterX(tertiary)),
                    Compare(RegisterX(primary), RegisterX(tertiary)),
                    BranchCond(curLabel, NeI),
                    Compare(RegisterX(secondary), RegisterX(tertiary)),
                    Label(curLabel),
                    SetCond(RegisterX(primary), EqI)
                )
            }

            case Or(x, y) => {
                val curLabel = formatter.generateGeneralLabel()
                instructionList ++= translateTwoExpr(x, y, regs)
                instructionList ++= List(
                    Move(ImmNum(1), RegisterX(tertiary)),
                    Compare(RegisterX(primary), RegisterX(tertiary)),
                    BranchCond(curLabel, EqI),
                    Compare(RegisterX(secondary), RegisterX(tertiary)),
                    Label(curLabel),
                    SetCond(RegisterX(primary), EqI)
                )
            }

            // Atom expressions.

            // Load the integer directly.
            case IntL(n) => instructionList ++= List(Move(ImmNum(n), RegisterX(primary)))

            // Load the boolean value using integers.
            case BoolL(b) => {
                val value = b match {
                    case true  => 1
                    case false => 0 
                }
                instructionList ++= List(Move(ImmNum(value), RegisterX(primary)))
            }

            // Obtain the integer value of a character.
            case CharL(c) => instructionList ++= List(Move(ImmNum(c.toInt), RegisterX(primary)))
            
            case StrL(s) => {
                val label = formatter.includeString(s)
                instructionList ++= List(Address(label, RegisterX(regs.head)))
            }

            case PairL() => instructionList ++= List(Move(ImmNum(0), RegisterX(primary))) // Treat as null pointer?

            // Pattern match for the identifier.
            case Ident(id) => instructionList ++= loadContentsFromIdentifier(id, regs)

            case ArrElem(id, xs) => {                
                instructionList.addAll(loadContentsFromIdentifier(id, regs)) // Address
                instructionList.addOne(Push(RegisterX(primary), RegisterXZR))

                var currentType = sem.curSymTable.findVarGlobal(id).get.tp
                    for (i <- 0 to xs.length - 1) {
                        currentType = currentType.asInstanceOf[S_ARRAY].tp
                        var elemSize = formatter.getSize(currentType)
                        instructionList.addAll(translate(xs(i), regs)) // Index in primary
                        instructionList.addAll(List(
                            Move(ImmNum(elemSize), RegisterX(secondary)),
                            MulI(RegisterX(secondary), RegisterX(primary)), // offset in primary
                            Pop(RegisterX(secondary), RegisterXZR),
                            AddI(RegisterX(secondary), RegisterX(primary)),
                            Move(ImmNum(0), RegisterX(secondary)),
                            Comment("Load")
                        ))

                        instructionList.addOne(
                            formatter.getSize(currentType) match {
                                // loading not correct
                                case 1 => LoadByte(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary), true)
                                case 4 => LoadWord(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary)) // ldrsw x7, [x7, x17, lsl #2]
                                case 8 => Load(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary)) // ldr x7, [x7, x17, lsl #3]
                            }
                        )
                        instructionList.addOne(Push(RegisterX(primary), RegisterXZR))
                    }
                    
                instructionList.addOne(Pop(RegisterX(primary), RegisterXZR))
            }
        }
        instructionList.toList
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
                    Load(BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary)), RegisterX(primary)))
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
        instructionList ++= (List(Label("main"), Push(RegisterFP, RegisterLR)))
        instructionList ++= translate(program.s)
        instructionList ++= (List(
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
        instructionList ++= (
            List(
                Label(formatter.regConf.funcLabel + func.id),
                Push(RegisterFP, RegisterLR),
                Move(RegisterSP, RegisterFP),
                Move(ImmNum(varAlloc), RegisterX(primary)),
                AddI(RegisterX(primary), RegisterSP) // varAlloc is negative
            )
        )
        instructionList ++= (calleeSave())
        instructionList ++= (translate(func.s))
        instructionList.toList
    } 

    def translate(stmt: Stmt): List[Instruction] = {
        val scratchRegs = formatter.regConf.scratchRegs
        val primary = scratchRegs(0)
        val secondary = scratchRegs(1)
        val tertiary = scratchRegs(2)
        val instructionList = ListBuffer.empty[Instruction]
        stmt match {
            case Skip() => instructionList ++= Nil
            case Decl(tp, id, rv) => {
                var charCheck = List[Instruction]()
                if (tp.isInstanceOf[CharT]) {
                    formatter.includeFx(new errorBadCharFx(formatter))
                    charCheck = List(isChar(RegisterX(primary)))
                }
                instructionList ++= translate(rv, scratchRegs)
                instructionList ++= charCheck
                instructionList ++= storeContentsToIdentifier(id, scratchRegs)
            }

            case Asgn(LIdent(id), rv) => {
                instructionList ++= translate(rv, scratchRegs)
                instructionList ++= storeContentsToIdentifier(id, scratchRegs)
            }

            // LArrElem and Pairs
            case Asgn(lv, rv) => {
                instructionList.addAll(translate(rv, scratchRegs))
                instructionList.addOne(Push(RegisterX(primary), RegisterXZR))
                instructionList.addAll(translate(lv, scratchRegs))
                instructionList.addAll(List(
                    Pop(RegisterX(secondary), RegisterXZR),
                    Move(ImmNum(0), RegisterX(tertiary))
                ))

                // primary is the pointer to the array *element* pointer
                // secondary is the value to store
                // tertiary has zero offset
                formatter.getSize(rv.tp) match {
                    case 1 => instructionList.addOne(StoreByte(RegisterW(secondary), BaseOfsRA(RegisterX(primary), RegisterX(tertiary))))
                    case 4 => instructionList.addOne(Store(RegisterW(secondary), BaseOfsRA(RegisterX(primary), RegisterX(tertiary)))) // str w8, [x7, x17, lsl #2]
                    case 8 => instructionList.addOne(Store(RegisterX(secondary), BaseOfsRA(RegisterX(primary), RegisterX(tertiary)))) // str x8, [x7, x17, lsl #3]
                }
            }

            case Read(lv) => lv match {
                case LIdent(id) => {
                    // Find the variable in the symbol table and get its position.
                    val v = sem.curSymTable.findVarGlobal(id).get
                    
                    // Function to generate both cases to load and store instructions.
                    def generateInstructions(pos: Position): List[Instruction] = pos match {
                        /* If the variable is in a register, generate a move instruction to move 
                           its value to the primary scratch register. */
                        case InRegister(r) => List(Move(RegisterX(primary), RegisterX(r)))
                        /* If the variable is on the temporary stack, generate instructions 
                           to move its offset to the secondary scratch register and load 
                           its value to the primary scratch register. */
                        case OnTempStack(r) => List(
                            Move(ImmNum(r), RegisterX(secondary)),
                            Load(BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary)), RegisterX(primary))
                        )
                        /* If the variable is on the stack, generate instructions to move its 
                           offset to the secondary scratch register and load its value to the primary 
                           scratch register. */
                        case OnStack(offset) => List(
                            Move(ImmNum(offset), RegisterX(secondary)),
                            Load(BaseOfsRA(RegisterX(formatter.regConf.framePReg), RegisterX(secondary)), RegisterX(primary))
                        )
                        case Undefined => throw new RuntimeException("Invalid position for read.")
                    }

                    instructionList ++= generateInstructions(v.pos) // Include the readIntFx internal function in the formatter.

                    // Preserves original value in cases of empty inputs for scanf.
                    instructionList ++= List(Move(RegisterX(primary), RegisterX(0)))

                    // Check the type of the variable and include the appropriate read function.
                    val readFx = sem.getType(lv) match {
                        case S_INT => new readIntFx(formatter)
                        case S_CHAR => new readCharFx(formatter)
                        case _ => throw new RuntimeException("Invalid type for read.")
                    }

                    formatter.includeFx(readFx)

                    /* Generate the load instructions and call the readIntFx function. 
                       The result will be stored in the primary scratch register. */
                    instructionList ++= callFx(readFx.label, formatter.regConf.scratchRegs, Left(List()), List())
                    instructionList ++= generateInstructions(v.pos) // Store the result back into the variable.
                }

                case pe: PairElem => {

                    // Obtain the address of the pair.
                    val lv_ = pe match {
                        case First(p) => p
                        case Second(p) => p
                    }
                    
                    instructionList ++= (translate(lv_, scratchRegs)) // Will load the pair address into primary.

                    // Check if the pair is null before attempting to read from it.
                    instructionList ++= List(
                        Compare(RegisterX(primary), RegisterXZR),
                        BranchCond(formatter.includeFx(new errorNullFx(formatter)), EqI)
                    )

                    // Determine offset for the specific pair element.
                    pe match {
                        case First(pos) => instructionList += (Move(ImmNum(0), RegisterX(secondary)))
                        case Second(pos) => instructionList += (Move(ImmNum(formatter.getSize(S_ANY)), RegisterX(secondary)))
                    }

                    // Obtain the value stored in the pair element
                    instructionList ++= (List(
                        Load(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary))
                    ))
                }

                case _ => throw new RuntimeException("Invalid case for read.")
            }

            case Free(x) => {
                // Include the errorNullFx internal function in the formatter.
                formatter.includeFx(new errorNullFx(formatter))
                instructionList ++= translate(x, scratchRegs)
                instructionList ++= List(
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

            case Return(x) => {
                instructionList ++= translate(x, formatter.regConf.scratchRegs)
                instructionList ++= List(
                    Move(RegisterX(formatter.regConf.scratchRegs(0)),
                    RegisterX(formatter.regConf.resultRegister))
                )
                instructionList ++= calleeRestore()
                instructionList ++= List(
                    // DEALLOC ALL VARIABLES!!!
                    Move(ImmNum(sem.curSymTable.stackAllocVars), RegisterX(formatter.regConf.scratchRegs.head)),
                    SubI(RegisterX(formatter.regConf.scratchRegs.head), RegisterSP),
                    Pop(RegisterFP, RegisterLR), ReturnI
                )
            }
            
            case Exit(x) => instructionList ++= callFx("exit", formatter.regConf.scratchRegs, Right(List(x)), List(S_INT))
            case Print(x) => instructionList ++= callFx(determinePrint(x), formatter.regConf.scratchRegs, Right(List(x)), List(S_ANY))
            case Println(x) => {
                instructionList ++= callFx(determinePrint(x), formatter.regConf.scratchRegs, Right(List(x)), List(S_ANY))
                instructionList ++= callFx(formatter.includeFx(new printLineFx(formatter)), formatter.regConf.scratchRegs, Left(List()), List())
            }
            case Cond(x, s1, s2) => { // TODO: Defintely need to change that
                val s1Label = formatter.generateGeneralLabel()
                val s2Label = formatter.generateGeneralLabel()
                instructionList ++= translate(x, scratchRegs)
                instructionList ++= (List(
                    Compare(RegisterX(primary), RegisterXZR), // TODO: Change ResiterXZR
                    BranchCond(s1Label, EqI),
                ))
                // TODO: Make sure that s1 and s2 have access to the parent symbol table but dont have access to each other's
                sem.curSymTable = sem.curSymTable.getNextChildSymbolTable()
                instructionList ++= calleeSave()
                instructionList ++= translate(s1)
                instructionList ++= calleeRestoreLocal()
                sem.curSymTable = sem.curSymTable.parent().get
                instructionList ++= (List(Branch(s2Label), Label(s1Label)))
                sem.curSymTable = sem.curSymTable.getNextChildSymbolTable()
                instructionList ++= calleeSave()
                instructionList ++= translate(s2)
                instructionList ++= calleeRestoreLocal()
                sem.curSymTable = sem.curSymTable.parent().get
                instructionList += Label(s2Label)
            }
            case Loop(x, s) => {
                val loopLabel = formatter.generateGeneralLabel()
                val condLabel = formatter.generateGeneralLabel()
                instructionList ++= (List(
                    Branch(condLabel),
                    Label(loopLabel)
                ))
                sem.curSymTable = sem.curSymTable.getNextChildSymbolTable()
                instructionList ++= calleeSave()
                instructionList ++= translate(s)
                instructionList ++= calleeRestoreLocal()
                sem.curSymTable = sem.curSymTable.parent().get
                instructionList += Label(condLabel)
                instructionList ++= translate(x, scratchRegs)
                instructionList ++= (List(
                    Compare(RegisterX(primary), RegisterXZR), // TODO: Change ResiterXZR
                    BranchCond(loopLabel, NeI),
                ))
            }
            case Body(s) => {
                sem.curSymTable = sem.curSymTable.getNextChildSymbolTable()
                instructionList ++= calleeSave()
                instructionList ++= translate(s)
                instructionList ++= calleeRestoreLocal()
                sem.curSymTable = sem.curSymTable.parent().get
            }

            case Delimit(s1, s2) => instructionList ++= translate(s1) ++ translate(s2)
        }
        instructionList.toList
    }

    def translate(lv: LValue, regs: List[Int]): List[Instruction] = {
        val primary = regs(0)
        val secondary = regs(1)
        val tertiary = regs(3)
        val instructionList = ListBuffer.empty[Instruction]
        lv match {
            case LArrElem(id, xs) => {
                // regs(0) stores the index -> actual offset
                // regs(1) stores the address of the array
                // regs(2) stores the size of the element

                instructionList.addAll(loadContentsFromIdentifier(id, regs))

                instructionList.addOne(Push(RegisterX(primary), RegisterXZR))

                var currentType = sem.curSymTable.findVarGlobal(id).get.tp.asInstanceOf[S_ARRAY].tp
                if (xs.length > 1) {
                    for (i <- 0 to xs.length - 2) {
                        var elemSize = formatter.getSize(currentType)
                        instructionList.addAll(translate(xs(i), regs)) // Index in primary
                        instructionList.addAll(List(
                            Move(ImmNum(elemSize), RegisterX(secondary)),
                            MulI(RegisterX(secondary), RegisterX(primary)), // offset in primary
                            Pop(RegisterX(secondary), RegisterXZR),
                            AddI(RegisterX(secondary), RegisterX(primary)),
                            Move(ImmNum(0), RegisterX(secondary)),
                            Load(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary))
                            // Address of the new array now in primary 
                            // Using Load only as we are guaranteed 8 bytes!
                        ))
                        currentType = currentType.asInstanceOf[S_ARRAY].tp
                    }
                }

                var elemSize = formatter.getSize(currentType)
                instructionList.addAll(translate(xs(xs.length - 1), regs)) // Index in primary
                // TODO: Add check!
                instructionList.addAll(List(
                    Move(ImmNum(elemSize), RegisterX(secondary)),
                    MulI(RegisterX(secondary), RegisterX(primary)), // offset in primary
                    Pop(RegisterX(secondary), RegisterXZR),
                    AddI(RegisterX(secondary), RegisterX(primary))
                ))
            }
            
            case LIdent(id) => instructionList ++= loadContentsFromIdentifier(id, regs)
            // NOTE: We get here ONLY when we need to find the address of the id
            // I.e. when the id is a pair or an array.
            // So, we just return their contents
            
            case pe: PairElem => {
                val lv_ = pe match {
                    case First(p) => p
                    case Second(p) => p
                }

                instructionList ++= (translate(lv_, regs)) 
                // determining the reference r_p which is the pointer of p

                instructionList ++= (List(
                    Compare(RegisterX(primary), RegisterXZR),
                    BranchCond(formatter.includeFx(new errorNullFx(formatter)), EqI)
                )) // Check

                if (lv_.isInstanceOf[PairElem] || lv_.isInstanceOf[LArrElem]) {
                    instructionList ++= (List(
                        Move(ImmNum(0), RegisterX(secondary)),
                        Load(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary))
                    ))
                } // following r to abtain a pointer to the pair p
                
                pe match {
                    case First(pos)  => instructionList += (Move(ImmNum(0), RegisterX(secondary)))
                    case Second(pos) => instructionList += (Move(ImmNum(formatter.getSize(S_ANY)), RegisterX(secondary)))
                }

                instructionList ++= (List(
                    AddI(RegisterX(secondary), RegisterX(primary)),
                )) // obtaining the reference to the *element* of the pair
            }
        }
        instructionList.toList
    }

    def translate(rv: RValue, regs: List[Int]): List[Instruction] = {
        val primary = regs(0)
        val secondary = regs(1)
        val instructionList = ListBuffer.empty[Instruction]
        rv match {
            case ArrL(xs) => {
                val arrLen = xs.length
                val elemSize = if (arrLen > 0) formatter.getSize(rv.tp.asInstanceOf[S_ARRAY].tp) else 0
                val arrSize = arrLen * elemSize
                var arrHead = 0
                instructionList ++= List(Comment(s"$arrLen element array"),
                    Move(ImmNum(arrSize + formatter.getSize(S_INT)), RegisterX(primary))                
                )
                instructionList ++= callFx(formatter.includeFx(new mallocFx(formatter)), formatter.regConf.scratchRegs, Left(List(RegisterX(primary))), List(S_ANY))
                instructionList ++= List(Move(RegisterX(formatter.regConf.resultRegister), RegisterX(formatter.regConf.pointerReg)),
                    Move(ImmNum(formatter.getSize(S_INT)), RegisterX(primary)),
                    AddI(RegisterX(primary), RegisterX(formatter.regConf.pointerReg)),
                    Move(ImmNum(xs.length), RegisterX(primary)),
                    Move(ImmNum(-(formatter.getSize(S_INT))), RegisterX(secondary)),
                    Store(RegisterW(primary), BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary)))
                )
                instructionList ++= (for (x <- xs) yield {
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
                }).flatten
                instructionList += (Move(RegisterX(formatter.regConf.pointerReg), RegisterX(primary)))
            }
            case Call(id, xs) => instructionList ++= callFx(formatter.regConf.funcLabel ++ id, formatter.regConf.scratchRegs, Right(xs), 
                sem.curSymTable.findFunGlobal(id).get.st.parDict.values.toList.map((v)=> v.tp))
            case RExpr(e) => instructionList ++= translate(e, regs)
            
            case NewPair(e1, e2) => {
                val pOfs1 = 0
                val pOfs2 = formatter.getSize(S_ANY)
                    instructionList += Move(ImmNum(2 * formatter.getSize(S_ANY)), RegisterX(primary))
                    instructionList ++= callFx(formatter.includeFx(new mallocFx(formatter)), formatter.regConf.scratchRegs, Left(List(RegisterX(primary))), List(S_ANY))
                    instructionList += (Move(RegisterX(primary), RegisterX(formatter.regConf.pointerReg)))
                    instructionList ++= translate(e1, regs)
                    instructionList ++= List(
                        Move(ImmNum(pOfs1), RegisterX(secondary)),
                        Store(RegisterX(primary), BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary)))
                        )
                    instructionList ++= translate(e2, regs)
                    instructionList ++= List(
                        Move(ImmNum(pOfs2), RegisterX(secondary)),
                        Store(RegisterX(primary), BaseOfsRA(RegisterX(formatter.regConf.pointerReg), RegisterX(secondary))),
                        Move(RegisterX(formatter.regConf.pointerReg), RegisterX(primary))
                    )
                    // Potential issue: X16 overwritten when dealing with array?
            }
            
            case pe: PairElem => {
                // This loads the contents of the pair from either location
                // For the address of the pair for each location, you use the lvalue instance of PairElem.
                
                var lv = pe match {
                    case First(lv) => lv
                    case Second(lv) => lv
                }
                
                instructionList ++= translate(lv, regs) // obtain reference r
                instructionList ++= (List(
                    Compare(RegisterX(primary), RegisterXZR),
                    BranchCond(formatter.includeFx(new errorNullFx(formatter)), EqI),
                )) 
                
                if (lv.isInstanceOf[PairElem] || lv.isInstanceOf[LArrElem]) {
                    instructionList ++= (List(
                        Move(ImmNum(0), RegisterX(secondary)),
                        Load(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary))
                    ))
                } // following r to abtain a pointer to the pair p

                
                pe match {
                    case First(lv) => {
                        instructionList += (Move(ImmNum(0), RegisterX(secondary)))
                    }
                    case Second(lv) => {
                        instructionList += (Move(ImmNum(formatter.getSize(S_ANY)), RegisterX(secondary)))
                    }
                }
                instructionList += (Load(BaseOfsRA(RegisterX(primary), RegisterX(secondary)), RegisterX(primary)))
            }
        }
        instructionList.toList
    }

    // Gives the correct print label for the expression
    // And adds the required dependencies
    def determinePrint(x: Expr): String = sem.getType(x) match {
        case S_STRING | S_ARRAY(S_CHAR) => formatter.includeFx(new printStringFx(formatter))
        case S_BOOL => formatter.includeFx(new printBoolFx(formatter))
        case S_CHAR => formatter.includeFx(new printCharFx(formatter))
        case S_INT =>  formatter.includeFx(new printIntFx(formatter))
        case S_PAIR(_, _) | S_ARRAY(_) | S_ERASED |  S_ANY | S_EMPTYARR => formatter.includeFx(new printPointerFx(formatter)) 
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

    def divMod(mod: Boolean, regs: List[Int]): List[Instruction] = {
        val primary = regs(0)
        val secondary = regs(1)
        var tertiary =  regs(2)
        if (!mod) tertiary = primary
        List(
            Compare(RegisterXZR, RegisterX(secondary)),
            BranchCond(formatter.includeFx(new errorDivZeroFx(formatter)), EqI),
            Move(RegisterX(primary), RegisterX(tertiary)),
            DivI(RegisterX(secondary), RegisterX(tertiary))
        )
    }
}