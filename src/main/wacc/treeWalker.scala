package wacc

import scala.collection.mutable.ListBuffer

class TreeWalker(var sem: Semantics, formatter: Aarch64_formatter) {
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    var instructionList = List[Instruction]()
    var callerRegs = (0 to 7).toList ++ (9 to 15).toList
    var calleeRegs = (19 to 28).toList
    var gpRegs = callerRegs ++ calleeRegs
    val funcLabel = "wacc_user_"


    var availRegs = ListBuffer.empty[Int]
    availRegs.addAll(gpRegs)
    
    val outputRegister = RegisterX(0)
    val dst = 0 // TODO: Might be a seperate destination for system call?
    val nxt = 1

    var labelNum = 0


    // Conventions for translate: 
    //  regs contains a list of all available general purpose registers
    //  and the first register is used as the `return` register for the
    //  specific instruction

    def translate(e: Expr, regs: List[Int]): List[Instruction] = e match {

        // UnOp expressions.
        case Not(x) => translate(x, regs) ++ 
            List(Compare(RegisterX(regs(dst)), ImmNum(1)), 
            SetCond(RegisterX(regs(dst)), NeI))

        case Neg(x) => translate(x, regs.tail) ++
            List(Move(ImmNum(0), RegisterX(regs.head)), 
            SubI(RegisterX(regs(nxt)), RegisterX(regs.head)))

        case Len(x) => translate(x, regs) 
        case Ord(x) => translate(x, regs) 
        case Chr(x) => translate(x, regs) 

        // BinOp expressions.
        case Mod(x, y) =>
            translate(x, regs) ++ translate(y, regs.tail) ++ 
            List(Move(RegisterX(regs(dst)), RegisterX(regs(nxt + 1))), // Store x into another register.
                DivI(RegisterX(regs(nxt)), RegisterX(regs(nxt + 1))), // Divide x by y.
                MulI(RegisterX(regs(nxt)), RegisterX(regs(nxt + 1))), // Multiply (y * quotient).
                SubI(RegisterX(regs(nxt + 1)), RegisterX(regs(dst))))  // Calculate remainder.

        case Add(x, y) => 
            translate(x, regs) ++ translate(y, regs.tail) ++ List(AddI(RegisterX(regs(nxt)), RegisterX(regs(dst))))
        
        case Minus(x, y) => 
            translate(x, regs) ++ translate(y, regs.tail) ++ List(SubI(RegisterX(regs(nxt)), RegisterX(regs(dst))))

        case Mul(x, y) =>
            translate(x, regs) ++ translate(y, regs.tail) ++ List(MulI(RegisterX(regs(nxt)), RegisterX(regs(dst))))

        case Div(x, y) => {
            formatter.includeFx(errorDivZeroFx)
            return translate(x, regs) ++ translate(y, regs.tail) ++ List(
                    Compare(RegisterXZR, RegisterX(regs(nxt))),
                    BranchCond(errorDivZeroFx.label, EqI),
                    DivI(RegisterX(regs(nxt)), RegisterX(regs(dst)))
                )
        }

        case GrT(x, y) => 
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterX(regs(dst)), GtI))

        case GrEqT(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterX(regs(dst)), GeI))

        case LsT(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterX(regs(dst)), LtI))

        case LsEqT(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterX(regs(dst)), LeI))

        case Eq(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterX(regs(dst)), EqI))

        case NEq(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterX(regs(dst)), NeI))
            
        case And(x, y) =>
            val curLabel = s".L${labelNum}"
            labelNum += 1
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), ImmNum(1)),
            BranchCond(curLabel, NeI),
            Compare(RegisterX(regs(nxt)), ImmNum(1)),
            Label(curLabel),
            SetCond(RegisterX(regs(dst)), EqI))

        case Or(x, y) => 
            val curLabel = s".L${labelNum}"
            labelNum += 1
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), ImmNum(1)),
            BranchCond(curLabel, EqI),
            Compare(RegisterX(regs(nxt)), ImmNum(1)),
            Label(curLabel),
            SetCond(RegisterX(regs(dst)), EqI))

        // Atom expressions.

        // Load the integer directly.
        case IntL(n) => List(Move(ImmNum(n), RegisterX(regs(dst))))

        // Load the boolean value using integers.
        case BoolL(b) => {
            val value = b match {
                case true  => 1
                case false => 0 
            }
            List(Move(ImmNum(value), RegisterX(regs(dst))))
        }

        // Obtain the integer value of a character.
        case CharL(c) => List(Move(ImmNum(c.toInt), RegisterX(regs(dst))))
        
        case StrL(s) => {
            val label = formatter.includeString(s)
            return List(Address(label, RegisterX(regs.head)))
        }

        case PairL() => 
            List(Move(ImmNum(0), RegisterX(regs(dst)))) // Treat as null pointer?

        // Pattern match for the identifier.
        case Ident(id) => sem.curSymTable.findVarGlobal(id).get.pos match {
            case InRegister(r) => List(Move(RegisterX(r), RegisterX(regs(dst))))
            case OnStack(offset) => ???
            case Undefined => ??? // Should not get here
        }

        case ArrElem(id, xs) => List()
    }

    def translate(list: List[Any], regs: List[Int]): List[Instruction] = list match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined list.")
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
        instructionList.addAll(List(Label("main"), Push(RegisterFP, RegisterLR, PreIndxA(RegisterSP, -16))))
        instructionList ++= translate(program.s, gpRegs.toList)
        instructionList.addAll(List(
            Pop(PstIndxIA(RegisterSP, 16), RegisterFP, RegisterLR),
            Move(ImmNum(0), RegisterX(0)),
            ReturnI
        ))
        instructionList.toList
    }

    def translate(func: Func): List[Instruction] = {
        // Callee saves and callee restore must go here.
        // Labels must be ensured unique
        val instructionList = ListBuffer.empty[Instruction]
        instructionList.addOne(Label(funcLabel + func.id))
        instructionList.addAll(translate(func.s, gpRegs.toList))
        instructionList.addOne(ReturnI)
        instructionList.toList
    } 

    def translate(stmt: Stmt, regs: List[Int]): List[Instruction] = stmt match {
        case Skip() => Nil
        case Decl(_, id, rv) => 
            val v = sem.curSymTable.findVarGlobal(id).get
            v.pos match {
            case InRegister(r) => translate(rv, r :: regs)
            case OnStack(offset) => ???
            case Undefined =>
                sem.curSymTable.redefineSymbol(id, VARIABLE(v.tp, InRegister(regs.head)))
                translate(rv, regs)
            }

        case Asgn(LIdent(id), rv) => sem.curSymTable.findVarGlobal(id).get.pos match {
            case InRegister(r) => translate(rv, r :: regs)
            case OnStack(offset) => ???
            case Undefined => ??? // Should not come here
        }
        // LArrElem and Pairs
        case Asgn(lv, rv) => ???

        case Read(lv) => ???
        case Free(x) => ???
        case Return(x) => ???
        case Exit(x) => callFx("exit", regs, List(x), List(S_INT))
        case Print(x) => callFx(determinePrint(x), regs, List(x), List(S_ANY))
        case Println(x) => {
            formatter.includeFx(printLineFx)
            callFx(determinePrint(x), regs, List(x), List(S_ANY)) ++
            callFx(printLineFx.label, regs, List(), List())
        }
        case Cond(x, s1, s2) => ???
        case Loop(x, s) => ???
        case Body(s) => ???

        case Delimit(s1, s2) =>
            val res = translate(s1, regs)
            sem.curSymTable.varDict.values.map(_.pos).foreach{ // Maybe move this somewhere else to a general function when needed
                case InRegister(r) =>
                    if (availRegs.contains(r)) availRegs.remove(availRegs.indexOf(r))
                case _ => () // Do nothing otherwise
            }
            res ++ translate(s2, availRegs.toList)
        // TODO (for delimit): Weighting? and register allocation
    }

    def translate(lv: LValue, regs: List[Int]): List[Instruction] = lv match {
        case LArrElem(id, xs) => ??? 
        case LIdent(id) => ???
        case pe: PairElem => translate(pe, regs.tail)
    }

    def translate(rv: RValue, regs: List[Int]): List[Instruction] = rv match {
        case ArrL(xs) => {
            formatter.includeFx(mallocFx)
            val arrLen = xs.length
            val elemSize = if (arrLen > 0) getSize(rv.tp.asInstanceOf[S_ARRAY].tp) else 0
            val arrSize = arrLen * elemSize
            var elems = ListBuffer.empty[Instruction]
            var arrHead = -4
            for (x <- xs) {
                elems.addAll(translate(x, 8::regs))
                elems.addOne(Store(RegisterW(8), BaseOfsIA(RegisterX(16), arrHead + elemSize)))
                arrHead += elemSize
            }
            // Will clean this up later but functionality is fine
            List(
                Comment(s"$arrLen element array"),
                Move(ImmNum(arrSize + 4), RegisterW(0)),
                BranchLink("_malloc"),
                Move(RegisterX(0), RegisterX(16)),
                AddI(ImmNum(4), RegisterX(16)),
                Move(ImmNum(xs.length), RegisterXR),
                Store(RegisterW(8), BaseOfsIA(RegisterX(16), -4))
            ) ++ elems.toList ++ List(Move(RegisterX(16), RegisterX(regs.head)))
        }
        case Call(id, xs) => ???
        case RExpr(e) => translate(e, regs)
        case NewPair(e1, e2) => ???
        case pe: PairElem => translate(pe, regs.tail)
    }

    def translate(pe: PairElem, regs: List[Int]): List[Instruction] = pe match {
        case First(lv) => ??? 
        case Second(lv) => ???
    }

    // Gives the correct print label for the expression
    // And adds the required dependencies
    def determinePrint(x: Expr): String = sem.getType(x) match {
        case S_STRING => {
            formatter.includeFx(printStringFx)
            printStringFx.label
        }
        case S_BOOL => {
            formatter.includeFx(printBoolFx)
            printBoolFx.label
        }
        case S_CHAR => {
            formatter.includeFx(printCharFx)
            printCharFx.label
        }
        case S_INT => {
            formatter.includeFx(printIntFx)
            printIntFx.label
        }
        case _ => ???
    }

    // TODO: Need to change the positions of variables if necessary?
    def saveRegs(regsNotInUse: List[Int], subset: List[Int]): List[Instruction] = {
        val regsInUse = subset.diff(regsNotInUse).toList
        Comment("Saving registers") :: (for {
            List(r1, r2) <- regsInUse.grouped(2).toList // [em422]
        } yield (Push(RegisterX(r1), RegisterX(r2), PreIndxA(RegisterSP, -16)))) ++ 
        // This can probably be compacted into above but idk how
        (if (regsInUse.size % 2 != 0) List(Push(RegisterX(regsInUse.last), RegisterXZR, PreIndxA(RegisterSP, -16))) else Nil) ++
        List(Comment("Saving registers END"))
    }

    def restoreRegs(regsNotInUse: List[Int], subset: List[Int]): List[Instruction] = {
        val regsInUse = subset.diff(regsNotInUse).toList
        Comment("Restoring registers") :: 
        (if (regsInUse.size % 2 != 0) List(Pop(PstIndxIA(RegisterSP, 16), RegisterX(regsInUse.last), RegisterXZR)) else Nil) ++ 
        (for {
            List(r1, r2) <- regsInUse.grouped(2).toList.reverse
        } yield (Pop(PstIndxIA(RegisterSP, 16), RegisterX(r1), RegisterX(r2)))) ++ List(Comment("Restoring registers END"))
        // This can probably be compacted into above but idk how
    }

    def callFx(label: String, regs: List[Int], args: List[Expr], parTypes:List[S_TYPE]): List[Instruction] = {
        // It is expected that will all arguments will be translated and stored in the register
        // or on the stack with an offset relative to the frame pointer. 
        val instrs = ListBuffer.empty[Instruction]

        instrs.addAll(callerSave(regs))
        // This will save everything
        // So now everything should be in the stack

        // To avoid accidentally overwriting registers, we put everything in stack and
        // Then load everything from the stack        
        for (i <- 0 to Math.min(7, args.length - 1)) {
            // TODO: Something like RegisterXR ++ availRegs might be more desirable below
            // <Magin number!> see below!
            instrs.addAll(translate(args(i), 8::gpRegs.toList)) 
            instrs.addOne(Move(RegisterXR, RegisterX(i)))
        }

        if (args.length >= 8) {
            val extraElems = args.length - 8
            
            var totalSize = 0
            // We know that args.length == parTypes.length from semantic checks
            for (i <- 8 to (args.length - 1)) {
                totalSize += getSize(parTypes(i))
            }

            instrs.addAll(List(
                Comment("Allocating stack for more than 8 arguments"),
                SubI(ImmNum(((totalSize / 16) + 1) * 16), RegisterSP)
                // May waste a memory location of the stack but can be fixed
                // during optimisation stage
            ))

            var ofs = 0
            for (i <- 8 to (args.length - 1)) {
                instrs.addAll(translate(args(i), 8::gpRegs.toList)) 
                var size = getSize(parTypes(i)) // Recalculation
                size match {
                    case 1 => instrs.addOne(StoreByte(RegisterXR, BaseOfsIA(RegisterSP, ofs)))
                    case 4 => instrs.addOne(StoreWord(RegisterXR, BaseOfsIA(RegisterSP, ofs)))
                    case 8 => instrs.addOne(Store(RegisterXR, BaseOfsIA(RegisterSP, ofs)))
                }
               ofs += size
            }
        }

        instrs.addOne(BranchLink(label))
        // The result will be saved on the x8.
        instrs.addOne(Move(RegisterX(0), RegisterXR))
        instrs.addAll(callerRestore(regs))

        instrs.toList
    }


    // calle functions save/restore registers, the frame pointer as well as the link register

    // KEY OPTIMISATION POINT: Callee saves now saves everything! There is a possibility to analyse the bedy of the function
    //                         to save only what we need!
    def callerSave(regsNotInUse: List[Int]): List[Instruction] = saveRegs(regsNotInUse, callerRegs)
    def calleeSave(regsNotInUse: List[Int]): List[Instruction] = {
        Push(RegisterFP, RegisterLR, PreIndxA(RegisterSP, -16)) ::
        saveRegs(List(), calleeRegs)
    }
    def callerRestore(regsNotInUse: List[Int]): List[Instruction] = restoreRegs(regsNotInUse, callerRegs)
    def calleeRestore(regsNotInUse: List[Int]): List[Instruction] = {
        restoreRegs(List(), calleeRegs) ++
        List(Pop(PstIndxIA(RegisterSP, 16), RegisterFP, RegisterLR))
    }

    def getSize(t: S_TYPE) = t match {
        // Returns number of bytes
        // <Addresses have 8 bytes>
        case S_INT => 4
        case S_BOOL => 1
        case S_STRING => 8
        case S_CHAR => 1
        case S_ARRAY(tp) => 8
        case S_PAIR(tp1, tp2) => 8
        case S_ERASED => 8
        case S_ANY => 8
        case S_EMPTYARR => 8
    }
}