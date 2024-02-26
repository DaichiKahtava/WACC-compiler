package wacc

import scala.collection.mutable.ListBuffer

class TreeWalker(var sem: Semantics) {
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    var instructionList = List[Instruction]()
    var gpRegs = ListBuffer.empty[Int]
    for (n <- 0 to 28) if (n != 8 || n < 15 || n > 19) gpRegs.addOne(n)

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
        case Not(x) => translate(x, regs.tail) ++ List(Compare(RegisterX(regs(regs.head)), ImmNum(1)), SetCond(RegisterXR, NeI))

        case Neg(x) => translate(x, regs.tail) ++
            List(Move(ImmNum(0), RegisterX(regs.head)), 
            SubI(RegisterX(regs(nxt)), RegisterX(regs.head)))

        case Len(x) => translate(x, regs) // TODO
        case Ord(x) => translate(x, regs) // TODO
        case Chr(x) => translate(x, regs) // TODO

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
            aarch64_formatter.includeFx(errorDivZeroFx)
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
            val label = aarch64_formatter.includeString(s)
            return List(Address(label, RegisterX(availRegs(dst))))
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
        var instructionList = List.empty[Instruction]
        program.funcs.foreach((f) => {
            // TODO: More idiomatic way of accessing the symbol table?
            sem.curSymTable = sem.curSymTable.findFunGlobal(f.id).get.st // We are in the local symbolTable.
            instructionList ++= translate(f)
            sem.curSymTable = sem.curSymTable.parent().get // We are in the parent/global symbolTable.
        })
        // TODO: Use blocks of sorts...
        instructionList ++= List(Label("main"), Push(RegisterFP, RegisterLR, PreIndxA(RegisterSP, -16)))
        instructionList ++= translate(program.s, gpRegs.toList)
        return instructionList ++ List(Move(ImmNum(0), outputRegister), Pop(PstIndxIA(RegisterSP, 16), RegisterFP, RegisterLR), ReturnI)
        // return instructionList // A bit redundant here? Can just return the generated List
    }

    def translate(func: Func): List[Instruction] = 
        // Callee saves and callee restore must go here.
        // Labels must be ensured unique
        Label(func.id) :: translate(func.s, gpRegs.toList) ++ List(ReturnI)

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
        case Exit(x) => 
            translate(x, regs) ++
            List(Move(RegisterX(regs(dst)), RegisterXR),
            Move(RegisterX(availRegs(dst)), RegisterXR), // TODO: designate argument registers before calling functions
            // Caller saves must go here
            BranchLink("exit"),
            // Caller resotre must go here
            Move(ImmNum(0), RegisterX(availRegs(dst)))) 
        case Print(x) => {
            translate(x, regs) ++
            List(Move(RegisterX(regs(dst)), RegisterXR),
            Move(RegisterX(availRegs(dst)), RegisterXR), // TODO:<Same as upwards!>
            // Caller saves must go here
            // Caller resotre must go here
            determinePrintBr(x),
            Move(ImmNum(0), RegisterX(availRegs(dst))))
        }
        case Println(x) => {
            aarch64_formatter.includeFx(printStringFx)
            aarch64_formatter.includeFx(printLineFx)
            // Caller saves goes here??
            saveRegs(regs) ++
            List(Comment("Translating expression for println")) ++
            translate(x, regs) ++
            List(Comment("Translating println"), 
            Move(RegisterX(regs(dst)), RegisterXR),
            Move(RegisterXR, outputRegister), // [dk2722] Don't understand why it gets overridden immediately
            // Move(RegisterX(availRegs(dst)), RegisterXR), // TODO:<Same as upwards!>
            determinePrintBr(x),
            BranchLink(printLineFx.label)) ++
            // Caller restore must go here
            restoreRegs(regs) ++
            List(Move(ImmNum(0), RegisterX(availRegs(dst))))
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
        case ArrL(xs) => ???
        case Call(id, xs) => ???
        case RExpr(e) => translate(e, regs)
        case NewPair(e1, e2) => ???
        case pe: PairElem => translate(pe, regs.tail)
    }

    def translate(pe: PairElem, regs: List[Int]): List[Instruction] = pe match {
        case First(lv) => ??? 
        case Second(lv) => ???
    }

    // Gives the correct print branch for the expression
    def determinePrintBr(x: Expr): Instruction = sem.getType(x) match {
        case S_STRING => {
            aarch64_formatter.includeFx(printStringFx)
            BranchLink(printStringFx.label)
        }
        case S_BOOL => {
            aarch64_formatter.includeFx(printBoolFx)
            BranchLink(printBoolFx.label)
        }
        case S_CHAR => {
            aarch64_formatter.includeFx(printCharFx)
            BranchLink(printCharFx.label)
        }
        case S_INT => {
            aarch64_formatter.includeFx(printIntFx)
            BranchLink(printIntFx.label)
        }
        case _ => ???
    }

    def saveRegs(regsNotInUse: List[Int]): List[Instruction] = {
        val regsInUse = gpRegs.diff(regsNotInUse).toList
        Comment("Saving registers") :: (for {
            List(r1, r2) <- regsInUse.grouped(2).toList
        } yield (Push(RegisterX(r1), RegisterX(r2), PreIndxA(RegisterSP, -16)))) ++ 
        // This can probably be compacted into above but idk how
        (if (regsInUse.size % 2 != 0) List(Push(RegisterX(regsInUse.last), RegisterXZR, PreIndxA(RegisterSP, -16))) else Nil)
    }

    def restoreRegs(regsNotInUse: List[Int]): List[Instruction] = {
        val regsInUse = gpRegs.diff(regsNotInUse).toList
        Comment("Restoring registers") :: 
        (if (regsInUse.size % 2 != 0) List(Pop(PstIndxIA(RegisterSP, 16), RegisterX(regsInUse.last), RegisterXZR)) else Nil) ++ 
        (for {
            List(r1, r2) <- regsInUse.grouped(2).toList.reverse
        } yield (Pop(PstIndxIA(RegisterSP, 16), RegisterX(r1), RegisterX(r2))))
        // This can probably be compacted into above but idk how
    }
}