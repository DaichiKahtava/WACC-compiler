package wacc

import scala.collection.mutable.ListBuffer

class TreeWalker(var curSymTable: SymTable) {
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    var instructionList = List[Instruction]()
    var gpRegs = ListBuffer.empty[Int]
    for (n <- 0 to 28) if (n != 8 || n < 15 || n > 19) gpRegs.addOne(n)

    var availRegs = ListBuffer.empty[Int]
    availRegs.addAll(gpRegs)
    
    val outputRegister = RegisterX(0)
    val dst = 0 // TODO: Might be a seperate destination for system call?
    val nxt = 1


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

        case Div(x, y) =>
            translate(x, regs) ++ translate(y, regs.tail) ++ List(DivI(RegisterX(regs(nxt)), RegisterX(regs(dst))))

        case GrT(x, y) => 
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterXR, GtI))

        case GrEqT(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterXR, GeI))

        case LsT(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterXR, LtI))

        case LsEqT(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterXR, LeI))

        case Eq(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterXR, EqI))

        case NEq(x, y) =>
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), RegisterX(regs(nxt))), SetCond(RegisterXR, NeI))
            
        case And(x, y) => 
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), ImmNum(1)))

        case Or(x, y) => 
            translate(x, regs) ++ 
            translate(y, regs.tail) ++ 
            List(Compare(RegisterX(regs(dst)), ImmNum(1)))

        // Atom expressions.

        // Load the integer directly.
        case IntL(n) => List(Load(ImmNum(n), RegisterX(regs(dst))))

        // Load the boolean value using integers.
        case BoolL(b) => List(Load(ImmNum(if (b) 1 else 0), RegisterX(regs(dst))))

        // Obtain the integer value of a character.
        case CharL(c) => List(Load(ImmNum(c.toInt), RegisterX(regs(dst))))
        
        case StrL(s) => {
            val label = aarch64_formatter.includeString(s)
            return List(Address(label, RegisterX(availRegs(dst))))
        }

        case PairL() => 
            List(Load(ImmNum(0), RegisterX(regs(dst)))) // Treat as null pointer?

        // Pattern match for the identifier.
        case Ident(id) => curSymTable.findVarGlobal(id).get.pos match {
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
            curSymTable = curSymTable.findFunGlobal(f.id).get.st // We are in the local symbolTable.
            instructionList ++= translate(f)
            curSymTable = curSymTable.parent().get // We are in the parent/global symbolTable.
        })
        // TODO: Use blocks of sorts...
        instructionList ++= List(Label("main"), Push(RegisterSP, RegisterFP, RegisterLR))
        instructionList ++= translate(program.s, gpRegs.toList)
        return instructionList ++ List(Pop(RegisterSP, RegisterFP, RegisterLR), ReturnI)
        // return instructionList // A bit redundant here? Can just return the generated List
    }

    def translate(func: Func): List[Instruction] = 
        // Callee saves and callee restore must go here.
        // Labels must be ensured unique
        Label(func.id) :: translate(func.s, gpRegs.toList) ++ List(ReturnI)

    def translate(stmt: Stmt, regs: List[Int]): List[Instruction] = stmt match {
        case Skip() => Nil
        case Decl(_, id, rv) => 
            val v = curSymTable.findVarGlobal(id).get
            v.pos match {
            case InRegister(r) => translate(rv, r :: regs)
            case OnStack(offset) => ???
            case Undefined =>
                curSymTable.redefineSymbol(id, VARIABLE(v.tp, InRegister(regs.head)))
                translate(rv, regs)
        }
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
            aarch64_formatter.includePrint()
            translate(x, regs) ++
            List(Move(RegisterX(regs(dst)), RegisterXR),
            Move(RegisterX(availRegs(dst)), RegisterXR), // TODO:<Same as upwards!>
            // Caller saves must go here
            // Caller resotre must go here
            BranchLink("_prints"),
            Move(ImmNum(0), RegisterX(availRegs(dst))))
        }
        case Println(x) => {
            aarch64_formatter.includePrint()
            translate(x, regs) ++
            List(Move(RegisterX(regs(dst)), RegisterXR),
            Move(RegisterX(availRegs(dst)), RegisterXR), // TODO:<Same as upwards!>
            // Caller saves must go here
            // Caller resotre must go here
            BranchLink("_prints"),
            Move(ImmNum(0), RegisterX(availRegs(dst))))
        }
        case Cond(x, s1, s2) => ???
        case Loop(x, s) => ???
        case Body(s) => ???
        case Delimit(s1, s2) => translate(s1, regs) ++ translate(s2, regs.tail) // Todo: Weighting?
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
}