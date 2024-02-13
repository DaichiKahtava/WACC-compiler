package wacc


class TreeWalker(var curSymTable: SymTable) {
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    var instructionList = List()
    var nextTempReg = 1 // Start with R1 for temporary values?
    val outputRegister = Register(0)

    def generateInstructionList(e: Expr) : List[Instruction] = e match {
        // UnOp expressions.

        case Not(x) => List()
        case Neg(x) => List()
        case Len(x) => List()
        case Ord(x) => List()
        case Chr(x) => List()

        // BinOp expressions.

        case Mod(x, y) => List()

        // Pattern match for Add.
        case Add(x, y) => 
            val output = generateInstructionList(x) ++ // Evaluate the left-hand side first.
            generateInstructionList(y) ++ 
            List(AddI(Register(nextTempReg), Register(nextTempReg - 1))) // ADD, put result in R0.
            val newTempReg = nextTempReg + 1
            nextTempReg = newTempReg // Update register reference.
            output
        
        // Similar to Add, replace AddI with SubI.
        case Minus(x, y) =>
            val output = generateInstructionList(x) ++ 
            generateInstructionList(y) ++ 
            List(SubI(Register(nextTempReg), Register(nextTempReg - 1)))
            val newTempReg = nextTempReg + 1
            nextTempReg = newTempReg
            output

        // [tm1722] Similarly as for Add and Sub, do the same fo Mul and Div.
        case Mul(x, y) => 
            val output = generateInstructionList(x) ++ // Evaluate the left-hand side first.
            generateInstructionList(y) ++ 
            List(MulI(Register(nextTempReg), Register(nextTempReg - 1)))
            val newTempReg = nextTempReg + 1
            nextTempReg = newTempReg // Update register reference.
            output

        case Div(x, y) => 
            val output = generateInstructionList(x) ++ // Evaluate the left-hand side first.
            generateInstructionList(y) ++ 
            List(DivI(Register(nextTempReg), Register(nextTempReg - 1)))
            val newTempReg = nextTempReg + 1
            nextTempReg = newTempReg // Update register reference.
            output

        case GrT(x, y) => List()
        case GrEqT(x, y) => List()
        case LsT(x, y) => List()
        case LsEqT(x, y) => List()
        case Eq(x, y) => List()
        case NEq(x, y) => List()
        case And(x, y) => List()
        case Or(x, y) => List()

        // Atom expressions.

        // Load the integer directly.
        case IntL(n) => List(Load(ImmNum(n), outputRegister))

        case BoolL(b) => List()
        case CharL(c) => List()
        case StrL(s) => List()
        case PairL() => List()

        // Pattern match for the identifier.
        case Ident(id) =>
            curSymTable.findVarGlobal(id) match {
                // Variable was found in the symbol table.
                case Some(varInfo) => List(Load(varInfo.asInstanceOf[Operand], Register(0)))
                // Variable was not found in the symbol table.
                case None => throw new RuntimeException(s"Undefined variable: $id")
        }

        case ArrElem(id, xs) => List()

        // Defaulting case.
        case _ => throw new RuntimeException("Undefined expression.")
    }

    def generateInstructionList(list: List[Any]) : List[Instruction] = list match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined list.")
    }

    def generateInstructionList(program: Program) : List[Instruction] = {
        program.funcs.foreach((f) => {
            curSymTable = curSymTable.findFunGlobal(f.id).get.st // We are in the local symbolTable.
            generateInstructionList(f.s)
            curSymTable = curSymTable.parent().get // We are in the parent/global symbolTable.
        })
        instructionList ::: generateInstructionList(program.s) // [tm1722] either ::: or ++ can be used.
        return instructionList
    }

    def generateInstructionList(stmt: Stmt) : List[Instruction] = stmt match {

        // Defaulting case.
        case _ => throw new RuntimeException("Undefined statement.")
    }

    def generateInstructionList(lv: LValue) : List[Instruction] = lv match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined left value.")
    }

    def generateInstructionList(rv: RValue) : List[Instruction] = rv match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined right value.")
    }

    def generateInstructionList(pe: PairElem) : List[Instruction] = pe match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined pair.")
    }
}