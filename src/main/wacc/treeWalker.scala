package wacc


class treeWalker(var curSymTable: SymTable) {
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    var instructionList = List()
    var nextTempReg = 1 // Start with R1 for temporary values?

    def generateInstructionList(e: Expr) : List[Instruction] = e match {
        // UnOp expressions.

        case Not(x) => List()
        case Neg(x) => List()
        case Len(x) => List()
        case Ord(x) => List()
        case Chr(x) => List()

        // BinOp expressions.

        case Mul(x, y) => List()
        case Div(x, y) => List()
        case Mod(x, y) => List()

        // Pattern match for Add.
        case Add(left, right) => 
            val output = generateInstructionList(left) ++ // Evaluate the left-hand side first.
            generateInstructionList(right) ++ 
            List(AddI(Register(nextTempReg), Register(0))) // ADD, put result in R0.
            val newTempReg = nextTempReg + 1
            nextTempReg = newTempReg // Update your register reference.
            return output
        
        // Similar to Add, replace AddI with SubI.
        case Minus(left, right) =>
            val output = generateInstructionList(left) ++ 
            generateInstructionList(right) ++ 
            List(SubI(Register(nextTempReg), Register(0)))
            val newTempReg = nextTempReg + 1
            nextTempReg = newTempReg
            return output

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
        case IntL(n) => List(Load(ImmNum(n), Register(0)))
        
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

    def generateInstructionList(list: List[Any]) : List[Instruction] = ???

    def generateInstructionList(program: Program) : List[Instruction] = {
        program.funcs.foreach((f) => {
            curSymTable = curSymTable.findFunGlobal(f.id).get.st // We are in the local symbolTable
            generateInstructionList(f.s)
            curSymTable = curSymTable.parent().get // We are in the parent/global symbolTable
        })
        instructionList ::: generateInstructionList(program.s) // [tm1722] either ::: or ++ can be used
        return instructionList
    }

    def generateInstructionList(stmt: Stmt) : List[Instruction] = ???
    def generateInstructionList(lv: LValue) : List[Instruction] = ???
    def generateInstructionList(rv: RValue) : List[Instruction] = ???
    def generateInstructionList(pe: PairElem) : List[Instruction] = ???
    
}