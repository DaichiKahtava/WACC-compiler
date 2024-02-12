package wacc


class treeWalker(var curSymTable: SymTable) {
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    var instructionList = List()

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
        case Add(x, y) => List()
        case Minus(x, y) => List()
        case GrT(x, y) => List()
        case GrEqT(x, y) => List()
        case LsT(x, y) => List()
        case LsEqT(x, y) => List()
        case Eq(x, y) => List()
        case NEq(x, y) => List()
        case And(x, y) => List()
        case Or(x, y) => List()

        // Atom expressions.
        case IntL(n) => List()
        case BoolL(b) => List()
        case CharL(c) => List()
        case StrL(s) => List()
        case PairL() => List()
        case Ident(id) => List()
        case ArrElem(id, xs) => List()

        // Defaulting case.
        case _ => throw new RuntimeException(s"Undefined expression.")
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