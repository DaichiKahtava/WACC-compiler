package wacc


class treeWalker(var curSymTable: SymTable) {
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    var instructionList = List()

    def generateInstructionList(e: Expr) : List[Instruction] = ???

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