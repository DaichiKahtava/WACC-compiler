package wacc

class treeWalker(){
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    // var assemblyCode = ???

    def generateAssemblyCode(e: Expr) : Instruction = ???

    def generateAssemblyCode(list: List[Any]) : Instruction = ???

    def generateAssemblyCode(program: Program) : Instruction = ???

    def generateAssemblyCode(stmt: Stmt) : Instruction = ???
    def generateAssemblyCode(lv: LValue) : Instruction = ???
    def generateAssemblyCode(rv: RValue) : Instruction = ???
    def generateAssemblyCode(pe: PairElem) : Instruction = ???
    
}