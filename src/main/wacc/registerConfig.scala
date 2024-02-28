package wacc

/* 
    Conventions for register use:
    - All variables should be stored in the calleSaves registers and stack
    - The assignment of the variables to their location is done before hand on the symbol table
    - scratchReg1 and scratchReg2 are used for simple operations
    - after each simple operation the result is stored on scratchReg1
    - Array construction is done with pointerReg
    - argRegs are only used for passing arguments, resultRegister stores the result from a function
*/

trait registerConfig {
    val callerRegs: List[Int]
    val argRegs: List[Int]
    val scratchRegs: List[Int]
    val calleeRegs: List[Int]
    val variabRegs: List[Int]
    val gpRegs: List[Int]
    val funcLabel: String
    val resultRegister: Int
    val pointerReg: Int
    val offsetReg: Int
}