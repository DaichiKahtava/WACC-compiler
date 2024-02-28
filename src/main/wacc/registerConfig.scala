package wacc

/* 
    Conventions for register use:
    - All variables should be stored in the calleSaves registers and stack
    - The assignment of the variables to their location is done before hand on the symbol table
    - scratchRegs are to be used for immediate operations and are not expected to persist (volatile!)
    - after each simple operation the result is stored on the simple element of scratchRegs
    - Array construction is done with pointerReg
    - argRegs are only used for passing arguments, resultRegister stores the result from a function
    - offsetReg *might* be used to store offsets when using pointers
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