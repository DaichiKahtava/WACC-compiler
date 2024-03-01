package wacc

/* 
    Conventions for register use:
    - All variables should be stored in the calleSaves registers and stack
    - The assignment of the variables to their location is done before hand on the symbol table
    - scratchRegs are to be used for immediate operations and are not expected to persist (volatile!)
    - after each simple operation the result is stored on the simple element of scratchRegs
    - pointerReg is used for 
        (a) array construction and 
        (b) as a temporary stack pointer when performing caller saves and restore
        <These cannot happen at the same time by semantics of WACC> 
    - argRegs are only used for passing arguments, resultRegister stores the result from a function
    - offsetReg is used to store large offsets when using pointers
    - framePReg always points to the general puspose register used as the frame pointer

    Conventions for grouping:
    - argRegs    must be a subset of callerRegs
    - variabRegs must be a subset of calleeRegs
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
    val framePReg: Int
}