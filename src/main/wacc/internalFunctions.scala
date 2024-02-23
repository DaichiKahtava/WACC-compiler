package wacc

sealed trait internalFunctions {
    val instructions: List[Instruction]
    val label: String
}

object errorDivZeroFx extends internalFunctions {
    val instructions = List(
        Comment("Division by zero error handler as seen in the ref. compiler"),
        Data("fatal error: division or modulo by zero\n", ".L._errDivZero_str0"),
        AlignInstr(),
        Label("_errDivZero"),
        Address(".L._errDivZero_str0", RegisterX(0)),
        BranchLink("_prints"),
        Move(ImmNum(-1), RegisterW(0)),
        BranchLink("exit")
    )
    val label: String = "_errDivZero"
}

