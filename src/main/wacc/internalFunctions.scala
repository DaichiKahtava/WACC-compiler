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

object printStringFx extends internalFunctions {
    val instructions: List[Instruction] = List (
        Comment("Print string as seen in the ref. compiler"),
        Data("%.*s", ".L._prints_str0"),
        AlignInstr(),
        Label("_prints"), // TODO: Possibly abstract common patterns (e.g. label after align)?
        Push(RegisterLR, RegisterXZR, PreIndxA(RegisterSP, -16)),
        Move(RegisterX(0), RegisterX(2)),
        LoadWord(BaseOfsIA(RegisterX(0), -4), RegisterX(1)),
        Address(".L._prints_str0", RegisterX(0)),
        BranchLink("printf"),
        Move(ImmNum(0), RegisterX(0)),
        BranchLink("fflush"),
        Pop(PstIndxIA(RegisterSP, 16), RegisterLR, RegisterXZR),
        ReturnI
    ) 
    val label: String = "_prints"
}

