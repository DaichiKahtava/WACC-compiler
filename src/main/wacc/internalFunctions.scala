package wacc

sealed trait InternalFunction {
    val label: String
    val instructions: List[Instruction]
    val dependencies: List[InternalFunction]
}

object errorDivZeroFx extends InternalFunction {
    val label: String = "_errDivZero"
    val instructions = List(
        Comment("Division by zero error handler as seen in the ref. compiler"),
        Data("fatal error: division or modulo by zero\n", ".L._errDivZero_str0"),
        AlignInstr(),
        Label(label),
        Address(".L._errDivZero_str0", RegisterX(0)),
        BranchLink("_prints"),
        Move(ImmNum(-1), RegisterW(0)),
        BranchLink("exit")
    )
    val dependencies: List[InternalFunction] = List.empty
}

object printStringFx extends InternalFunction {
    val label: String = "_prints"
    val instructions: List[Instruction] = List (
        Comment("Print string as seen in the ref. compiler"),
        Comment("The pointer of the string to be printed is expected at X0"),
        Data("%.*s", ".L._prints_str0"),
        AlignInstr(),
        Label(label), // TODO: Possibly abstract common patterns (e.g. label after align and push/pop)?
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
    val dependencies: List[InternalFunction] = List.empty
}

object printBoolFx extends InternalFunction {
    val label: String = "_printb"
    val instructions: List[Instruction] = List (
        Comment("Print bool as seen in the ref. compiler"),
        Data("false", ".L._printb_str0"),
        Data("true", ".L._printb_str1"),
        Data("%.*s", ".L._printb_str2"),
        AlignInstr(),
        Label(label), // TODO: Possibly abstract common patterns (e.g. label after align and push/pop)?
        Push(RegisterLR, RegisterXZR, PreIndxA(RegisterSP, -16)),

        Compare(RegisterW(0), ImmNum(0)),
        BranchCond(".L_printb0", NeI),
        Address(".L._printb_str0", RegisterX(1)),
        Branch(".L_printb1"),

        Label(".L_printb0"),
        Address(".L._printb_str1", RegisterX(1)),

        Label(".L_printb1"),
        Address(".L._printb_str2", RegisterX(0)),
        BranchLink(printStringFx.label),
        ReturnI
    ) 

    val dependencies: List[InternalFunction] = List(printStringFx)
}