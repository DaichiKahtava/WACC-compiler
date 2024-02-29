package wacc

sealed trait InternalFunction {
    val label: String
    val instructions: List[Instruction]
    val dependencies: List[InternalFunction]
}

// Internal functions must always be called with callFx!

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
    val dependencies: List[InternalFunction] = List(printStringFx)
}

object errorOutOfMemoryFx extends InternalFunction {
    val label: String = "_errOutOfMemory"
    val instructions: List[Instruction] = List (
        Data("fatal error: out of memory\n", ".L._errOutOfMemory_str0"),
        AlignInstr(),
        Label(label),
        Address(".L._errOutOfMemory_str0", RegisterX(0)),
        BranchLink("_prints"),
        Move(ImmNum(-1), RegisterX(0)),
        BranchLink("exit")
    )
    val dependencies: List[InternalFunction] = List(printStringFx)
}

object errorOutOfBoundsFx extends InternalFunction {
    val label: String = "_errOutOfBounds"
    val instructions: List[Instruction] = List(
        // Assumes that X1 stores the index
        Data("fatal error: array index %d out of bounds", ".L._errOutOfBounds_str0"),
        AlignInstr(),
        Label(label),
        Address(".L._errOutOfBounds_str0", RegisterX(0)),
        BranchLink("printf"),
        Move(ImmNum(0), RegisterX(0)), 
        BranchLink("fflush"),
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
        Push(RegisterLR, RegisterXZR),
        Move(RegisterX(0), RegisterX(2)),
        Move(ImmNum(-4), RegisterX(3)),
        LoadWord(BaseOfsRA(RegisterX(0), RegisterX(3)), RegisterX(1)),
        Address(".L._prints_str0", RegisterX(0)),
        BranchLink("printf"),
        Move(ImmNum(0), RegisterX(0)),
        BranchLink("fflush"),
        Pop(RegisterLR, RegisterXZR),
        ReturnI
    ) 
    val dependencies: List[InternalFunction] = List.empty
}

object printCharFx extends InternalFunction {
    val label: String = "_printc"
    val instructions: List[Instruction] = List (
        Comment("Print character as seen in the ref. compiler"),
        Comment("The pointer of the string to be printed is expected at X0"),
        Data("%c", ".L._printc_str0"),
        AlignInstr(),
        Label(label), // TODO: Possibly abstract common patterns (e.g. label after align and push/pop)?
        Push(RegisterLR, RegisterXZR),
        Move(RegisterX(0), RegisterX(1)),
        Address(".L._printc_str0", RegisterX(0)),
        BranchLink("printf"),
        Move(ImmNum(0), RegisterX(0)),
        BranchLink("fflush"),
        Pop(RegisterLR, RegisterXZR),
        ReturnI
    ) 
    val dependencies: List[InternalFunction] = List.empty
}

object printIntFx extends InternalFunction {
    val label: String = "_printi"
    val instructions: List[Instruction] = List (
        Comment("Print character as seen in the ref. compiler"),
        Comment("The pointer of the string to be printed is expected at X0"),
        Data("%d", ".L._printi_str0"),
        AlignInstr(),
        Label(label), // TODO: Possibly abstract common patterns (e.g. label after align and push/pop)?
        Push(RegisterLR, RegisterXZR),
        Move(RegisterX(0), RegisterX(1)),
        Address(".L._printi_str0", RegisterX(0)),
        BranchLink("printf"),
        Move(ImmNum(0), RegisterX(0)),
        BranchLink("fflush"),
        Pop(RegisterLR, RegisterXZR),
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
        AlignInstr(),
        Label(label), // TODO: Possibly abstract common patterns (e.g. label after align and push/pop)?
        Push(RegisterLR, RegisterXZR),


        
        Compare(RegisterW(0), RegisterWZR),
        BranchCond(".L_printb0", NeI),
        Address(".L._printb_str0", RegisterX(0)),
        Branch(".L_printb1"),

        Label(".L_printb0"),
        Address(".L._printb_str1", RegisterX(0)),

        Label(".L_printb1"),
        BranchLink(printStringFx.label),
        Pop(RegisterLR, RegisterXZR),
        ReturnI
    ) 

    val dependencies: List[InternalFunction] = List(printStringFx)
}

object printLineFx extends InternalFunction {
    val label: String = "_println"
    val instructions: List[Instruction] = List (
        Comment("Just puts down a new line - used in conjunction with _print[i|b|s|...]"),
        Data("\n", ".L._println_newline"),
        AlignInstr(),
        Label(label),
        Push(RegisterLR, RegisterXZR),

        Address(".L._println_newline", RegisterX(0)),
        BranchLink(printStringFx.label),

        Pop(RegisterLR, RegisterXZR),
        ReturnI
    )
    val dependencies: List[InternalFunction] = List(printStringFx)
}

object mallocFx extends InternalFunction {
    val label: String = "_malloc"
    val instructions: List[Instruction] = List (
        Comment("Allocating memory"),
        Label(label),
        Push(RegisterLR, RegisterXZR),
        BranchLink("malloc"),

        // Instead of CBZ, should do the same thing
        Compare(RegisterX(0), RegisterXZR), 
        BranchCond("_errOutOfMemory", EqI),

        Pop(RegisterLR, RegisterXZR),
        ReturnI
    )
    val dependencies: List[InternalFunction] = List(errorOutOfMemoryFx)
}

object readIntFx extends InternalFunction {
    val label: String = "_readi"
    val instructions: List[Instruction] = List (
        Comment("Read int as in the reference compiler"),
        Data("%d\n", ".L._readi_str0"),
        AlignInstr(),
        Label(label),
        Push(RegisterX(0), RegisterLR),
        Move(RegisterX(1), RegisterSP),
        Address(".L._readi_str0", RegisterX(0)),
        BranchLink("scanf"),
        Pop(RegisterX(0), RegisterLR),
        ReturnI
    )
    val dependencies: List[InternalFunction] = List.empty
}

object ArrayStoreFx extends InternalFunction {
    val label: String = "_arrStore4:"
    val instructions: List[Instruction] = List(
        Label(label),
        Comment("Special calling convention: array ptr passed in X7, index in X17, value to store in X8,LR (W30) is used as general register"),
        Push(RegisterLR, RegisterXZR),
        SignExWord(RegisterW(17), RegisterX(17)),
        Compare(RegisterW(17), RegisterXZR),
        CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), LtI),
        BranchCond(errorOutOfBoundsFx.label, LtI),
        Move(ImmNum(-4), RegisterX(9)), // Temporary (-4 should come from the size of S_INT)
        LoadWord(BaseOfsRA(RegisterX(7), RegisterX(9)), RegisterLR), // ldrsw lr, [x7, #-4]
        Compare(RegisterW(17), RegisterW(30)),
        CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), GeI),
        BranchCond(errorOutOfBoundsFx.label, GeI),
        Store(RegisterW(8), BaseOfsExtendShift(RegisterX(7), RegisterX(17), LiteralA("lsl"), Some(2))), // str w8, [x7, x17, lsl #2]
        Pop(RegisterLR, RegisterXZR),
        ReturnI
    )
    val dependencies: List[InternalFunction] = List(errorOutOfBoundsFx)
}

object ArrayLoadFx extends InternalFunction {
    val label: String = "_arrLoad4:"
    val instructions: List[Instruction] = List(
        Label(label),
        Comment("Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7"),
        Push(RegisterLR, RegisterXZR),
        SignExWord(RegisterW(17), RegisterX(17)),
        Compare(RegisterW(17), RegisterXZR),
        CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), LtI),
        BranchCond(errorOutOfBoundsFx.label, LtI),
        Move(ImmNum(-4), RegisterX(9)), // Temporary (-4 should come from the size of S_INT)
        LoadWord(BaseOfsRA(RegisterX(7), RegisterX(9)), RegisterLR), // ldrsw lr, [x7, #-4]
        Compare(RegisterW(17), RegisterW(30)),
        CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), GeI),
        BranchCond(errorOutOfBoundsFx.label, GeI),
        LoadWord(BaseOfsExtendShift(RegisterX(7), RegisterX(17), LiteralA("lsl"), Some(2)), RegisterX(7)), // ldrsw x7, [x7, x17, lsl #2]
        Pop(RegisterLR, RegisterXZR),
        ReturnI
    )
    val dependencies: List[InternalFunction] = List(errorOutOfBoundsFx)
}