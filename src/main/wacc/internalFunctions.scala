package wacc

sealed trait InternalFunction {
    val label: String
    val dependencies: List[InternalFunction]
    val instructions: List[Instruction]
}

// Internal functions must always be called with callFx!

class errorDivZeroFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_errDivZero"
    val dependencies: List[InternalFunction] = List(new printStringFx(argRegs))
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
    override def equals(x: Any): Boolean = x.isInstanceOf[errorDivZeroFx]
    override def hashCode(): Int = 1
}

class errorOutOfMemoryFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_errOutOfMemory"
    val dependencies: List[InternalFunction] = List(new printStringFx(argRegs))
    val instructions: List[Instruction] = List (
        Data("fatal error: out of memory\n", ".L._errOutOfMemory_str0"),
        AlignInstr(),
        Label(label),
        Address(".L._errOutOfMemory_str0", RegisterX(0)),
        BranchLink("_prints"),
        Move(ImmNum(-1), RegisterX(0)),
        BranchLink("exit")
    )
    override def equals(x: Any): Boolean = x.isInstanceOf[errorOutOfMemoryFx]
    override def hashCode(): Int = 2
}

class errorOutOfBoundsFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_errOutOfBounds"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = List(
        // Assumes that X1 stores the index
        AlignInstr(),
        Label(label),
        Address(".L._errOutOfBounds_str0", RegisterX(0)),
        BranchLink("printf"),
        Move(ImmNum(0), RegisterX(0)), 
        BranchLink("fflush"),
        Move(ImmNum(-1), RegisterW(0)),
        BranchLink("exit")
    )
    override def equals(x: Any): Boolean = x.isInstanceOf[errorOutOfBoundsFx]
    override def hashCode(): Int = 3
}

class printStringFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_prints"
    val dependencies: List[InternalFunction] = List.empty
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
    override def equals(x: Any): Boolean = x.isInstanceOf[printStringFx]
    override def hashCode(): Int = 4
}

class printCharFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_printc"
    val dependencies: List[InternalFunction] = List.empty
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
    override def equals(x: Any): Boolean = x.isInstanceOf[printCharFx]
    override def hashCode(): Int = 5
}

class printIntFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_printi"
    val dependencies: List[InternalFunction] = List.empty
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
    override def equals(x: Any): Boolean = x.isInstanceOf[printIntFx]
    override def hashCode(): Int = 6
}

class printBoolFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_printb"
    val dependencies: List[InternalFunction] = List(new printStringFx(argRegs))
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
        BranchLink(dependencies(0).label),
        Pop(RegisterLR, RegisterXZR),
        ReturnI
    ) 

    override def equals(x: Any): Boolean = x.isInstanceOf[printBoolFx]
    override def hashCode(): Int = 7
}

class printLineFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_println"
    val dependencies: List[InternalFunction] = List(new printStringFx(argRegs))
    val instructions: List[Instruction] = List (
        Comment("Just puts down a new line - used in conjunction with _print[i|b|s|...]"),
        Data("\n", ".L._println_newline"),
        AlignInstr(),
        Label(label),
        Push(RegisterLR, RegisterXZR),

        Address(".L._println_newline", RegisterX(0)),
        BranchLink(dependencies(0).label),

        Pop(RegisterLR, RegisterXZR),
        ReturnI
    )
    override def equals(x: Any): Boolean = x.isInstanceOf[printLineFx]
    override def hashCode(): Int = 8
}

class mallocFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_malloc"
    val dependencies: List[InternalFunction] = List(new errorOutOfMemoryFx(argRegs))
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
    override def equals(x: Any): Boolean = x.isInstanceOf[mallocFx]
    override def hashCode(): Int = 9
}

class readIntFx(argRegs: List[Int]) extends InternalFunction {
    val label: String = "_readi"
    val dependencies: List[InternalFunction] = List.empty
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
    override def equals(x: Any): Boolean = x.isInstanceOf[readIntFx]
    override def hashCode(): Int = 10
}

class ArrayStoreFx(argRegs: List[Int], val size: Int) extends InternalFunction {
    val label: String = "_arrStore4:"
    val dependencies: List[InternalFunction] = List(new errorOutOfBoundsFx(argRegs))
    val instructions: List[Instruction] = List(
        Label(label),
        Comment("Special calling convention: array ptr passed in X7, index in X17, value to store in X8,LR (W30) is used as general register"),
        Push(RegisterLR, RegisterXZR),
        SignExWord(RegisterW(17), RegisterX(17)),
        Compare(RegisterW(17), RegisterXZR),
        CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), LtI),
        BranchCond(dependencies(0).label, LtI),
        Move(ImmNum(-4), RegisterX(9)), // Temporary (-4 should come from the size of S_INT)
        LoadWord(BaseOfsRA(RegisterX(7), RegisterX(9)), RegisterLR), // ldrsw lr, [x7, #-4]
        Compare(RegisterW(17), RegisterW(30)),
        CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), GeI),
        BranchCond(dependencies(0).label, GeI),
        Store(RegisterW(8), BaseOfsExtendShift(RegisterX(7), RegisterX(17), LiteralA("lsl"), Some(2))), // str w8, [x7, x17, lsl #2]
        Pop(RegisterLR, RegisterXZR),
        ReturnI
    )
    override def equals(x: Any): Boolean = x.isInstanceOf[ArrayStoreFx] && x.asInstanceOf[ArrayStoreFx].size == size
    override def hashCode(): Int = 11
}

class ArrayLoadFx(argRegs: List[Int], val size: Int) extends InternalFunction {
    val label: String = "_arrLoad4:"
    val dependencies: List[InternalFunction] = List(new errorOutOfBoundsFx(argRegs))
    val instructions: List[Instruction] = List(
        Label(label),
        Comment("Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7"),
        Push(RegisterLR, RegisterXZR),
        SignExWord(RegisterW(17), RegisterX(17)),
        Compare(RegisterW(17), RegisterXZR),
        CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), LtI),
        BranchCond(dependencies(0).label, LtI),
        Move(ImmNum(-4), RegisterX(9)), // Temporary (-4 should come from the size of S_INT)
        LoadWord(BaseOfsRA(RegisterX(7), RegisterX(9)), RegisterLR), // ldrsw lr, [x7, #-4]
        Compare(RegisterW(17), RegisterW(30)),
        CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), GeI),
        BranchCond(dependencies(0).label, GeI),
        LoadWord(BaseOfsExtendShift(RegisterX(7), RegisterX(17), LiteralA("lsl"), Some(2)), RegisterX(7)), // ldrsw x7, [x7, x17, lsl #2]
        Pop(RegisterLR, RegisterXZR),
        ReturnI
    )
    override def equals(x: Any): Boolean = x.isInstanceOf[ArrayLoadFx] && x.asInstanceOf[ArrayStoreFx].size == size
    override def hashCode(): Int = 12
}