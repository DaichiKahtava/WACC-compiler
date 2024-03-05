package wacc

sealed trait InternalFunction {
    val label: String
    val dependencies: List[InternalFunction]
    val instructions: List[Instruction]
}

// Internal functions must always be called with callFx!

class errorBadCharFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_errBadChar"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = {
        val primary = frm.regConf.argRegs(0)
        List(
            Data("fatal error: int %d is not ascii character 0-127 \n", ".L._errBadChar_str0"),
            AlignInstr(),
            Label(label),
            Address(".L._errBadChar_str0", RegisterX(primary)),
            BranchLink("printf"),
            Move(ImmNum(0), RegisterX(primary)),
            BranchLink("fflush"),
            Move(ImmNum(-1), RegisterW(primary)),
            BranchLink("exit")
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[errorBadCharFx]
    override def hashCode(): Int = 1
}

class errorDivZeroFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_errDivZero"
    val dependencies: List[InternalFunction] = List(new printStringFx(frm))
    val instructions: List[Instruction] = {
        val primary = frm.regConf.argRegs(0)
        List(
            Comment("Division by zero error handler as seen in the ref. compiler"),
            Data("fatal error: division or modulo by zero\n", ".L._errDivZero_str0"),
            AlignInstr(),
            Label(label),
            Address(".L._errDivZero_str0", RegisterX(primary)),
            BranchLink("_prints"),
            Move(ImmNum(-1), RegisterW(primary)),
            BranchLink("exit")
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[errorDivZeroFx]
    override def hashCode(): Int = 2
}

class errorNullFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_errNull"
    val dependencies: List[InternalFunction] = List(new printStringFx(frm))
    val instructions: List[Instruction] = {
        val primary = frm.regConf.argRegs(0)
        List(
            Data("fatal error: null pair dereferenced or freed\n", ".L._errNull_str0"),
            AlignInstr(),
            Label(label),
            Address(".L._errNull_str0", RegisterX(primary)),
            BranchLink("_prints"),
            Move(ImmNum(-1), RegisterW(primary)),
            BranchLink("exit")
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[errorNullFx]
    override def hashCode(): Int = 3
}

class errorOutOfMemoryFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_errOutOfMemory"
    val dependencies: List[InternalFunction] = List(new printStringFx(frm))
    val instructions: List[Instruction] = {
        val primary = frm.regConf.argRegs(0)
        List (
            Data("fatal error: out of memory\n", ".L._errOutOfMemory_str0"),
            AlignInstr(),
            Label(label),
            Address(".L._errOutOfMemory_str0", RegisterX(primary)),
            BranchLink("_prints"),
            Move(ImmNum(-1), RegisterX(primary)),
            BranchLink("exit")
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[errorOutOfMemoryFx]
    override def hashCode(): Int = 4
}

class errorOutOfBoundsFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_errOutOfBounds"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = {
        val primary = frm.regConf.argRegs(0)
        List(
            // Assumes that X1 stores the index
            Data("fatal error: array index %d out of bounds\n", ".L._errOutOfBounds_str0"),
            AlignInstr(),
            Label(label),
            Address(".L._errOutOfBounds_str0", RegisterX(primary)),
            BranchLink("printf"),
            Move(ImmNum(0), RegisterX(primary)), 
            BranchLink("fflush"),
            Move(ImmNum(-1), RegisterW(primary)),
            BranchLink("exit")
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[errorOutOfBoundsFx]
    override def hashCode(): Int = 5
}

class errorOverFlowFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_errOverflow"
    val dependencies: List[InternalFunction] = List(new printStringFx(frm))
    val instructions: List[Instruction] = {
        val primary = frm.regConf.argRegs(0)
        List(
            Data("fatal error: integer overflow or underflow occurred\n", ".L._errOverflow_str0"),
            AlignInstr(),
            Label(label),
            Address(".L._errOverflow_str0", RegisterX(primary)),
            BranchLink("_prints"),
            Move(ImmNum(-1), RegisterW(primary)),
            BranchLink("exit")
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[errorOverFlowFx]
    override def hashCode(): Int = 6
}

class printStringFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_prints"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = {
        val primary = frm.regConf.argRegs(0) // X0
        val formatString = frm.regConf.argRegs(1) // X1
        val printString = frm.regConf.argRegs(2) // X2
        val sizeReg = frm.regConf.argRegs(3) // X3
        List (
            Comment("Print string as seen in the ref. compiler"),
            Comment("The pointer of the string to be printed is expected at X0"),
            Data("%.*s", ".L._prints_str0"),
            AlignInstr(),
            Label(label), // TODO: Possibly abstract common patterns (e.g. label after align and push/pop)?
            Push(RegisterLR, RegisterXZR),
            Move(RegisterX(primary), RegisterX(printString)),
            Move(ImmNum(-4), RegisterX(sizeReg)),
            LoadWord(BaseOfsRA(RegisterX(primary), RegisterX(sizeReg)), RegisterX(formatString)),
            Address(".L._prints_str0", RegisterX(primary)),
            BranchLink("printf"),
            Move(ImmNum(0), RegisterX(primary)),
            BranchLink("fflush"),
            Pop(RegisterLR, RegisterXZR),
            ReturnI
        ) 
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[printStringFx]
    override def hashCode(): Int = 7
}

class printCharFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_printc"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = {
            val primary   = frm.regConf.argRegs(0) // Initially has the character but then stores the format
            val secondary = frm.regConf.argRegs(1) // Stores the character for printf
            List (
                Comment("Print character as seen in the ref. compiler"),
                Comment("The pointer of the string to be printed is expected at X0"),
                Data("%c", ".L._printc_str0"),
                AlignInstr(),
                Label(label), // TODO: Possibly abstract common patterns (e.g. label after align and push/pop)?
                Push(RegisterLR, RegisterXZR),
                Move(RegisterX(primary), RegisterX(secondary)),
                Address(".L._printc_str0", RegisterX(primary)),
                BranchLink("printf"),
                Move(ImmNum(0), RegisterX(primary)),
                BranchLink("fflush"),
                Pop(RegisterLR, RegisterXZR),
                ReturnI
        ) 
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[printCharFx]
    override def hashCode(): Int = 8
}

class printIntFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_printi"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = {
        val primary   = frm.regConf.argRegs(0) // Initially has the integer but then stores the format
        val secondary = frm.regConf.argRegs(1) // Stores the integer for printf
        List (
            Comment("Print character as seen in the ref. compiler"),
            Comment("The pointer of the string to be printed is expected at X0"),
            Data("%d", ".L._printi_str0"),
            AlignInstr(),
            Label(label), // TODO: Possibly abstract common patterns (e.g. label after align and push/pop)?
            Push(RegisterLR, RegisterXZR),
            Move(RegisterX(primary), RegisterX(secondary)),
            Address(".L._printi_str0", RegisterX(primary)),
            BranchLink("printf"),
            Move(ImmNum(0), RegisterX(primary)),
            BranchLink("fflush"),
            Pop(RegisterLR, RegisterXZR),
            ReturnI
        ) 
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[printIntFx]
    override def hashCode(): Int = 9
}

class printBoolFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_printb"
    val dependencies: List[InternalFunction] = List(new printStringFx(frm))
    val instructions: List[Instruction] = {
        val booleanReg = frm.regConf.argRegs(0)
        List (
            Comment("Print bool as seen in the ref. compiler"),
            Data("false", ".L._printb_str0"),
            Data("true", ".L._printb_str1"),
            AlignInstr(),
            Label(label), // TODO: Possibly abstract common patterns (e.g. label after align and push/pop)?
            Push(RegisterLR, RegisterXZR),


            
            Compare(RegisterW(booleanReg), RegisterWZR),
            BranchCond(".L_printb0", NeI),
            Address(".L._printb_str0", RegisterX(booleanReg)),
            Branch(".L_printb1"),

            Label(".L_printb0"),
            Address(".L._printb_str1", RegisterX(booleanReg)),

            Label(".L_printb1"),
            BranchLink(dependencies(0).label),
            Pop(RegisterLR, RegisterXZR),
            ReturnI
        ) 
    }

    override def equals(x: Any): Boolean = x.isInstanceOf[printBoolFx]
    override def hashCode(): Int = 10
}

class printLineFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_println"
    val dependencies: List[InternalFunction] = List(new printStringFx(frm))
    val instructions: List[Instruction] = {
        val primary = frm.regConf.argRegs(0) // stores the address of the new line
        List (
            Comment("Just puts down a new line - used in conjunction with _print[i|b|s|...]"),
            Data("\n", ".L._println_newline"),
            AlignInstr(),
            Label(label),
            Push(RegisterLR, RegisterXZR),

            Address(".L._println_newline", RegisterX(primary)),
            BranchLink(dependencies(0).label),

            Pop(RegisterLR, RegisterXZR),
            ReturnI
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[printLineFx]
    override def hashCode(): Int = 11
}

class printPointerFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_printp"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = {
        // TODO: Label magic numbers like the other functions
        List(
            Data("%p", ".L._printp_str0"),
            AlignInstr(),
            Label(label),
            Push(RegisterLR, RegisterXZR),
            Move(RegisterX(0), RegisterX(1)),
            Address(".L._printp_str0", RegisterX(0)),
            BranchLink("printf"),
            Move(ImmNum(0), RegisterX(0)),
            BranchLink("fflush"),
            Pop(RegisterLR, RegisterXZR),
            ReturnI
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[printPointerFx]
    override def hashCode(): Int = 12
}

class mallocFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_malloc"
    val dependencies: List[InternalFunction] = List(new errorOutOfMemoryFx(frm))
    val instructions: List[Instruction] = {
        val addressReg = frm.regConf.resultRegister
        List (
            Comment("Allocating memory"),
            Label(label),
            Push(RegisterLR, RegisterXZR),
            BranchLink("malloc"),

            // No need to move the size as it is already stored where
            // we needs (first argument register == X0s)

            // Instead of CBZ, should do the same thing
            Compare(RegisterX(addressReg), RegisterXZR), 
            BranchCond("_errOutOfMemory", EqI),

            Pop(RegisterLR, RegisterXZR),
            ReturnI
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[mallocFx]
    override def hashCode(): Int = 13
}

class readIntFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_readi"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = {
        // KEY! The scanf returns the value in the stack
        val primary   = frm.regConf.argRegs(0) 
        // Stores the default value, then the format string and then the return value 
        val secondary = frm.regConf.argRegs(1) 
        // Temporary storage for stack pointer 
        List (
            Comment("Read int as in the ref. compiler"),
            Data("%d\n", ".L._readi_str0"),
            AlignInstr(),
            Label(label),
            Push(RegisterX(primary), RegisterLR),
            Move(RegisterSP, RegisterX(secondary)),
            Address(".L._readi_str0", RegisterX(primary)),
            BranchLink("scanf"),
            Pop(RegisterX(primary), RegisterLR),
            ReturnI
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[readIntFx]
    override def hashCode(): Int = 14
}

// Format predominantly similar to readIntFx (just above).
class readCharFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_readc"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = {
        val primary   = frm.regConf.argRegs(0) 
        val secondary = frm.regConf.argRegs(1)
        List(
            Comment("Read char as in the ref. compiler"),
            Data("%c\n", ".L._readc_str0"),
            AlignInstr(),
            Label(label),
            Push(RegisterX(primary), RegisterLR),
            Move(RegisterSP, RegisterX(secondary)),
            Address(".L._readc_str0", RegisterX(primary)),
            BranchLink("scanf"),
            Pop(RegisterX(primary), RegisterLR),
            ReturnI
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[readCharFx]
    override def hashCode(): Int = 15
}

class ArrayStoreFx(frm: Aarch64_formatter, val size: Int) extends InternalFunction {
    val label: String = s"_arrStore$size"
    val dependencies: List[InternalFunction] = List(new errorOutOfBoundsFx(frm))
    val instructions: List[Instruction] = {
        List(
            Label(label),
            Comment("Special calling convention: array ptr passed in X7, index in X17, value to store in X8,LR (W30) is used as general register"),
            Push(RegisterLR, RegisterXZR),
            SignExWord(RegisterW(17), RegisterX(17)),
            Compare(RegisterW(17), RegisterWZR),
            CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), LtI),
            BranchCond(dependencies(0).label, LtI),
            Move(ImmNum(-(frm.getSize(S_INT))), RegisterX(9)), // Temporary (-4 should come from the size of S_INT)
            LoadWord(BaseOfsRA(RegisterX(7), RegisterX(9)), RegisterLR), // ldrsw lr, [x7, #-4]
            Compare(RegisterW(17), RegisterW(30)),
            CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), GeI),
            BranchCond(dependencies(0).label, GeI),
            size match {
                case 1 => StoreByte(RegisterWR, BaseOfsRA(RegisterX(7), RegisterX(17)))
                case 4 => Store(RegisterWR, BaseOfsExtendShift(RegisterX(7), RegisterX(17), LiteralA("lsl"), Some(2))) // str w8, [x7, x17, lsl #2]
                case 8 => Store(RegisterXR, BaseOfsExtendShift(RegisterX(7), RegisterX(17), LiteralA("lsl"), Some(3))) // str x8, [x7, x17, lsl #3]
            },
            Pop(RegisterLR, RegisterXZR),
            ReturnI
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[ArrayStoreFx] && x.asInstanceOf[ArrayStoreFx].size == size
    override def hashCode(): Int = 100 + size
}

class ArrayLoadFx(frm: Aarch64_formatter, val size: Int) extends InternalFunction {
    val label: String = s"_arrLoad$size"
    val dependencies: List[InternalFunction] = List(new errorOutOfBoundsFx(frm))
    val instructions: List[Instruction] = {
        List(
            Label(label),
            Comment("Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7"),
            Push(RegisterLR, RegisterXZR),
            SignExWord(RegisterW(17), RegisterX(17)),
            Compare(RegisterW(17), RegisterWZR),
            CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), LtI),
            BranchCond(dependencies(0).label, LtI),
            Move(ImmNum(-(frm.getSize(S_INT))), RegisterX(9)), // Temporary (-4 should come from the size of S_INT)
            LoadWord(BaseOfsRA(RegisterX(7), RegisterX(9)), RegisterLR), // ldrsw lr, [x7, #-4]
            Compare(RegisterW(17), RegisterW(30)),
            CondSelect(RegisterX(17), RegisterX(1), RegisterX(1), GeI),
            BranchCond(dependencies(0).label, GeI),
            size match {
                case 1 => LoadByte(BaseOfsRA(RegisterX(7), RegisterX(17)), RegisterX(7), true)
                case 4 => LoadWord(BaseOfsExtendShift(RegisterX(7), RegisterX(17), LiteralA("lsl"), Some(2)), RegisterX(7)) // ldrsw x7, [x7, x17, lsl #2]
                case 8 => Load(BaseOfsExtendShift(RegisterX(7), RegisterX(17), LiteralA("lsl"), Some(3)), RegisterX(7)) // ldr x7, [x7, x17, lsl #3]
            },
            Pop(RegisterLR, RegisterXZR),
            ReturnI
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[ArrayLoadFx] && x.asInstanceOf[ArrayLoadFx].size == size
    override def hashCode(): Int = 200 + size
}

class PairLoadFx(frm: Aarch64_formatter) extends InternalFunction {
    val label: String = "_pairLoad"
    val dependencies: List[InternalFunction] = List.empty
    val instructions: List[Instruction] = {
        val address = frm.regConf.argRegs(0) // result
        val position = frm.regConf.argRegs(1)
        List(
            Label(label),
            // The first argument should be the address
            // The second argument should be whether the forst or second element are returned
            Comment("Gets either the first or second element from a pair"),
            Push(RegisterLR, RegisterXZR),

            Compare(RegisterX(position), RegisterXZR),
            BranchCond("_pairLoadPos0", EqI),
            Move(ImmNum(frm.getSize(S_ANY)), RegisterX(position)),
            Branch("_pairAfterLoad"),
            Label("_pairLoadPos0"),
            Move(ImmNum(0), RegisterX(position)),
            Label("_pairAfterLoad"),

            Load(BaseOfsRA(RegisterX(address), RegisterX(position)), RegisterX(address)),

            Pop(RegisterLR, RegisterXZR),
            ReturnI
        )
    }
    override def equals(x: Any): Boolean = x.isInstanceOf[PairLoadFx]
    override def hashCode(): Int = 16
}