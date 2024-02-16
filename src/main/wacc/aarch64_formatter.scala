package wacc

import java.util.stream.Collectors

object aarch64_formatter {
    def generateAssembly(instructions: List[Instruction]): String = {
        instructions.map(generateAssembly(_)).foldRight("")((s1,s2)=> s1++s2) // TODO: Perf. consid
    }

    def generateAssembly(instr: Instruction): String = instr match {
        case Label(label) => label + ":\n"
        case Jump(label) => "b\t" + label + "\n"
        case ReturnI(_) => "ret\n"
        case Move(src, dst) => "mov\t" + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        
        case Load(src, dst) => "mov\t" + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        case LoadPair(src, dst1, dst2) => "ldp\t" + generateRegister(dst1) + ", " +
            generateRegister(dst2) + ", [" + generateRegister(src) + "]\n"
            // TODO: provision for src to be an operand (+- do we need pairs?)

        case Store(src, dst) => "str\t" + generateRegister(src) + ", [" + generateRegister(dst) + "]\n" 
        case StorePair(src, dst1, dst2) => "stp\t" + generateRegister(dst1) + ", " +
            generateRegister(dst2) + ", [" + generateRegister(src) + "]\n"

        case Branch(label) => "b\t" + label
        case BranchCond(label, cond) => "b." + generateCondition(cond) + "\t" + label + "\n"
        case BranchLink(label, addr) => "bl\t" + String.valueOf(addr) + " " + label + "\n"

        case AddI(src, dst) => ??? // Need to decide on whether we convert arity of operations 
        case SubI(src, dst) => ??? //   from two to three elements here or at a previous stage  
        case MulI(src, dst) => ???
        case DivI(src, dst) => ???
        
        case Compare(r1, r2) => "cmp\t" + generateOperand(r1) + generateOperand(r2) + "\n"
    }

    def generateOperand(op: Operand): String = op match {
        // The validity of the operand should be checked before!
        case ImmNum(n) => "#" + String.valueOf(n)
        case r@Register(_) => generateRegister(r)
    }

    def generateRegister(reg: Register): String = "X" + String.valueOf(reg.addr)

    def generateCondition(cond: CondI): String =  cond match {
        case EqI => "eq"
        case NeI => "ne"
        case CsI => "cs"
        case CcI => "cc"
        case MiI => "mi"
        case PlI => "pl"
        case VsI => "vs"
        case VcI => "vc"
        case HiI => "hi"
        case LsI => "ls"
        case GeI => "ge"
        case LtI => "lt"
        case GtI => "gt"
        case LeI => "le"
    }
}
