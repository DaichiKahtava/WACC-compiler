package wacc

import java.util.stream.Collectors
import scala.collection.mutable.ListBuffer

object aarch64_formatter {

    var errorDivZero = false
    var printString = false

    val internalFxs = collection.mutable.Set.empty[InternalFunction]

    // Global data for program. Note that data for internal programs are
    // Included in the internal programs themselves
    var stringLabelCounter = -1 // -1 means no string
    val stringLabel = ".L.str"
    val data = ListBuffer.empty[String]

    def generateAssembly(instructions: List[Instruction]): String = {
        val full_assembly = new StringBuilder

        // String literal data

        if (stringLabelCounter != -1) {
            full_assembly.addAll(".data\n")
            for (i <- 0 to (data.length - 1)) {
                val str = data(i)
                full_assembly.addAll("\t.word " + str.length + "\n" + stringLabel + i +":\n\t.asciz \"" + deescapeString(str) + "\"\n")
            }
        }

        // Pre-amble for instructions

        full_assembly.addAll(".align 4\n.text\n.global main\n")

        // Instruction list
        
        instructions.map(generateAssembly(_)).foreach(full_assembly.addAll(_))
        
        // Helper functions (taken from the reference compiler). 
        // TODO: Abstract them using Instruction
        

        internalFxs.foreach(f => f.instructions.map(generateAssembly(_)).foreach(full_assembly.addAll(_)))

        return full_assembly.result()
    }

    def includeString(s: String): String = {
        data.addOne(s)
        stringLabelCounter = stringLabelCounter + 1 
        return stringLabel + String.valueOf(stringLabelCounter)
    }

    def includeFx(f: InternalFunction) = {
        internalFxs.add(f)
    }

    def generateAssembly(instr: Instruction): String = instr match {
        case Comment(cmnt) => "// " + cmnt + "\n"
        
        case Label(label) => label + ":\n"
        case Data(s, l) => "\t.word " + s.length + "\n" + l +":\n\t.asciz \"" + deescapeString(s) + "\"\n"
        case AlignInstr() => ".align 4\n"


        case Jump(label) => "b\t" + label + "\n"
        case ReturnI => "ret\n"
        case Move(src, dst) => "mov\t" + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        
        case Load(src, dst) => "ldr\t" + generateRegister(dst) + ", " + generateAddress(src) + "\n"
        case LoadByte(src, dst, signed) => signed match {
            case false => "ldrb\t" + generateRegister(dst) + ", " + generateAddress(src) + "\n"
            case true  => "ldrsb\t" + generateRegister(dst) + ", " + generateAddress(src) + "\n"
        }
        case LoadHalf(src, dst, signed) => signed match {
            case false => "ldrh\t" + generateRegister(dst) + ", " + generateAddress(src) + "\n"
            case true  => "ldrsh\t" + generateRegister(dst) + ", " + generateAddress(src) + "\n"
        }
        case LoadWord(src, dst) => "ldrsw\t" + generateRegister(dst) + ", " + generateAddress(src) + "\n"
        case Pop(src, dst1, dst2) => "ldp\t" + generateRegister(dst1) + ", " +
            generateRegister(dst2) + ", " + generateAddress(src) + "\n"
            // TODO: do we need pair loading?

        case Store(src, dst) => "str\t" + generateRegister(src) + ", " + generateAddress(dst) + "\n" 
        case Push(src1, src2, dst) => "stp\t" + generateRegister(src1) + ", " +
            generateRegister(src2) + ", " + generateAddress(dst) + "\n" // TODO: Generalise offsets

        case Branch(label) => "b\t" + label
        case BranchCond(label, cond) => "b." + generateCondition(cond) + "\t" + label + "\n"
        case BranchLink(label) => "bl\t" + label + "\n"

        case AddI(src, dst) => "add\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        case SubI(src, dst) => "sub\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        case MulI(src, dst) => "mul\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        case DivI(src, dst) => {
            errorDivZero = true
            return "cmp\tXZR, " + generateOperand(src) + "\n" + 
            "b.eq\t_errDivZero\n"+
            "sdiv\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        }

        case Address(label, dst) => {
            val Register = generateRegister(dst)
            return "adrp\t" + generateRegister(dst) + ", " + label + "\n" +
                "add\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", :lo12:" + label + "\n"
        }
        
        case Compare(r1, r2) => "cmp\t" + generateOperand(r1) + generateOperand(r2) + "\n" + ""
        case SetCond(r, cond) => ???
    }

    def generateOperand(op: Operand): String = op match {
        // The validity of the operand should be checked before!
        case ImmNum(n) => "#" + String.valueOf(n)
        case r: Register => generateRegister(r)
    }

    def generateRegister(Register: Register): String = Register match {
        case RegisterXR => "X8"
        case RegisterFP => "fp"
        case RegisterLR => "lr"
        case RegisterSP => "sp"
        case RegisterWSP => "wsp"
        case RegisterXZR => "xzr"
        case RegisterWZR => "wzr"
        case RegisterX(n) => "X" + String.valueOf(n)
        case RegisterW(n) => "W" + String.valueOf(n)
    }

    def generateGPRegister(Register: Register): String = Register match {
        case RegisterXR => "X8"
        case RegisterFP => "X29"
        case RegisterLR => "X30"
        case RegisterSP => "X31"
        case RegisterWSP => "W31"
        case RegisterXZR => "X31"
        case RegisterWZR => "W31"
        case RegisterX(n) => "X" + String.valueOf(n)
        case RegisterW(n) => "W" + String.valueOf(n)
    }

    def generateAddress(a: AdrMode): String = a match {
        case BaseA(base) => "[" + generateRegister(base) + "]"
        case BaseOfsIA(base, ofs) => "[" + generateRegister(base) + ", #" + ofs + "]"
        case BaseOfsRA(base, ofsReg, shift) => "[" + generateRegister(base) + ", " +
            generateRegister(ofsReg) + "]"
        case PreIndxA(base, ofs) => "[" + generateRegister(base) + ", #" + ofs + "]!"
        case PstIndxIA(base, ofs) => "[" + generateRegister(base) + "], #" + ofs
        case PstIndxRA(base, ofsReg) => "[" + generateRegister(base) + "], " + generateRegister(ofsReg)
        case LiteralA(l) => l
    }

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

    def deescapeString(str: String): String = str.flatMap(deescapeChar)

    def deescapeChar(ch: Char): String = ch match {
        case '\u0000' => "\\0"
        case '\b' => "\\b"
        case '\t' => "\\t"
        case '\n' => "\\n"
        case '\f' => "\\f"
        case '\r' => "\\r"
        case '\"' => "\\\""
        case '\'' => "\\\'"
        case '\\' => "\\\\"
        case c: Char => String.valueOf(c)
    }
}
