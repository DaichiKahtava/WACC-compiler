package wacc

import java.util.stream.Collectors
import scala.collection.mutable.ListBuffer
import java.io._

class Aarch64_formatter() {

    var errorDivZero = false
    var printString = false

    val internalFxs = collection.mutable.Set.empty[InternalFunction]

    // Register configuration 
    val regConf = new registerConfig{
        val callerRegs = (0 to 7).toList ++ (9 to 15).toList
        val argRegs = (0 to 7).toList
        val scratchRegs = (8 to 15).toList
        val calleeRegs = (19 to 28).toList
        val variabRegs = calleeRegs
        val gpRegs = callerRegs ++ calleeRegs
        val funcLabel = "wacc_user_"
        val resultRegister = 0
        val pointerReg = 16
        val offsetReg = 17
        val framePReg = 29
        val stackAlign: Int = 16
    }

    // Global data for program. Note that data for internal programs are
    // Included in the internal programs themselves
    var stringLabelCounter = -1 // -1 means no string
    val stringLabel = ".L.str"
    val data = ListBuffer.empty[String]

    def generateAssembly(instructions: List[Instruction], filename: String): Unit = {
        // Create a new PrintWriter instance for the output file.
        val writer = new PrintWriter(new File(filename))

        // String literal data.

        // Check if there are any string labels to be written.
        if (stringLabelCounter != -1) {
            writer.write(".data\n")
            for (i <- 0 to (data.length - 1)) {
                val str = data(i)
                writer.write("\t.word " + str.length + "\n" + stringLabel + i +":\n\t.asciz \"" + deescapeString(str) + "\"\n")
            }
        }

        // Write the pre-amble for instructions.
        writer.write(".align 4\n.text\n.global main\n")
        
        // Convert the list of instructions to a LazyList, generate assembly code for each instruction, and write it to the file.
        // LazyList chosen over Stream due to deprecation of Streams.
        instructions.to(LazyList).map(generateAssembly).foreach(writer.write)
        
        // Generate assembly code for internal functions
        internalFxs.to(LazyList).flatMap(f => f.instructions.map(generateAssembly)).foreach(writer.write)

        // Close the PrintWriter to ensure all output is written and resources are released.
        writer.close()
    }

    def includeString(s: String): String = {
        data.addOne(s)
        stringLabelCounter = stringLabelCounter + 1 
        return stringLabel + String.valueOf(stringLabelCounter)
    }

    def includeFx(f: InternalFunction): String = {
        internalFxs.add(f)
        f.dependencies.foreach(includeFx) // TODO: Provision for cycles in dependencies.
        f.label
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
        case Pop(dst1, dst2) => "ldp\t" + generateRegister(dst1) + ", " +
            generateRegister(dst2) + ", [" + generateRegister(RegisterSP) + "], #" + (getSize(S_ANY) * 2) + "\n"

        case Store(src, dst) => "str\t" + generateRegister(src) + ", " + generateAddress(dst) + "\n"
        case StoreByte(src, dst) => "strb\t" + generateRegister(src) + ", " + generateAddress(dst) + "\n"
        case StoreHalf(src, dst) => "strh\t" + generateRegister(src) + ", " + generateAddress(dst) + "\n"
        case StoreWord(src, dst) => "strw\t" + generateRegister(src) + ", " + generateAddress(dst) + "\n"
        case Push(src1, src2) => "stp\t" + generateRegister(src1) + ", " +
            generateRegister(src2) + ", [" + generateRegister(RegisterSP) + ", #" + (getSize(S_ANY) * -2) + "]!\n" 

        case SignExWord(src, dst) => "sxtw\t" + generateRegister(dst) + ", " + generateRegister(src) + "\n"

        case Branch(label) => "b\t" + label + "\n"
        case BranchCond(label, cond) => "b." + generateCondition(cond) + "\t" + label + "\n"
        case BranchLink(label) => "bl\t" + label + "\n"

        case AddI(src, dst) => "add\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        case SubI(src, dst) => "sub\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        case MulI(src, dst) => "mul\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        case DivI(src, dst) => "sdiv\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", " + generateOperand(src) + "\n"

        case Address(label, dst) => {
            val Register = generateRegister(dst)
            return "adrp\t" + generateRegister(dst) + ", " + label + "\n" +
                "add\t" + generateRegister(dst) + ", " + generateRegister(dst) + ", :lo12:" + label + "\n"
        }
        
        case Compare(r1, r2) => "cmp\t" + generateOperand(r1) + ", " + generateOperand(r2) + "\n" + ""
        case SetCond(r, cond) => "cset\t" + generateOperand(r) + ", " + generateCondition(cond) + "\n"
        case CondSelect(src1, src2, dst, cond) => 
            "csel\t" + generateRegister(dst) + ", " + generateRegister(src1) + ", " + 
            generateRegister(src2) + ", " + generateCondition(cond) + "\n"
    }

    def generateOperand(op: Operand): String = op match {
        // The validity of the operand should be checked before!
        case ImmNum(n) => "#" + String.valueOf(n)
        case r: Register => generateRegister(r)
    }

    def generateRegister(Register: Register): String = Register match {
        case RegisterXR => "X8"
        case RegisterWR => "W8"
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
        case RegisterWR => "W8"
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
        case BaseOfsRA(base, ofsReg) => "[" + generateRegister(base) + ", " +
            generateRegister(ofsReg) + "]"
        case BaseOfsExtendShift(base, ofsReg, extend, shift) => // TO REMOVE
            "[" + generateRegister(base) + ", " + generateRegister(ofsReg) + ", " + 
            generateAddress(extend) + " #" + shift.get + "]"
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

    def getSize(t: S_TYPE) = t match {
        // Returns number of bytes
        // <Addresses have 8 bytes -  the size of a register>
        case S_INT => 4
        case S_BOOL => 1
        case S_STRING => 8
        case S_CHAR => 1
        case S_ARRAY(tp) => 8
        case S_PAIR(tp1, tp2) => 8
        case S_ERASED => 8
        case S_ANY => 8 // size of a register.
        case S_EMPTYARR => 8
    }
}
