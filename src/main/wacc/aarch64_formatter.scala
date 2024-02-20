package wacc

import java.util.stream.Collectors
import scala.collection.mutable.ListBuffer

object aarch64_formatter {

    var errorDivZero = false

    var stringLabelCounter = -1 // -1 means no string
    val stringLabel = ".str"
    val data = ListBuffer.empty[String]

    def generateAssembly(instructions: List[Instruction]): String = {
        val full_assembly = new StringBuilder

        // String literal data

        if (stringLabelCounter != -1) {
            full_assembly.addAll(".data\n")
            for (i <- 0 to (data.length)) {
                val str = data(i)
                full_assembly.addAll("\t.word" + str.length +"\n\t.asciz " + str + "\n")
            }
        }

        // Pre-amble for instructions

        full_assembly.addAll(".align 4\n.text\n.global main\n")

        // Instruction list
        
        instructions.map(generateAssembly(_)).foreach(full_assembly.addAll(_))
        
        // Error handling functions.
        
        if (errorDivZero) {
            full_assembly.addAll("""
	// Division by zero error handler as seen in the ref. compiler
	// length of .L._errDivZero_str0
	.word 40
.L._errDivZero_str0:
	.asciz "fatal error: division or modulo by zero\n"
.align 4
_errDivZero:
	adr x0, .L._errDivZero_str0
	bl _prints
	mov w0, #-1
	bl exit
""")
        }


        return full_assembly.result()
    }

    def includeString(s: String): String = {
        data.addOne(s)
        stringLabelCounter += stringLabelCounter
        return stringLabel + String.valueOf(stringLabelCounter)
    }

    def generateAssembly(instr: Instruction): String = instr match {
        case Label(label) => label + ":\n"
        case Jump(label) => "b\t" + label + "\n"
        case ReturnI => "ret\n"
        case Move(src, dst) => "mov\t" + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        
        case Load(src, dst) => "mov\t" + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        case Pop(src, dst1, dst2) => "ldp\t" + generateRegister(dst1) + ", " +
            generateRegister(dst2) + ", [" + generateRegister(src) + "]\n"
            // TODO: provision for src to be an operand (+- do we need pairs?)

        case Store(src, dst) => "str\t" + generateRegister(src) + ", [" + generateRegister(dst) + "]\n" 
        case Push(src, dst1, dst2) => "stp\t" + generateRegister(dst1) + ", " +
            generateRegister(dst2) + ", [" + generateRegister(src) + "]\n"

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

        case Address(label, dst) => "adr\t" + generateOperand(dst) + ", " + label + "\n"
        
        case Compare(r1, r2) => "cmp\t" + generateOperand(r1) + generateOperand(r2) + "\n" +
        ""
    }

    def generateOperand(op: Operand): String = op match {
        // The validity of the operand should be checked before!
        case ImmNum(n) => "#" + String.valueOf(n)
        case r@Register(_) => generateRegister(r)
    }

    def generateRegister(reg: Register): String = "X" + String.valueOf(reg.regN)

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
