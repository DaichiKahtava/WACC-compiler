package wacc

import java.util.stream.Collectors
import scala.collection.mutable.ListBuffer

object aarch64_formatter {

    var errorDivZero = false
    var print = false

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
                full_assembly.addAll("\t.word " + str.length + "\n" + stringLabel + i +":\n\t.asciz \"" + str + "\"\n")
            }
        }

        // Pre-amble for instructions

        full_assembly.addAll(".align 4\n.text\n.global main\n")

        // Instruction list
        
        instructions.map(generateAssembly(_)).foreach(full_assembly.addAll(_))
        
        // Helper functions (taken from the reference compiler). 
        // TODO: Abstract them using Instruction
        
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

        if (print) {
            full_assembly.addAll("""
// length of .L._prints_str0
	.word 4
.L._prints_str0:
	.asciz "%.*s"
.align 4
_prints:
	// push {lr}
	stp lr, xzr, [sp, #-16]!
	mov x2, x0
	ldrsw x1, [x0, #-4]
	adr x0, .L._prints_str0
	bl printf
	mov x0, #0
	bl fflush
	// pop {lr}
	ldp lr, xzr, [sp], #16
	ret
""")
        }


        return full_assembly.result()
    }

    def includeString(s: String): String = {
        data.addOne(s)
        stringLabelCounter = stringLabelCounter + 1 
        return stringLabel + String.valueOf(stringLabelCounter)
    }

    def includePrint() = {
        print = true
    }

    def generateAssembly(instr: Instruction): String = instr match {
        case Label(label) => label + ":\n"
        case Jump(label) => "b\t" + label + "\n"
        case ReturnI => "ret\n"
        case Move(src, dst) => "mov\t" + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        
        case Load(src, dst) => "mov\t" + generateRegister(dst) + ", " + generateOperand(src) + "\n"
        case Pop(src, dst1, dst2) => "ldp\t" + generateRegister(dst1) + ", " +
            generateRegister(dst2) + ", [" + generateRegister(src) + "], #16\n"
            // TODO: provision for src to be an operand (+- do we need pairs?)

        case Store(src, dst) => "str\t" + generateRegister(src) + ", [" + generateRegister(dst) + "]\n" 
        case Push(src, dst1, dst2) => "stp\t" + generateRegister(dst1) + ", " +
            generateRegister(dst2) + ", [" + generateRegister(src) + ", #16]!\n" // TODO: Generalise offsets

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
        case RegisterXZR => "X31"
        case RegisterWZR => "X31"
        case RegisterX(n) => "X" + String.valueOf(n)
        case RegisterW(n) => "W" + String.valueOf(n)
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
}
