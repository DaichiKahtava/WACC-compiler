package wacc

sealed trait Instruction

case class Label(label: String) extends Instruction
case class Jump(label: String) extends Instruction
case class ReturnI(label: String) extends Instruction

// Replace operand with more specific classes if necessary
case class Move(src: Operand, dst: Register) extends Instruction
case class Load(src: Operand, dst: Register) extends Instruction
case class LoadPair(src: Register, dst1: Register, dst2: Register) extends Instruction
case class Store(src: Register, dst: Register) extends Instruction
case class StorePair(src1: Register, src2: Register, dst: Register) extends Instruction
case class Address(label: String, dst: Operand) extends Instruction
case class Branch(label: String) extends Instruction
case class BranchCond(label: String, cond: CondI) extends Instruction
case class BranchLink(label: String, addr: Int) extends Instruction // Calls function and stores address in the link register...

// May want to have the two operands and the destination as separate arguments
// case class Add(op1, op2, dst)
case class AddI(src: Operand, dst: Register) extends Instruction
case class SubI(src: Operand, dst: Register) extends Instruction
case class MulI(src: Operand, dst: Register) extends Instruction
case class DivI(src: Operand, dst: Register) extends Instruction

case class Compare(r1: Operand, r2: Operand) extends Instruction

// TODO: Fill in with all types of operands
sealed trait Operand
sealed trait Register extends Operand
case object R0 extends Register
case object R1 extends Register
case object R2 extends Register
case object R3 extends Register
case object R4 extends Register
case object R5 extends Register
case object R6 extends Register
case object R7 extends Register
case object R8 extends Register
case object R9 extends Register
case object R10 extends Register
case object R11 extends Register
case object R12 extends Register
case object R13 extends Register
case object R14 extends Register
case object R15 extends Register
case object R16 extends Register
case object R17 extends Register
case object R18 extends Register
case object R19 extends Register
case object R20 extends Register
case object R21 extends Register
case object R22 extends Register
case object R23 extends Register
case object R24 extends Register
case object R25 extends Register
case object R26 extends Register
case object R27 extends Register
case object R28 extends Register
case object R29 extends Register
case object R30 extends Register
case object R31 extends Register

case class ImmNum(n: Int) extends Operand

sealed trait CondI
case object EqI extends CondI
case object NeI extends CondI
case object CsI extends CondI
case object CcI extends CondI
case object MiI extends CondI
case object PlI extends CondI
case object VsI extends CondI
case object VcI extends CondI
case object HiI extends CondI
case object LsI extends CondI
case object GeI extends CondI
case object LtI extends CondI
case object GtI extends CondI
case object LeI extends CondI
