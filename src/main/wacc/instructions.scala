package wacc

sealed trait Instruction

case class Label(label: String) extends Instruction
case class Jump(label: String) extends Instruction
case object ReturnI extends Instruction

// Replace operand with more specific classes if necessary
case class Move(src: Operand, dst: Register) extends Instruction
case class Load(src: Operand, dst: Register) extends Instruction
case class Store(src: Register, dst: Register) extends Instruction
case class Address(label: String, dst: Register) extends Instruction
case class Branch(label: String) extends Instruction
case class BranchCond(label: String, cond: CondI) extends Instruction
case class BranchLink(label: String) extends Instruction // Calls function and stores address in the link register...

case class Push(src1: Register, src2: Register, dst: Register) extends Instruction
case class Pop(src: Register, dst1: Register, dst2: Register) extends Instruction

// May want to have the two operands and the destination as separate arguments
// case class Add(op1, op2, dst)

// Arity 2 operations: dst := dst <op> src
case class AddI(src: Operand, dst: Register) extends Instruction
case class SubI(src: Operand, dst: Register) extends Instruction
case class MulI(src: Operand, dst: Register) extends Instruction
case class DivI(src: Operand, dst: Register) extends Instruction

case class SetCond(r: Register, cond: CondI) extends Instruction
case class Compare(r1: Operand, r2: Operand) extends Instruction


// TODO: Fill in with all types of operands
sealed trait Operand
sealed trait Register extends Operand
case class RegisterX(regN: Int) extends Register
case class RegisterW(regN: Int) extends Register
case object RegisterXR extends Register // Indirect result register
case object RegisterFP extends Register // Frame pointer
case object RegisterLR extends Register // Link register
case object RegisterSP extends Register // Stack pointer
case object RegisterXZR extends Register // Zero register
case object RegisterWZR extends Register // Zero register

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
