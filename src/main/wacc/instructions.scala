package wacc

sealed trait Instruction

case class Label(label: String) extends Instruction
case class Jump(label: String) extends Instruction
case class ReturnI(label: String) extends Instruction

// Replace operand with more specific classes if necessary
case class Move(src: Operand, dst: Register) extends Instruction
case class Load(src: Operand, dst: Register) extends Instruction
case class Pop(src1: Register, src2: Register, dst: Register) extends Instruction
case class Store(src: Register, dst: Register) extends Instruction
case class Push(src: Register, dst1: Register, dst2: Register) extends Instruction
case class Address(label: String, dst: Operand) extends Instruction
case class Branch(label: String) extends Instruction
case class BranchCond(label: String, cond: CondI) extends Instruction
case class BranchLink(label: String, addr: Int) extends Instruction // Calls function and stores address in the link register...

// May want to have the two operands and the destination as separate arguments
// case class Add(op1, op2, dst)

// Arity 2 operations: dst := dst <op> src
case class AddI(src: Operand, dst: Register) extends Instruction
case class SubI(src: Operand, dst: Register) extends Instruction
case class MulI(src: Operand, dst: Register) extends Instruction
case class DivI(src: Operand, dst: Register) extends Instruction

case class Compare(r1: Operand, r2: Operand) extends Instruction

// TODO: Fill in with all types of operands
sealed trait Operand
case class Register(addr: Int) extends Operand
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
