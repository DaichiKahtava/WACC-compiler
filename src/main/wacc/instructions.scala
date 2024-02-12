package wacc

sealed trait Instruction

case class Label(label: String) extends Instruction
case class Jump(label: String) extends Instruction
case class ReturnI(label: String) extends Instruction

// Replace operand with more specific classes if necessary
case class Move(src: Operand, dst: Operand) extends Instruction
case class Load(src: Operand, dst: Operand) extends Instruction
case class LoadPair() extends Instruction
case class Store(src: Operand, dst: Operand) extends Instruction
case class StorePair() extends Instruction
case class Address(src: Operand, dst: Operand) extends Instruction
case class Branch(label: String) extends Instruction
case class BranchCond(label: String, cond: Cond) extends Instruction
case class BranchLink(label: String, addr: Int) extends Instruction // Calls function and stores address in the link register...

// May want to have the two operands and the destination as separate arguments
// case class Add(op1, op2, dst)
case class AddI(src: Operand, dst: Operand) extends Instruction
case class SubI(src: Operand, dst: Operand) extends Instruction
case class MulI(src: Operand, dst: Operand) extends Instruction
case class DivI(src: Operand, dst: Operand) extends Instruction

case class Compare(r1: Operand, r2: Operand) extends Instruction

// TODO: Fill in with all types of operands
sealed trait Operand
case class Register(addr: Int) extends Operand
case class ImmNum(n: Int) extends Operand

sealed trait Cond
case object Eq extends Cond
case object Ne extends Cond
case object Cs extends Cond
case object CC extends Cond
case object Mi extends Cond
case object Pl extends Cond
case object Vs extends Cond
case object Vc extends Cond
case object Hi extends Cond
case object Ls extends Cond
case object Ge extends Cond
case object Lt extends Cond
case object Gt extends Cond
case object Le extends Cond
