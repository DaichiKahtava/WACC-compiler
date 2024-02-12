package wacc

sealed trait Instruction

case class Label(label: String) extends Instruction
case class Jump(label: String) extends Instruction
case class Return(label: String) extends Instruction

// Replace operand with more specific classes if necessary
case class Move(src: Operand, dst: Operand) extends Instruction
case class Load(src: Operand, dst: Operand) extends Instruction
case class Store(src: Operand, dst: Operand) extends Instruction
case class Address(src: Operand, dst: Operand) extends Instruction
case class Branch(label: String) extends Instruction
case class BranchL(label: String, addr: Int) extends Instruction // Calls function and stores address in the link register...

// May want to have the two operands and the destination as separate arguments
// case class Add(op1, op2, dst)
case class Add(src: Operand, dst: Operand) extends Instruction
case class Sub(src: Operand, dst: Operand) extends Instruction
case class Mul(src: Operand, dst: Operand) extends Instruction
case class Div(src: Operand, dst: Operand) extends Instruction

case class Compare(r1: Operand, r2: Operand) extends Instruction

// TODO: Fill in with all types of operands
sealed trait Operand
case class Register(addr: Int) extends Operand
case class ImmNum(n: Int) extends Operand