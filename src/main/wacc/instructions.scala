package wacc

sealed trait Instruction

case class Label(label: String) extends Instruction
case class Jump(fName: String) extends Instruction
case object Return extends Instruction

case class Move(dest: Operand, src: Operand) extends Instruction

case class Add(dest: Operand, src: Operand) extends Instruction
case class Sub(dest: Operand, src: Operand) extends Instruction
case class Mul(dest: Operand, src: Operand) extends Instruction
case class Div(dest: Operand, src: Operand) extends Instruction

case class Compare(r1: Operand, r2: Operand) extends Instruction

sealed trait Operand

case class Register() extends Operand