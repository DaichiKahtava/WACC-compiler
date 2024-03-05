package wacc

sealed trait Instruction

case class Comment(cmnt: String) extends Instruction

case class Label(label: String) extends Instruction
case class Data(s: String, label: String) extends Instruction 
// Data puts the data where specified. This is useful for internal functions,
// but for the main program, use includeString instead.
// Note: String can use escaped characters like normal
case class AlignInstr() extends Instruction

case class Jump(label: String) extends Instruction
case object ReturnI extends Instruction

// All instructions are formed in such a way that data is flowing from
// The first argument towards the last argument that is a register.

// Replace operand with more specific classes if necessary
case class Move(src: Operand, dst: Register) extends Instruction
case class Address(label: String, dst: Register) extends Instruction
case class Branch(label: String) extends Instruction
case class BranchCond(label: String, cond: CondI) extends Instruction
case class BranchLink(label: String) extends Instruction // Calls function and stores address in the link register...

// Pushing two registers to stack
case class Push(src1: Register, src2: Register) extends Instruction
case class Pop(dst1: Register, dst2: Register) extends Instruction

case class Load(src: AdrMode, dst: Register) extends Instruction
case class LoadWord(src: AdrMode, dst: Register) extends Instruction // always signed
case class LoadByte(src: AdrMode, dst: Register, signed: Boolean) extends Instruction
case class LoadHalf(src: AdrMode, dst: Register, signed: Boolean) extends Instruction
// Load and store pair are used for push and pop only.
case class Store(src: Register, dst: AdrMode) extends Instruction
case class StoreWord(src: Register, dst: AdrMode) extends Instruction
case class StoreByte(src: Register, dst: AdrMode) extends Instruction
case class StoreHalf(src: Register, dst: AdrMode) extends Instruction

case class SignExWord(src: RegisterW, dst: RegisterX) extends Instruction

// May want to have the two operands and the destination as separate arguments
// case class Add(op1, op2, dst)

// Arity 2 operations: dst := dst <op> src
case class AddI(src: Register, dst: Register) extends Instruction
case class SubI(src: Register, dst: Register) extends Instruction
case class MulI(src: Register, dst: Register) extends Instruction
case class DivI(src: Register, dst: Register) extends Instruction

case class SetCond(r: Register, cond: CondI) extends Instruction
case class Compare(r1: Register, r2: Register) extends Instruction
case class CondSelect(src1: Register, src2: Register, dst: Register, cond: CondI) extends Instruction

case class isChar(src: Register) extends Instruction // Checks if the value is a valid character

// TODO: Fill in with all types of operands
sealed trait Operand 
//Operand is either a register or an immediate (used for Move only).
sealed trait Register extends Operand

// Groups of registers can go here
sealed trait RegisterXorSP extends Register


case class RegisterX(regN: Int) extends Register with RegisterXorSP {
  val w = RegisterW(regN)
}
case class RegisterW(regN: Int) extends Register 
case object RegisterXR extends Register // Indirect result register
case object RegisterWR extends Register
case object RegisterFP extends Register // Frame pointer
case object RegisterLR extends Register // Link register
case object RegisterSP extends Register with RegisterXorSP // Stack pointer
case object RegisterWSP extends Register // Stack pointer
case object RegisterXZR extends Register // Zero register
case object RegisterWZR extends Register // Zero register


sealed trait AdrMode
// [base]
case class BaseA(base: RegisterXorSP) extends AdrMode 
// [base, #imm]
// case class BaseOfsIA(base: RegisterXorSP, ofs: Int) extends AdrMode 

// [base, Xm]
// Used to replicate [base, #imm] as per the immediate values' convention
case class BaseOfsRA(base: RegisterXorSP, ofsReg: RegisterX) extends AdrMode 
// [base, Xm, extend, amount] - to be removed
case class BaseOfsExtendShift(base: RegisterXorSP, ofsReg: RegisterX, extend: LiteralA, shift: Option[Int]) extends AdrMode

// [base #imm]!
//case class PreIndxA(base: RegisterXorSP, ofs: Int) extends AdrMode 

// [base], #imm
case class PstIndxIA(base: RegisterXorSP, ofs: Int) extends AdrMode 
// [base], Xm
// used to replicate: [base], #imm
case class PstIndxRA(base: RegisterXorSP, ofsReg: RegisterX) extends AdrMode 
// label
case class LiteralA(l: String) extends AdrMode 

case class ImmNum(n: Int) extends Operand
/*
    Convention for use of immediate values:
    - Binary Operations & Comparisons occur only between registers
      -> To do a binary operation with an immediate, load immediate to scratch register first 
    - Save and Store instructions only use a register as an offset
    - Immediate can be loaded through the Move instruction (which is the pseudocode moc in Aarch64)

    This removes the need for checks for ImmNum.
*/

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
