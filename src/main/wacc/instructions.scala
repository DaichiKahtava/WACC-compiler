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

case class Push(src1: Register, src2: Register, dst: AdrMode) extends Instruction
case class Pop(src: AdrMode, dst1: Register, dst2: Register) extends Instruction

case class Load(src: AdrMode, dst: Register) extends Instruction
case class LoadWord(src: AdrMode, dst: Register) extends Instruction // always signed
case class LoadByte(src: AdrMode, dst: Register, signed: Boolean) extends Instruction
case class LoadHalf(src: AdrMode, dst: Register, signed: Boolean) extends Instruction
case class LoadRegSignedWord(src: AdrMode, dst: Register) extends Instruction
// Load and store pair are used for push and pop only.
case class Store(src: Register, dst: AdrMode) extends Instruction
case class StoreWord(src: Register, dst: AdrMode) extends Instruction
case class StoreByte(src: Register, dst: AdrMode) extends Instruction
case class StoreHalf(src: Register, dst: AdrMode) extends Instruction

case class SignExWord(src: RegisterW, dst: RegisterX) extends Instruction

// May want to have the two operands and the destination as separate arguments
// case class Add(op1, op2, dst)

// Arity 2 operations: dst := dst <op> src
case class AddI(src: Operand, dst: Register) extends Instruction
case class SubI(src: Operand, dst: Register) extends Instruction
case class MulI(src: Operand, dst: Register) extends Instruction
case class DivI(src: Operand, dst: Register) extends Instruction

case class SetCond(r: Register, cond: CondI) extends Instruction
case class Compare(r1: Operand, r2: Operand) extends Instruction
case class CondSelect(src1: Register, src2: Register, dst: Register, cond: CondI) extends Instruction


// TODO: Fill in with all types of operands
sealed trait Operand
case class ImmNum(n: Int) extends Operand
sealed trait Register extends Operand

// Groups of registers can go here
sealed trait RegisterXorSP extends Register


case class RegisterX(regN: Int) extends Register with RegisterXorSP
case class RegisterW(regN: Int) extends Register
case object RegisterXR extends Register // Indirect result register
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
case class BaseOfsIA(base: RegisterXorSP, ofs: Int) extends AdrMode 
// [base, Xm]
case class BaseOfsRA(base: RegisterXorSP, ofsReg: RegisterX, shift: Option[Int]) extends AdrMode 
// [base, Xm, extend, amount]
case class BaseOfsExtendShift(base: RegisterXorSP, ofsReg: RegisterX, extend: LiteralA, shift: Option[Int]) extends AdrMode
// [base #imm]!
case class PreIndxA(base: RegisterXorSP, ofs: Int) extends AdrMode 
// [base], #imm
case class PstIndxIA(base: RegisterXorSP, ofs: Int) extends AdrMode 
// [base], Xm
// Probably not needed as we are not doing SIMD instructions
case class PstIndxRA(base: RegisterXorSP, ofsReg: RegisterX) extends AdrMode 
// label
case class LiteralA(l: String) extends AdrMode 


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
