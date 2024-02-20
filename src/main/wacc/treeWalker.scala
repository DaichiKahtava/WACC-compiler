package wacc

import scala.collection.mutable.ListBuffer

class TreeWalker(var curSymTable: SymTable) {
    // GLOBAL POINTER TO THE FINAL (NOT-FORMATTED) ASSEMBLY CODE
    var instructionList = List[Instruction]()
    var gpRegs = ListBuffer.empty[Register]
    for (n <- 0 to 15) gpRegs.addOne(Register(n))
    for (n <- 19 to 28) gpRegs.addOne(Register(n))

    var availRegs = ListBuffer.empty[Register]
    availRegs.addAll(gpRegs)
    
    val outputRegister = Register(0)
    val fp = Register(29)
    val lr = Register(30)
    val sp = Register(31)

    def translate(e: Expr, regs: List[Register]): List[Instruction] = e match {
        // Conventions: regs contains a list of all available registers and the first register
        //              is used as the return register


        // UnOp expressions.
        case Not(x) => List()
        case Neg(x) => List()
        case Len(x) => List()
        case Ord(x) => List()
        case Chr(x) => List()

        // BinOp expressions.
        case Mod(x, y) => List()
        case Add(x, y) => 
            val dst = regs.head
            val nxt = regs.tail.head
            translate(x, regs) ++ translate(y, regs.tail) ++ List(AddI(nxt, dst))
        
        case Minus(x, y) => 
            val dst = regs.head
            val nxt = regs.tail.head
            translate(x, regs) ++ translate(y, regs.tail) ++ List(SubI(nxt, dst))

        case Mul(x, y) =>
            val dst = regs.head
            val nxt = regs.tail.head
            translate(x, regs) ++ translate(y, regs.tail) ++ List(MulI(nxt, dst))

        case Div(x, y) =>
            val dst = regs.head
            val nxt = regs.tail.head
            translate(x, regs) ++ translate(y, regs.tail) ++ List(DivI(nxt, dst))

        case GrT(x, y) => List()
        case GrEqT(x, y) => List()
        case LsT(x, y) => List()
        case LsEqT(x, y) => List()
        case Eq(x, y) => List()
        case NEq(x, y) => List()
        case And(x, y) => List()
        case Or(x, y) => List()

        // Atom expressions.

        // Load the integer directly.
        case IntL(n) => List(Load(ImmNum(n), outputRegister))

        // Load the boolean value using integers.
        case BoolL(b) => List(Load(ImmNum(if (b) 1 else 0), outputRegister))

        // Obtain the integer value of a character.
        case CharL(c) => List(Load(ImmNum(c.toInt), outputRegister))
        
        case StrL(s) => {
            val label = aarch64_formatter.includeString(s)
            return List(Address(label, availRegs(0)))
        }

        case PairL() => 
            List(Load(ImmNum(0), outputRegister)) // Treat as null pointer?

        // Pattern match for the identifier.
        case Ident(id) =>
            curSymTable.findVarGlobal(id) match {
                // Variable was found in the symbol table.
                case Some(varInfo) => List(Load(varInfo.asInstanceOf[Operand], outputRegister))
                // Variable was not found in the symbol table.
                case None => throw new RuntimeException(s"Undefined variable: $id")
        }

        case ArrElem(id, xs) => List()
    }

    def translate(list: List[Any]): List[Instruction] = list match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined list.")
    }

    def translate(program: Program): List[Instruction] = {
        program.funcs.foreach((f) => {
            curSymTable = curSymTable.findFunGlobal(f.id).get.st // We are in the local symbolTable.
            translate(f.s)
            curSymTable = curSymTable.parent().get // We are in the parent/global symbolTable.
        })
        instructionList ++ translate(program.s)
        return instructionList // A bit redundant here? Can just return the generated List
    }

    def translate(func: Func): List[Instruction] = 
        Label(func.id) :: translate(func.s) ++ List(ReturnI(func.id))

    def translate(stmt: Stmt): List[Instruction] = stmt match {
        case Skip() => Nil
        case Decl(tp, id, rv) => ???
        case Asgn(lv, rv) => ???
        case Read(lv) => ???
        case Free(x) => ???
        case Return(x) => ???
        case Exit(x) => ???
        case Print(x) => ???
        case Println(x) => ???
        case Cond(x, s1, s2) => ???
        case Loop(x, s) => ???
        case Body(s) => ???
    }

    def translate(lv: LValue): List[Instruction] = lv match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined left value.")
    }

    def translate(rv: RValue): List[Instruction] = rv match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined right value.")
    }

    def translate(pe: PairElem): List[Instruction] = pe match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined pair.")
    }
}