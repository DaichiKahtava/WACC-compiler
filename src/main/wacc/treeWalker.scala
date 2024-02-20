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
    val fp = Register(29) // Frame pointer register
    val lr = Register(30) // Link register
    val sp = Register(31) // Stack pointer
    val xr = Register(8)  // Indirect result register

    // Conventions for translate: 
    //  regs contains a list of all available general purpose registers
    //  and the first register is used as the `return` register for the
    //  specific instruction


    def translate(e: Expr, regs: List[Register]): List[Instruction] = e match {

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
        case IntL(n) => List(Load(ImmNum(n), regs(0)))

        // Load the boolean value using integers.
        case BoolL(b) => List(Load(ImmNum(if (b) 1 else 0), regs(0)))

        // Obtain the integer value of a character.
        case CharL(c) => List(Load(ImmNum(c.toInt), regs(0)))
        
        case StrL(s) => {
            val label = aarch64_formatter.includeString(s)
            return List(Address(label, availRegs(0)))
        }

        case PairL() => 
            List(Load(ImmNum(0), regs(0))) // Treat as null pointer?

        // Pattern match for the identifier.
        case Ident(id) =>
            curSymTable.findVarGlobal(id) match {
                // Variable was found in the symbol table.
                case Some(varInfo) => List(Load(varInfo.asInstanceOf[Operand], regs(0)))
                // Variable was not found in the symbol table.
                case None => throw new RuntimeException(s"Undefined variable: $id")
        }

        case ArrElem(id, xs) => List()
    }

    def translate(list: List[Any], regs: List[Register]): List[Instruction] = list match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined list.")
    }

    def translate(program: Program): List[Instruction] = {
        var instructionList = List.empty[Instruction]
        program.funcs.foreach((f) => {
            curSymTable = curSymTable.findFunGlobal(f.id).get.st // We are in the local symbolTable.
            instructionList ++= translate(f)
            curSymTable = curSymTable.parent().get // We are in the parent/global symbolTable.
        })
        // TODO: Use blocks of sorts...
        instructionList ++= List(Label("main"))
        instructionList ++= translate(program.s, gpRegs.toList)
        return instructionList ++ List(ReturnI)
        // return instructionList // A bit redundant here? Can just return the generated List
    }

    def translate(func: Func): List[Instruction] = 
        // Callee saves and callee restore must go here.
        // Labels must be ensured unique
        Label(func.id) :: translate(func.s, gpRegs.toList) ++ List(ReturnI)

    def translate(stmt: Stmt, regs: List[Register]): List[Instruction] = stmt match {
        case Skip() => Nil
        case Decl(tp, id, rv) => ???
        case Asgn(lv, rv) => ???
        case Read(lv) => ???
        case Free(x) => ???
        case Return(x) => ???
        case Exit(x) => 
            translate(x, regs) ++
            List(Move(regs(0), xr),
            Move(availRegs(0), xr),
            // Caller saves must go here
            BranchLink("exit"),
            // Caller resotre must go here
            Move(ImmNum(0), availRegs(0))) 
        case Print(x) => ???
        case Println(x) => ???
        case Cond(x, s1, s2) => ???
        case Loop(x, s) => ???
        case Body(s) => ???
        case Delimit(s1, s2) => ???
    }

    def translate(lv: LValue, regs: List[Register]): List[Instruction] = lv match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined left value.")
    }

    def translate(rv: RValue, regs: List[Register]): List[Instruction] = rv match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined right value.")
    }

    def translate(pe: PairElem, regs: List[Register]): List[Instruction] = pe match {
        // Defaulting case.
        case _ => throw new RuntimeException("Undefined pair.")
    }
}