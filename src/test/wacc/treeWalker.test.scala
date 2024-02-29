package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class treeWalkerTest extends AnyFlatSpec with BeforeAndAfterEach
{
    var sem = new Semantics("foo.txt")
    var frm = new Aarch64_formatter()
    var tw = new TreeWalker(sem, frm)
    var testGpRegsList = List.empty[Int]
    var testRegisterX = RegisterX(0)

    override protected def beforeEach(): Unit = {
        sem = new Semantics("foo.txt")
        frm = new Aarch64_formatter()
        sem.curSymTable = new SymTable(None, None)
        tw = new TreeWalker(sem, frm)
        testGpRegsList = frm.regConf.gpRegs.toList
        testRegisterX = RegisterX(0)
    }

    "translate method" should "generate the correct instruction list for simple expressions" in {
        tw.translate(IntL(0)(0,0), testGpRegsList) shouldBe List(Move(ImmNum(0), testRegisterX))
        tw.translate(IntL(100)(0,0), testGpRegsList) shouldBe List(Move(ImmNum(100), testRegisterX))
        tw.translate(BoolL(true)(0,0), testGpRegsList) shouldBe List(Move(ImmNum(1), testRegisterX))
        tw.translate(BoolL(false)(0,0), testGpRegsList) shouldBe List(Move(ImmNum(0), testRegisterX))
        tw.translate(CharL('k')(0,0), testGpRegsList) shouldBe List(Move(ImmNum(107), testRegisterX))
        tw.translate(StrL("hello")(0,0), testGpRegsList) shouldBe List(Address(".L.str0", testRegisterX))
        // TODO: tests for Ident, PairL, ArrElem
    }

    it should "generate the correct instruction list for UnOp expressions" in {
        // TODO: Review and chane the following test case
        //tw.translate(Not(BoolL(true)(0,0))(0,0), testGpRegsList) shouldBe List(Move(ImmNum(1),RegisterX(1)), Compare(RegisterX(0),ImmNum(1)), SetCond(RegisterXR,NeI))
        tw.translate(Neg(IntL(100)(0,0))(0,0), testGpRegsList) shouldBe List(Move(ImmNum(100),RegisterX(0)), Move(RegisterX(0), RegisterX(1)), Move(ImmNum(0),RegisterX(0)), SubI(RegisterX(1),RegisterX(0)))
        // TODO: Len(x), Ord(x), Chr(x)
    }

    it should "generate the correct instruction list for BinOp expressions" in {
        tw.translate(Add(IntL(100)(0,0), IntL(100)(0,0))(0,0), testGpRegsList) shouldBe 
            List(Move(ImmNum(100), RegisterX(0)), Push(RegisterX(0), RegisterXZR, PreIndxA(RegisterSP, -16)), Move(ImmNum(100), RegisterX(0)), Pop(PstIndxIA(RegisterSP, 16), RegisterX(1), RegisterXZR), AddI(RegisterX(1), RegisterX(0)))
        tw.translate(Minus(IntL(100)(0,0), IntL(100)(0,0))(0,0), testGpRegsList) shouldBe 
            List(Move(ImmNum(100), RegisterX(0)), Push(RegisterX(0), RegisterXZR, PreIndxA(RegisterSP, -16)), Move(ImmNum(100), RegisterX(0)), Pop(PstIndxIA(RegisterSP, 16), RegisterX(1), RegisterXZR), SubI(RegisterX(1), RegisterX(0)))
        tw.translate(Mul(IntL(100)(0,0), IntL(100)(0,0))(0,0), testGpRegsList) shouldBe 
            List(Move(ImmNum(100), RegisterX(0)), Push(RegisterX(0), RegisterXZR, PreIndxA(RegisterSP, -16)), Move(ImmNum(100), RegisterX(0)), Pop(PstIndxIA(RegisterSP, 16), RegisterX(1), RegisterXZR), MulI(RegisterX(1), RegisterX(0)))
        tw.translate(Div(IntL(100)(0,0), IntL(100)(0,0))(0,0), testGpRegsList) shouldBe 
            List(Move(ImmNum(100), RegisterX(0)), Push(RegisterX(0), RegisterXZR, PreIndxA(RegisterSP, -16)), Move(ImmNum(100), RegisterX(0)), Pop(PstIndxIA(RegisterSP, 16), RegisterX(1), RegisterXZR), Compare(RegisterXZR, RegisterX(1)), BranchCond("_errDivZero", EqI), DivI(RegisterX(1), RegisterX(0)))
    }
    
}