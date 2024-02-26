package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class treeWalkerTest extends AnyFlatSpec with BeforeAndAfterEach
{
    val sem = new Semantics("foo.txt")
    val tw = new TreeWalker(sem)
    val testGpRegsList = tw.gpRegs.toList
    val testRegisterX = RegisterX(0)

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
        tw.translate(Not(BoolL(true)(0,0))(0,0), testGpRegsList) shouldBe List(Move(ImmNum(1),RegisterX(1)), Compare(RegisterX(0),ImmNum(1)), SetCond(RegisterXR,NeI))
        tw.translate(Neg(IntL(100)(0,0))(0,0), testGpRegsList) shouldBe List(Move(ImmNum(100),RegisterX(1)), Move(ImmNum(0),RegisterX(0)), SubI(RegisterX(1),RegisterX(0)))
        // TODO: Len(x), Ord(x), Chr(x)
    }
    
}