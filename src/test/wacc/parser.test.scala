package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class parserTest extends AnyFlatSpec with BeforeAndAfterEach {

    val testAtomExpr = IntL(0)
    

    "The funcEnd method" should "return true for Return and Exit statements" in {
        parser.funcEnd(Return(testAtomExpr)) shouldBe true
        parser.funcEnd(Exit(testAtomExpr)) shouldBe true
    }

    it should "return true for Cond statements with both of the results being exiting (also checks recursively)" in {
        parser.funcEnd(Cond(testAtomExpr, Return(testAtomExpr), Return(testAtomExpr))) shouldBe true
        parser.funcEnd(Cond(testAtomExpr, Exit(testAtomExpr), Exit(testAtomExpr))) shouldBe true
        parser.funcEnd(Cond(testAtomExpr, Return(testAtomExpr), Exit(testAtomExpr))) shouldBe true
        parser.funcEnd(Cond(testAtomExpr, Exit(testAtomExpr), Return(testAtomExpr))) shouldBe true
        parser.funcEnd(Cond(testAtomExpr, Cond(testAtomExpr, Return(testAtomExpr), Return(testAtomExpr)), Return(testAtomExpr))) shouldBe true
    }

    it should "return false for Cond statements with at least 1 of the result NOT being exiting (also checks recursively)" in {
        parser.funcEnd(Cond(testAtomExpr, Skip, Return(testAtomExpr))) shouldBe false
        parser.funcEnd(Cond(testAtomExpr, Exit(testAtomExpr), Skip)) shouldBe false
        parser.funcEnd(Cond(testAtomExpr, Skip, Skip)) shouldBe false
        parser.funcEnd(Cond(testAtomExpr, Return(testAtomExpr), Cond(testAtomExpr, Skip, Return(testAtomExpr)))) shouldBe false
    }
        
    it should "return true for Delimit statements with last statement being exiting (also checks recursively)" in {
        parser.funcEnd(Delimit(Return(testAtomExpr), Return(testAtomExpr))) shouldBe true
        parser.funcEnd(Delimit(Return(testAtomExpr), Exit(testAtomExpr))) shouldBe true
        parser.funcEnd(Delimit(Skip, Return(testAtomExpr))) shouldBe true
        parser.funcEnd(Delimit(Skip, Exit(testAtomExpr))) shouldBe true
        parser.funcEnd(Delimit(Skip, Delimit(Skip, Exit(testAtomExpr)))) shouldBe true
    }

    it should "return false for Delimit statements with last statement NOT being exiting (also checks recursively)" in {
        parser.funcEnd(Delimit(Return(testAtomExpr), Skip)) shouldBe false
        parser.funcEnd(Delimit(Exit(testAtomExpr), Skip)) shouldBe false
        parser.funcEnd(Delimit(Skip, Delimit(Exit(testAtomExpr), Skip))) shouldBe false
    }

    it should "return false for other statements" in {
        val testLValue = LIdent("test")
        val testIdent = "test"
        val testType = IntT

        parser.funcEnd(Skip) shouldBe false
        parser.funcEnd(Decl(testType, testIdent, testAtomExpr)) shouldBe false
        parser.funcEnd(Asgn(testLValue, testAtomExpr)) shouldBe false
        parser.funcEnd(Read(testLValue)) shouldBe false
        parser.funcEnd(Free(testAtomExpr)) shouldBe false
        parser.funcEnd(Print(testAtomExpr)) shouldBe false
        parser.funcEnd(Println(testAtomExpr)) shouldBe false
        parser.funcEnd(Loop(testAtomExpr, Skip)) shouldBe false
        parser.funcEnd(Body(Skip)) shouldBe false
        }




}