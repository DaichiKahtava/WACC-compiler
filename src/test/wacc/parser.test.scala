package wacc

import parsley.Parsley._
import parsley.{Parsley, Result}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.PrivateMethodTester

class parserTest extends AnyFlatSpec with BeforeAndAfterEach // with PrivateMethodTester
{
    val testAtom = 0
    val testAtomIdent = testAtom.toString
    val testAtomExpr = IntL(0)
    
    val notExitingStmt = Skip

    val testIdent = "test"
    val testLValue = LIdent("test")
    
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
        parser.funcEnd(Cond(testAtomExpr, notExitingStmt, Return(testAtomExpr))) shouldBe false
        parser.funcEnd(Cond(testAtomExpr, Exit(testAtomExpr), notExitingStmt)) shouldBe false
        parser.funcEnd(Cond(testAtomExpr, notExitingStmt, notExitingStmt)) shouldBe false
        parser.funcEnd(Cond(testAtomExpr, Return(testAtomExpr), Cond(testAtomExpr, notExitingStmt, Return(testAtomExpr)))) shouldBe false
    }
        
    it should "return true for Delimit statements with last statement being exiting (also checks recursively)" in {
        parser.funcEnd(Delimit(Return(testAtomExpr), Return(testAtomExpr))) shouldBe true
        parser.funcEnd(Delimit(RExpreturn(testAtomExpr), Exit(testAtomExpr))) shouldBe true
        parser.funcEnd(Delimit(notExitingStmt, Return(testAtomExpr))) shouldBe true
        parser.funcEnd(Delimit(notExitingStmt, Exit(testAtomExpr))) shouldBe true
        parser.funcEnd(Delimit(notExitingStmt, Delimit(notExitingStmt, Exit(testAtomExpr)))) shouldBe true
    }

    it should "return false for Delimit statements with last statement NOT being exiting (also checks recursively)" in {
        parser.funcEnd(Delimit(Return(testAtomExpr), notExitingStmt)) shouldBe false
        parser.funcEnd(Delimit(Exit(testAtomExpr), notExitingStmt)) shouldBe false
        parser.funcEnd(Delimit(notExitingStmt, Delimit(Exit(testAtomExpr), notExitingStmt))) shouldBe false
    }

    it should "return false for other statements" in {
        val testType = IntT

        parser.funcEnd(Skip) shouldBe false
        parser.funcEnd(Decl(testType, testIdent, testAtomExpr)) shouldBe false
        parser.funcEnd(Asgn(testLValue, testAtomExpr)) shouldBe false
        parser.funcEnd(Read(testLValue)) shouldBe false
        parser.funcEnd(Free(testAtomExpr)) shouldBe false
        parser.funcEnd(Print(testAtomExpr)) shouldBe false
        parser.funcEnd(Println(testAtomExpr)) shouldBe false
        parser.funcEnd(Loop(testAtomExpr, notExitingStmt)) shouldBe false
        parser.funcEnd(Body(notExitingStmt)) shouldBe false
        }

    "The parse method" should "return the correct AST for the typep" in {
        /* TODO: Once arrayType and pairType is fixed */


    }
    it should "return the correct AST for the basicType" in {
        parser.baseType.parse("int").contains(IntT) shouldBe true
        parser.baseType.parse("bool").contains(BoolT) shouldBe true
        parser.baseType.parse("char").contains(CharT) shouldBe true
        parser.baseType.parse("string").contains(StringT) shouldBe true
    }
    it should "return the correct AST for the arrayType" in {
        /* TODO: Fix arrayType */
        // parser.arrayType.parse("int[]").contains(ArrayT(IntT)) shouldBe true
        // parser.arrayType.parse("bool[]").contains(ArrayT(BoolT)) shouldBe true
        // parser.arrayType.parse("char[]").contains(ArrayT(CharT)) shouldBe true
        // parser.arrayType.parse("string[]").contains(ArrayT(StringT)) shouldBe true
    }
    it should "return the correct AST for the pairType" in {
        /* TODO: Fix pairType */
        // parser.pairType.parse("pair(int,int)").contains(Pair(IntT, IntT)) shouldBe true
        // parser.pairType.parse("pair(bool,bool)").contains(Pair(BoolT, BoolT)) shouldBe true
        // parser.pairType.parse("pair(char,char)").contains(Pair(CharT, CharT)) shouldBe true
        // parser.pairType.parse("pair(string,string)").contains(Pair(StringT, StringT)) shouldBe true
    }
    
    it should "return the correct AST for the lvalue" in {
        parser.lvalue.parse(testIdent).contains(testLValue) shouldBe true // LIdent

        /* TODO: tests for LArrElem */
        // val testArrElement0 = "[0]"
        // val testArrElement1 = "[1]"
        // parser.lvalue.parse(testIdent+testArrElement0).contains(LArrElem(testIdent, Some(IntL(0)))) shouldBe true // LArrElem
        // parser.lvalue.parse(testIdent+testArrElement0+testArrElement1).contains(LArrElem(testIdent, Some(IntL(0)), Some(IntL(1)))) shouldBe true // LArrElem
        
        parser.lvalue.parse("fst "+testIdent).contains(First(testLValue)) shouldBe true // PairElem
        parser.lvalue.parse("snd "+testIdent).contains(Second(testLValue)) shouldBe true // PairElem

    }

    it should "return the correct AST for the rvalue" in {
        val testIdentExpr = Ident("test")
        parser.rvalue.parse(testIdent).contains(testIdentExpr) shouldBe true // Ident

        /* TODO: tests for ArrElem */
        // parser.rvalue.parse("test[0]").contains(ArrElem("test", List(IntL(0)))) shouldBe true // ArrElem
        // parser.rvalue.parse("test[0][1]").contains(ArrElem("test", List(IntL(0), IntL(1)))) shouldBe true // ArrElem

        /* TODO: tests for newPair */
        // parser.rvalue.parse("newpair(0,1)").contains(NewPair(IntL(0), IntL(1))) shouldBe true // NewPair
        // println(parser.rvalue.parse("fst "+testIdent))
        parser.rvalue.parse("fst "+testIdent).contains(First(testLValue)) shouldBe true // PairElem
        parser.rvalue.parse("snd "+testIdent).contains(Second(testLValue)) shouldBe true // PairElem

        /* TODO: tests for Call */
        // println(parser.rvalue.parse("call "+testIdent+"("+testAtomIdent+")"))
        // parser.rvalue.parse("call "+testIdent+"("+testAtomIdent+")").contains(Call(testIdent, List(testAtomExpr))) shouldBe true // CURRENTLY OUTPUTS: Success(Ident(call))
        
    }

    


}