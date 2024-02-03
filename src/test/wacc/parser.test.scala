package wacc

import parsley.Parsley._
import parsley.{Parsley, Result}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class parserTest extends AnyFlatSpec with BeforeAndAfterEach {

    val testAtomExpr = IntL(0)
    val notExitingStmt = Skip
    
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
        parser.funcEnd(Delimit(Return(testAtomExpr), Exit(testAtomExpr))) shouldBe true
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
        parser.funcEnd(Loop(testAtomExpr, notExitingStmt)) shouldBe false
        parser.funcEnd(Body(notExitingStmt)) shouldBe false
        }

    "The parse method" should "return the correct AST" in {
        parser.lvalue.parse("x").contains(LIdent("x"))
        // val testProgram = "int x"
        // // program.parse(testProgram) shouldBe Program(List(), Decl(IntT, "x", IntL(0)))
        // parser.stmt.parse("skip;") shouldBe Skip
        // val testFunc = "int f() is skip end"
        // val testParam = "int x"
        // val testDecl = "int x = 0"
        // val testAsgn = "x = 0"
        // val testRead = "read x"
        // val testFree = "free x"
        // val testReturn = "return 0"
        // val testExit = "exit 0"
        // val testPrint = "print 0"
        // val testPrintln = "println 0"
        // val testCond = "if 0 then skip else skip fi"
        // val testLoop = "while 0 do skip done"
        // val testBody = "begin skip end"
        // val testDelimit = "skip; skip"
        // val testLIdent = "x"
        // val testLArrElem = "x[0]"
        // val testIntL = "0"
        // val testBoolL = "true"
        // val testCharL = "'a'"
        // val testStrL = "\"test\""
        // val testIdent = "test"
        // val testArrElem = "test[0]"
        // val testArrLiter = "[0, 1, 2]"
        // val testNewPair = "newpair(0, 1)"
        // val testPairElem = "fst x"
        // val testCall = "call test(0, 1)"
        // val testType = "int"
        // val testPairElemType = "pair"
        // val testStmt = "skip"
        // val testExpr = "0"
        // val testLValue = "x"
        // val testRValue = "0"
        // val testParamList = "int x, int y"
        // val testPair = "pair(int, int)"
        // val testArrayT = "int[]"
        // val testArrayT2 = "int[][]"
        // val testArrayT3 = "pair(int, int)[]"
        // val testArrayT4 = "pair(int, int)[][]"
        // val testArrayT5 = "pair(int, int)[][][]"
        // val testArrayT6 = "pair(int, int)[][][][]"
        // val testArrayT7 = "pair(int, int)[][][][][]"
        // val testArrayT8 = "pair(int, int)[][][][][][]"
        // val testArrayT9 = "pair(int,
    }


}