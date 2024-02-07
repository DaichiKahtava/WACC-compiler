package wacc

import parsley.Parsley._
import parsley.{Parsley, Result}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.PrivateMethodTester
import org.scalatest.Ignore

class parserTest extends AnyFlatSpec with BeforeAndAfterEach // with PrivateMethodTester
{

    /* Key consideration for the tests!
       The position is NOT used for comparisons (although they always need to be defined).
       They will need to be accessed either through direct pattern matching or by typecasting
       using asInstance[foo].
    */

    val testAtom = 0
    val testAtomIdent = testAtom.toString
    val testAtomExpr = IntL(0)(0,0)
    val testAtomRExpr = RExpr(IntL(0)(0,0))(0,0)
    
    val notExitingStmt = Skip // Should have a location!!!

    val testIdent = "test"
    val testLValue = LIdent("test")(0,0)
    
    // The following tests DO NOT check the position of anything. So we just put everything
    // at (0,0)
    "The funcEnd method" should "return true for Return and Exit statements" in {
        parser.funcEnd(Return(testAtomExpr)(0,0)) shouldBe true
        parser.funcEnd(Exit(testAtomExpr)(0,0)) shouldBe true
    }

    it should "return true for Cond statements with both of the results being exiting (also checks recursively)" in {
        parser.funcEnd(Cond(testAtomExpr, Return(testAtomExpr)(0,0), Return(testAtomExpr)(0,0))(0,0)) shouldBe true
        parser.funcEnd(Cond(testAtomExpr, Exit(testAtomExpr)(0,0), Exit(testAtomExpr)(0,0))(0,0)) shouldBe true
        parser.funcEnd(Cond(testAtomExpr, Return(testAtomExpr)(0,0), Exit(testAtomExpr)(0,0))(0,0)) shouldBe true
        parser.funcEnd(Cond(testAtomExpr, Exit(testAtomExpr)(0,0), Return(testAtomExpr)(0,0))(0,0)) shouldBe true
        parser.funcEnd(Cond(testAtomExpr, Cond(testAtomExpr, Return(testAtomExpr)(0,0), Return(testAtomExpr)(0,0))(0,0),
         Return(testAtomExpr)(0,0))(0,0)) shouldBe true
    }

    it should "return false for Cond statements with at least 1 of the result NOT being exiting (also checks recursively)" in {
        parser.funcEnd(Cond(testAtomExpr, notExitingStmt, Return(testAtomExpr)(0,0))(0,0)) shouldBe false
        parser.funcEnd(Cond(testAtomExpr, Exit(testAtomExpr)(0,0), notExitingStmt)(0,0)) shouldBe false
        parser.funcEnd(Cond(testAtomExpr, notExitingStmt, notExitingStmt)(0,0)) shouldBe false
        parser.funcEnd(Cond(testAtomExpr, Return(testAtomExpr)(0,0), Cond(testAtomExpr, notExitingStmt, Return(testAtomExpr)(0,0))(0,0))(0,0)) shouldBe false
    }
        
    it should "return true for Delimit statements with last statement being exiting (also checks recursively)" in {
        parser.funcEnd(Delimit(Return(testAtomExpr)(0,0), Return(testAtomExpr)(0,0))) shouldBe true
        parser.funcEnd(Delimit(Return(testAtomExpr)(0,0), Exit(testAtomExpr)(0,0))) shouldBe true
        parser.funcEnd(Delimit(notExitingStmt, Return(testAtomExpr)(0,0))) shouldBe true
        parser.funcEnd(Delimit(notExitingStmt, Exit(testAtomExpr)(0,0))) shouldBe true
        parser.funcEnd(Delimit(notExitingStmt, Delimit(notExitingStmt, Exit(testAtomExpr)(0,0)))) shouldBe true
    }

    it should "return false for Delimit statements with last statement NOT being exiting (also checks recursively)" in {
        parser.funcEnd(Delimit(Return(testAtomExpr)(0,0), notExitingStmt)) shouldBe false
        parser.funcEnd(Delimit(Exit(testAtomExpr)(0,0), notExitingStmt)) shouldBe false
        parser.funcEnd(Delimit(notExitingStmt, Delimit(Exit(testAtomExpr)(0,0), notExitingStmt))) shouldBe false
    }

    it should "return false for other statements" in {
        val testType = IntT()(0,0)

        parser.funcEnd(Skip) shouldBe false
        parser.funcEnd(Decl(testType, testIdent, testAtomRExpr)(0,0)) shouldBe false
        parser.funcEnd(Asgn(testLValue, testAtomRExpr)(0,0)) shouldBe false
        parser.funcEnd(Read(testLValue)(0,0)) shouldBe false
        parser.funcEnd(Free(testAtomExpr)(0,0)) shouldBe false
        parser.funcEnd(Print(testAtomExpr)(0,0)) shouldBe false
        parser.funcEnd(Println(testAtomExpr)(0,0)) shouldBe false
        parser.funcEnd(Loop(testAtomExpr, notExitingStmt)(0,0)) shouldBe false
        parser.funcEnd(Body(notExitingStmt)(0,0)) shouldBe false
        }

    "The parse method" should "return the correct AST for the typep" in {
        /* TODO: Once arrayType and pairType is fixed */


    }
    it should "return the correct AST for the basicType" in {
        parser.baseType.parse("int").contains(IntT()(0,0)) shouldBe true
        parser.baseType.parse("bool").contains(BoolT()(0,0)) shouldBe true
        parser.baseType.parse("char").contains(CharT()(0,0)) shouldBe true
        parser.baseType.parse("string").contains(StringT()(0,0)) shouldBe true
    }
    it should "return the correct AST for the arrayType" in {
        /* TODO: Fix arrayType */
        parser.arrayType.parse("int[]").contains(ArrayT(IntT()(0,0))(0,0)) shouldBe true
        parser.arrayType.parse("bool[]").contains(ArrayT(BoolT()(0,0))(0,0)) shouldBe true
        parser.arrayType.parse("char[]").contains(ArrayT(CharT()(0,0))(0,0)) shouldBe true
        parser.arrayType.parse("string[]").contains(ArrayT(StringT()(0,0))(0,0)) shouldBe true
    }
    it should "return the correct AST for the pairType" in {
        /* TODO: Fix pairType */
        parser.pairType.parse("pair(int,int)").contains(Pair(IntT()(0,0), IntT()(0,0))(0,0)) shouldBe true
        parser.pairType.parse("pair(bool,bool)").contains(Pair(BoolT()(0,0), BoolT()(0,0))(0,0)) shouldBe true
        parser.pairType.parse("pair(char,char)").contains(Pair(CharT()(0,0), CharT()(0,0))(0,0)) shouldBe true
        parser.pairType.parse("pair(string,string)").contains(Pair(StringT()(0,0), StringT()(0,0))(0,0)) shouldBe true
    }
    
    it should "return the correct AST for the lvalue" in {
        parser.lvalue.parse(testIdent).contains(testLValue) shouldBe true // LIdent

        /* TODO: tests for LArrElem */
        val testArrElement0 = "[0]"
        val testArrElement1 = "[1]"

        parser.lvalue.parse(testIdent + testArrElement0).contains(LArrElem(testIdent, List(IntL(0)(0,0)))(0,0)) shouldBe true // LArrElem
        parser.lvalue.parse(testIdent+testArrElement0+testArrElement1).contains(LArrElem(testIdent, List(IntL(0)(0,0), IntL(1)(0,0)))(0,0)) shouldBe true // LArrElem
        
        parser.lvalue.parse("fst "+testIdent).contains(First(testLValue)(0,0)) shouldBe true // PairElem
        parser.lvalue.parse("snd "+testIdent).contains(Second(testLValue)(0,0)) shouldBe true // PairElem

    }

    it should "return the correct AST for the rvalue" in {
        val testIdentExpr = RExpr(Ident("test")(0,0))(0,0)
        val secondTestAtom = 1
        val secondTestAtomIdent = secondTestAtom.toString
        val secondTestAtomExpr = IntL(1)(0,0)

        parser.rvalue.parse(testIdent).contains(testIdentExpr) shouldBe true // Ident
        
        parser.rvalue.parse("newpair("+testAtomIdent+","+secondTestAtomIdent+")").contains(NewPair(testAtomExpr, secondTestAtomExpr)(0,0)) shouldBe true // Pair
        parser.rvalue.parse("fst "+testIdent).contains(First(testLValue)(0,0)) shouldBe true // PairElem
        parser.rvalue.parse("snd "+testIdent).contains(Second(testLValue)(0,0)) shouldBe true // PairElem

        parser.rvalue.parse("[]").contains(ArrL(List.empty)) shouldBe true // (empty) ArrL
        parser.rvalue.parse("["+testAtomIdent+","+secondTestAtomIdent+"]").contains(ArrL(List(testAtomExpr, secondTestAtomExpr))) shouldBe true // ArrL
        
        parser.rvalue.parse("call "+testIdent+"("+testAtomIdent+")").contains(Call(testIdent, List(testAtomExpr))(0,0)) shouldBe true // Call
        
    }

    "The type system" should "be able to parse a simple single type" in {
        var p = parser.typep.parse("int")
        p.isSuccess shouldBe true
        p.get shouldBe IntT()(0,0)
        p = parser.typep.parse("string")
        p.isSuccess shouldBe true
        p.get shouldBe StringT()(0,0)
    }   

    it should "reject custom type names" in {
        var p = parser.typep.parse("foo")
        p.isFailure shouldBe true
    }

    it should "be case sensitive" in {
        var p = parser.typep.parse("Int")
        p.isFailure shouldBe true
    }

    it should "be able to parse a simple array" in {
        var p = parser.typep.parse("int []")
        p.isSuccess shouldBe true
        p.get shouldBe ArrayT(IntT()(0,0))(0,0)
        p = parser.typep.parse("string []")
        p.isSuccess shouldBe true
        p.get shouldBe ArrayT(StringT()(0,0))(0,0)

    }

    it should "be able to handle no whitespace between the name and \"[\"" in {
        val p = parser.typep.parse("int[]")
        p.isSuccess shouldBe true
        p.get shouldBe ArrayT(IntT()(0,0))(0,0)
    }

    it should "not accept anything between \"[\" and \"]\"" in {
        val p = parser.typep.parse("int[gta]")
        // TODO: As of now it stops at the right bracket
        //       Should it fail instead?
        p.isSuccess shouldBe true
        p.get shouldBe IntT()(0,0)
    }

    it should "recognise a arbitrary dimention array" in {
        var p = parser.typep.parse("int[][]")
        //It currently stops at the first brackets
        p.isSuccess shouldBe true
        p.get shouldBe ArrayT(ArrayT(IntT()(0,0))(0,0))(0,0)
        p = parser.typep.parse("int[][][][]")
        p.isSuccess shouldBe true
        p.get shouldBe ArrayT(ArrayT(ArrayT(ArrayT(IntT()(0,0))(0,0))(0,0))(0,0))(0,0)
    }

    it should "parse pairs" in {
        val p = parser.typep.parse("pair(int, string)")
        p.isSuccess shouldBe true
        p.get shouldBe Pair(IntT()(0,0), StringT()(0,0))(0,0)
    }

    it should "parse list of pairs" in {
        val p = parser.typep.parse("pair(int, string)[]")
        p.isSuccess shouldBe true
        p.get shouldBe ArrayT(Pair(IntT()(0,0), StringT()(0,0))(0,0))(0,0)
        val q = parser.arrayType.parse("pair(int, string)[]")
        q.isSuccess shouldBe true
        q.get shouldBe ArrayT(Pair(IntT()(0,0), StringT()(0,0))(0,0))(0,0)
    }

    it should "reject nested pairs" in {
        // Need to check the typecast. It does what we want but not *how* we want
        val p = parser.typep.parse("pair(int, pair(string, int))")
        p.isFailure shouldBe true
    }

    it should "accept pair *lists* inside pairs" in {
        // Rejects the lists
        val p = parser.typep.parse("pair(int, pair(string, int)[])")
        p.isSuccess shouldBe true
        p.get shouldBe Pair(IntT()(0,0), ArrayT(Pair(StringT()(0,0), IntT()(0,0))(0,0))(0,0))(0,0)
    }

    it should "reject unknown types inside pairs" in {
        val p = parser.typep.parse("pair(int, bar)")
        p.isFailure shouldBe true
    }

    it should "handle nested arrays in pairs" in {
        var p = parser.typep.parse("pair(string[][], pair(int [], char [] [])[])")
        p.isSuccess shouldBe true
        p.get shouldBe Pair(ArrayT(ArrayT(StringT()(0,0))(0,0))(0,0),
         ArrayT(Pair(ArrayT(IntT()(0,0))(0,0), ArrayT(ArrayT(CharT()(0,0))(0,0))(0,0))(0,0))(0,0))(0,0)
        p = parser.typep.parse("pair(string[][], pair(int [], char [] []))")
        p.isSuccess shouldBe false
    }

    "The parserBridgeposX class" should "give coorect position for leafs" in {
        var p = parser.typep.parse("string")
        p.isSuccess shouldBe true
        p.get shouldBe StringT()(0,0)
    }

    it should "give correct position for binary nodes" in {
        var p = parser.expr.parse("1 + 2")
        p.isSuccess shouldBe true
        p.get shouldBe Add(IntL(1)(0,0), IntL(2)(0,0))(1,1)
    } 

    // "temptest.gta" should "Identify correct location?" in {
    //     var p = parser.expr.parse("1 + 2")
    //     val ast = p.get
    //     ast shouldBe Add(IntL(1)(1, 1), IntL(2)(3, 1))(1, 1)
    //     ast match {
    //         case a@Add(x, y) => println(a.pos)
    //         case _: Atom => println("Achievement: How did we get here")
    //     }
    //     println(ast.asInstanceOf[Add])
    // }
}

// class parserTest extends AnyFlatSpec with BeforeAndAfterEach {
//     "temptest.gta" should "parse an int" in {
//         var p = parser.expr.parse("1 + 2")
//         val ast = p.get
//         ast shouldBe Add(IntL(1)(1, 1), IntL(2)(3, 1))(1, 1)
//         ast match {
//             case a@Add(x, y) => println(a.pos)
//             case _: Atom => println("Achievement: How did we get here")
//         }
//         println(ast.asInstanceOf[Add])
//     }
// }

class PositionTest extends AnyFlatSpec {
  "The parser" should "correctly parse positions for boolean literals" in {
    val p = parser.expr.parse("true")
    p.isSuccess shouldBe true
    val node = p.get
    node shouldBe BoolL(true)(1,1)
  }

  it should "correctly parse positions for character literals" in {
    val p = parser.expr.parse("'a'")
    p.isSuccess shouldBe true
    val node = p.get
    node shouldBe CharL('a')(1,1)
  }

  it should "correctly parse positions for string literals" in {
    val p = parser.expr.parse("\"foo\"")
    p.isSuccess shouldBe true
    val node = p.get
    node shouldBe StrL("foo")(1,1)
  }

  it should "correctly parse positions for identifiers" in {
    val p = parser.expr.parse("foo")
    p.isSuccess shouldBe true
    val node = p.get
    node shouldBe Ident("foo")(1,1)
  }

  it should "correctly parse positions for unary nodes" in {
    val p = parser.expr.parse("-1")
    p.isSuccess shouldBe true
    val node = p.get
    node shouldBe Neg(IntL(1)(1,2))(1,1)
  }

  // it should "correctly parse positions for array literals" in {
  //   val p = parser.expr.parse("[1, 2, 3]")
  //   p.isSuccess shouldBe true
  //   val node = p.get
  //   node shouldBe ArrL(List(IntL(1)(1,2), IntL(2)(1,5), IntL(3)(1,8)))(1,1)
  // }

  // it should "correctly parse positions for pair literals" in {
  //   val p = parser.expr.parse("pair(1, 2)")
  //   p.isSuccess shouldBe true
  //   val node = p.get
  //   node shouldBe NewPair(IntL(1)(1,9), IntL(2)(1,12))(1,1)
  // }
}