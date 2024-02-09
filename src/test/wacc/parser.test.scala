package wacc

import parsley.Parsley._
import parsley.{Parsley, Result}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.PrivateMethodTester
import org.scalatest.Ignore
import parsley.debug._

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
    
    val notExitingStmt = Skip()(0, 0) // Should have a location!!!

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

        parser.funcEnd(Skip()(0, 0)) shouldBe false
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
}

class PositionTest extends AnyFlatSpec {
  "The parser" should "correctly parse positions for boolean literals" in {
    val p = parser.expr.parse("true")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case b: BoolL =>
        b.b shouldBe true
        b.pos shouldBe (1,1)
      case _ =>
        fail("Parsing failed to produce a BoolL.")
    }
  }

  it should "correctly parse positions for character literals" in {
    val p = parser.expr.parse("'a'")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case c: CharL =>
        c.c shouldBe 'a'  
        c.pos shouldBe (1, 1)
      case _ => 
        fail("Parsing failed to produce a CharL.")
    } 
  }

  it should "correctly parse positions for string literals" in {
    val p = parser.expr.parse("\"foo\"")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case s: StrL =>
        s.s shouldBe "foo" 
        s.pos shouldBe (1, 1)
      case _ => 
        fail("Parsing failed to produce a StrL.")
    } 
  }

  it should "correctly parse positions for identifiers" in {
    val p = parser.expr.parse("foo")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case i: Ident =>
        i.id shouldBe "foo" 
        i.pos shouldBe (1, 1)
      case _ => 
        fail("Parsing failed to produce an Ident.")
    } 
  }

  it should "correctly parse positions for unary nodes" in {
    val p = parser.expr.parse("-1")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case n: Neg =>
        n.x match {
          case intL: IntL => 
            intL.n shouldBe 1 
            intL.pos shouldBe (1, 2)
          case _ =>
            fail("Inner expression is not an IntL.")
        } 
        n.pos shouldBe (1, 1)
      case _ => 
        fail("Parsing failed to produce a Neg.")
    } 
  }

  it should "correctly parse positions for unary minus expressions" in {
    val p = parser.expr.parse("-foo")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case n: Neg =>
        n.x match {
          case ident: Ident => 
            ident.id shouldBe "foo" 
            ident.pos shouldBe (1, 2)
          case _ =>
            fail("Inner expression is not an Ident.")
        } 
        n.pos shouldBe (1, 1)
      case _ => 
        fail("Parsing failed to produce a Neg.")
    } 
  }

  it should "correctly parse positions for not expressions" in {
    val p = parser.expr.parse("!foo")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case n: Not =>
        n.x match {
          case ident: Ident => 
            ident.id shouldBe "foo" 
            ident.pos shouldBe (1, 2) 
          case _ => fail("Inner expression is not an Ident.")
        } 
        n.pos shouldBe (1, 1) 
      case _ => fail("Parsing failed to produce a Not.")
    } 
  }

  it should "correctly parse positions for parentheses expressions" in {
    val p = parser.expr.parse("(foo)")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case ident: Ident => 
        ident.id shouldBe "foo"
        ident.pos shouldBe (1, 2) 
      case _ => fail("Parsing failed to produce the expected expression.") 
    }
  }

  it should "correctly parse positions for complex  expressions" in {
    val p =   parser.expr.parse("1 + 2 * 3")
    p.isSuccess  shouldBe true
    val node =  p.get
    node match {
      case a: Add =>
        a.x match { 
            case  intL: IntL => intL.n shouldBe 1; intL.pos  shouldBe (1, 1)
            case _ => fail("Unexpected LHS expression in Add.")
        }
        a.y match {
          case m: Mul => 
            m.x match { 
                case intL: IntL => intL.n shouldBe 2; intL.pos shouldBe (1, 5)
                case _ => fail("Unexpected LHS expression in Mul.")
            }
            m.y match { 
                case intL: IntL => intL.n shouldBe 3; intL.pos shouldBe (1, 9)
                case _ => fail("Unexpected RHS expression in Mul.")
            }
          case _ => fail("Unexpected RHS expression in Add.") 
        }
        a.pos shouldBe (1, 3) 
      case _ => fail("Parsing failed to produce an Add.")
    }
  }

  it should "correctly parse positions for nested expressions" in {
    val p = parser.expr.parse("(1 + 2) * 3")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case m: Mul =>
        m.x match { 
          case a: Add =>
            a.x match { 
              case intL: IntL => intL.n shouldBe 1; intL.pos shouldBe (1, 2)
              case _ => fail("Unexpected LHS expression in Add.")
            }
            a.y match { 
              case intL: IntL => intL.n shouldBe 2; intL.pos shouldBe (1, 6) 
              case _ => fail("Unexpected RHS expression in Add.") 
            }
            a.pos shouldBe (1, 4)
          case _ => fail("Unexpected LHS expression in Mul.") 
        }
        m.y match { 
          case intL: IntL => intL.n shouldBe 3; intL.pos shouldBe (1,11) 
          case _ => fail("Unexpected RHS expression in Mul.")
        }
        m.pos shouldBe (1, 9) 
      case _ => fail("Parsing failed to produce a Mul.")
    }
  }

  ignore should "correctly parse positions for assignment statements" in {
    val p = parser.stmt.parse("x = 1")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case a: Asgn =>
        a.lv match { 
          case lIdent: LIdent => lIdent.id shouldBe "x"; lIdent.pos shouldBe (1, 1)
          case _ => fail("Unexpected LValue in assignment.") 
        }
        a.rv match { 
          case r: RExpr => 
            r.x match { 
              case intL: IntL => intL.n shouldBe 1; intL.pos shouldBe (1, 5)
              case _ => fail("Unexpected expression within RExpr.")
            }
          case _ => fail("Unexpected RValue type in assignment.") 
        }
        a.pos shouldBe (1, 3) 
      case _ => fail("Parsing failed to produce an Asgn.")
    } 
  }

  it should "correctly parse positions for array indexing" in {
    val p = parser.lvalue.parse("arr[1]")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case arrElem: LArrElem =>
        arrElem.id shouldBe "arr"
        arrElem.xs match {
          case List(intL: IntL) =>
            intL.n shouldBe 1
            intL.pos shouldBe (1,5)
          case _ => fail("Unexpected expression(s) within array index")
        }
        arrElem.pos shouldBe (1,1)
      case _ => fail("Parsing failed to produce an LArrElem")
    } 
  }

  it should "correctly parse positions for read statements" in {
    val p =  parser.stmt.parse("read x")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case r: Read =>
        r.lv match {
          case lIdent: LIdent =>
            lIdent.id shouldBe "x"
            lIdent.pos shouldBe (1,6)
          case _ => fail("Unexpected LValue type in Read statement.")
        }
        r.pos shouldBe (1,1)
      case _ => fail("Parsing failed to produce a Read.")
    }
  }

  it should "correctly parse positions for function declarations" in {
    val p = parser.func.parse("int func() is begin return 1 end end")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case func: Func =>
        func.tp shouldBe IntT()(1,1)
        func.id shouldBe "func"
        func.params shouldBe List()
        func.s match {
          case body: Body => 
            body.s match {
              case ret: Return =>
                ret.x match {
                  case intL: IntL =>
                    intL.n shouldBe 1
                    intL.pos shouldBe (1,28)
                  case _ => fail("Unexpected expression type within Return.")
                }
                ret.pos shouldBe (1,21)
              case _ => fail("Expected Return statement within Body.")
            }
            body.pos shouldBe (1,15)
          case _ => fail("Unexpected statement type within Func body.")
        }
        func.pos shouldBe (1,1)
      case _ => fail("Parsing failed to produce a Func.")
    }
  }

  ignore should "correctly parse positions for variable declarations" in {
    val p = parser.stmt.parse("int x = 1")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case decl: Decl =>
        decl.tp shouldBe IntT()(1,1).asInstanceOf[Type]
        decl.id shouldBe "x"
        decl.rv match {
          case r: RExpr =>
            r.x match {
              case intL: IntL =>
                intL.n shouldBe 1
                intL.pos shouldBe (1,9)
              case _ => fail("Unexpected expression within RExpr of declaration.")
            }
            r.pos shouldBe (1,9)
          case _ => fail("Expected an RExpr as the RHS of a declaration.")
        }
        decl.pos shouldBe (1,1)
      case _ => fail("Parsing failed to produce a Decl.")
    }
  }

  it should "correctly parse positions for function calls with nested arguments" in {
    val p = parser.rvalue.parse("call foo(1 + 2, 3 * 4)")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case c: Call =>
        c.id shouldBe "foo"
        c.xs should have size 2
        c.xs match {
          case List(add: Add, mul: Mul) =>
            add.x match { 
              case intL: IntL => intL.n shouldBe 1; intL.pos shouldBe (1,10)
              case _ => fail("Unexpected LHS expression within Add argument of Call")
            }
            add.y match { 
              case intL: IntL => intL.n shouldBe 2; intL.pos shouldBe (1,14) 
              case _ => fail("Unexpected RHS expression within Add argument of Call.")
            }
            add.pos shouldBe (1,12)
            mul.x match { 
              case intL: IntL => intL.n shouldBe 3; intL.pos shouldBe (1,17) 
              case _ => fail("Unexpected LHS expression within Mul argument of Call.")
            }
            mul.y match { 
              case intL: IntL => intL.n shouldBe 4; intL.pos shouldBe (1,21) 
              case _ => fail("Unexpected RHS expression within Mul argument of Call.")
            }
            mul.pos shouldBe (1,19) 
          case _ => fail("Unexpected expression(s) within Call arguments.") 
        }
        c.pos shouldBe (1,1)
      case _ => fail("Parsing failed to produce a Call.")
    }
  }

  it should "correctly parse positions for if expressions" in {
    val p = parser.stmt.parse("if true then return 1 else return 2 fi")
    p.isSuccess shouldBe true
    val node = p.get
    node match {
      case cond: Cond => 
        cond.x match {
          case b: BoolL => 
            b.b shouldBe true 
            b.pos shouldBe (1, 4) 
          case _ => fail("Unexpected expression type within Cond.")
        }
        cond.s1 match {
          case ret: Return =>
            ret.x match {
              case intL: IntL => 
                intL.n shouldBe 1 
                intL.pos shouldBe (1, 21) 
              case _ => fail("Unexpected expression type within Return.")
            }
            ret.pos shouldBe (1, 14)
          case _ => fail("Expected Return as 'then' branch of Cond.")
        }
        cond.s2 match {
          case ret: Return => 
            ret.x match {
              case intL: IntL => 
                intL.n shouldBe 2 
                intL.pos shouldBe (1, 35) 
              case _ => fail("Unexpected expression type within Return.")
            }
            ret.pos shouldBe (1, 28)
          case _ => fail("Expected Return as 'then' branch of Cond.")
        }
        cond.pos shouldBe (1, 1)
      case _ => fail("Parsing failed to produce a Cond.")
    }
  }

  it should "correctly parse positions for function calls with multiple arguments" in {
    val p = parser.rvalue.parse("call foo(1, 2, 3)")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case c: Call =>
        c.pos shouldBe (1, 1)
        c.id shouldBe "foo"
        c.xs should have size 3

        c.xs.zipWithIndex.foreach { case (arg, index) =>  
          arg match {
            case intL: IntL =>
              intL.pos shouldBe (1, 10 + index * 3) 
            case _ => fail("Unexpected argument type within Call.")
          }
        }
      case _ => fail("Parsing failed to produce a Call.")
    }
  }

  it should "correctly parse positions for array literals" in {
    val p = parser.arrayLiter.parse("[1, 2, 3]")
    p.isSuccess shouldBe true
    val node = p.get
    node shouldBe ArrL(List(IntL(1)(1,2), IntL(2)(1,5), IntL(3)(1,8)))
  }

  it should "correctly parse positions for pair literals" in {
    val p = parser.rvalue.parse("newpair(1, 2)")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case pair: NewPair =>
        pair.pos shouldBe (1, 1)

        pair.x1 match {
          case intL: IntL => intL.pos shouldBe (1, 9)
          case _ => fail("Unexpected type for the first element of NewPair.")
        }

        pair.x2 match {
          case intL: IntL => intL.pos shouldBe (1, 12)
          case _ => fail("Unexpected type for the second element of NewPair.") 
        }

      case _ => fail("Parsing didn't produce a NewPair object.") 
    }
  }

  it should "correctly parse positions for while-do-done expressions" in {
    val p = parser.stmt.parse("while true do return 1 done")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
       case loop: Loop =>
        loop.pos shouldBe (1, 1)

        loop.x match {
          case boolL: BoolL => boolL.pos shouldBe (1, 7)
          case _ => fail("Unexpected condition type within the while loop.")
        }

        loop.s match {
          case returnStmt: Return =>
            returnStmt.pos shouldBe (1, 15) 
            returnStmt.x match {
               case intL: IntL => intL.pos shouldBe (1, 22) 
               case _ => fail("Unexpected expression type within the Return.")
            }
          case _ => fail("Unexpected statement type within the loop body.")
        } 
      case _ => fail("Parsing didn't produce a Loop object.")
    }
  }


  it should "correctly parse positions for begin-end expressions" in {
    val p = parser.stmt.parse("begin print 1 end")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case body: Body =>
        body.pos shouldBe (1, 1)

        body.s match {
          case printStmt: Print =>
            printStmt.pos shouldBe (1, 7)

            printStmt.x match {
              case intL: IntL => intL.pos shouldBe (1, 13) 
              case _ => fail("Unexpected expression type within Print.")
            }
          case _ => fail("Unexpected statement type within Body.")
        }
      case _ => fail("Parsing didn't produce a Body object.")
    }
  }

  it should "correctly parse positions for function declarations with parameters" in {
    val p = parser.func.parse("int func(int x, int y) is begin return x + y end end")
    p.isSuccess shouldBe true
    val node = p.get
    node shouldBe Func(IntT()(1,1), "func", List(Param(IntT()(1,10), "x")(1,14), 
                  Param(IntT()(1,17), "y")(1,21)), Body(Return(Add(Ident("x")(1,38), 
                  Ident("y")(1,42))(1,41))(1,38))(1,38))(1,5)
  }

  it should "correctly parse positions for function calls with no arguments" in {
    val p = parser.rvalue.parse("call foo()")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case call: Call =>
        call.pos shouldBe (1, 1)
        call.id shouldBe "foo"  

      case _ => fail("Parsing didn't produce a Call object") 
    }
  }

  it should "correctly parse positions for nested array literals" in {
    // val p = parser.arrayLiter.parse("[[1, 2], [3, 4]]")
    // p.isSuccess shouldBe true
    // val node = p.get
    // node shouldBe ArrL(List(ArrL(List(IntL(1)(1,2), IntL(2)(1,5))).asInstanceOf[Expr], 
    //               ArrL(List(IntL(3)(1,10), IntL(4)(1,13))).asInstanceOf[Expr]))
  }

  it should "correctly parse positions for nested pair literals" in {
    // val p = parser.rvalue.parse("newpair(newpair(), newpair())")
    // p.isSuccess shouldBe true
    // val node = p.get
    // node shouldBe NewPair(NewPair()(1,9).asInstanceOf[Expr], NewPair()(1,20).asInstanceOf[Expr])(1,1)
  }

  it should "correctly parse positions for nested while-do-done expressions" in {
    val p = parser.stmt.parse("while true do while false do return 1 done done")
    p. isSuccess shouldBe true
    val node = p.get

    node match {
      case outerLoop: Loop =>
        outerLoop.pos shouldBe (1, 1)

        outerLoop.x match {
          case boolL: BoolL => boolL.pos shouldBe (1, 7) 
          case _ => fail("Unexpected condition type in outer while loop.")
        }

        outerLoop.s match {
          case innerLoop: Loop =>
            innerLoop.pos shouldBe (1, 15)

            innerLoop.x match {
              case falseBool: BoolL => falseBool.pos shouldBe (1, 21)
              case _ => fail("Unexpected condition type within inner loop.")
            }
          case _ => fail("Unexpected statement type within outer loop body.")
        }
      case _ => fail("Failed to parse as a Loop (outer level).")
    }
  }

  it should "correctly parse positions for nested begin-end expressions" in {
    val p = parser.stmt.parse("begin begin print 1 end end")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case outerBody: Body =>
        outerBody.pos shouldBe (1, 1) 

        outerBody.s match { 
          case innerBody: Body => 
            innerBody.pos shouldBe (1, 7) 

            innerBody.s match { 
              case printStmt: Print =>
                printStmt.pos shouldBe (1, 13)

                printStmt.x match {
                  case intl: IntL => intl.pos shouldBe (1, 19)
                  case _ => fail("Unexpected expression within Print.")
                } 

              case _ => fail("Expected a Print statement within the inner Body.")
            }

          case _ => fail("Expected a Body as the statement within the outer Body.")
        }

      case _ => fail("Parsing did not produce a Body as the outermost expression.") 
    }
  }

  it should "correctly parse positions for array literals with single element" in {
    val p = parser.arrayLiter.parse("[1]") 
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case arrL: ArrL =>
        arrL.xs.head match {  
          case intL: IntL => intL.pos shouldBe (1, 2)
          case _ => fail("Unexpected type within the array literal.")
        }
      case _ => fail("Parsing didn't produce an ArrL object.") 
    }
  }

  it should "correctly parse positions for pair literals with expressions" in {
    val p = parser.rvalue.parse("newpair(1+2, 3*4)")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case pair: NewPair =>
        pair.pos shouldBe (1, 1)

        pair.x1 match {
          case add: Add =>
            add.pos shouldBe (1, 10) 
            add.x match {
              case intL: IntL => intL.pos shouldBe (1, 9)
              case _ => fail("Unexpected type for the left operand of Add.")
            }
            add.y match {
              case intL: IntL => intL.pos shouldBe (1, 11)
              case _ => fail("Unexpected type for the right operand of Add.") 
            }
          case _ => fail("Unexpected type for the first element of NewPair.")  
        }

        pair.x2 match {
          case mul: Mul =>
            mul.pos shouldBe (1, 15) 
            mul.x match {
              case intL: IntL => intL.pos shouldBe (1, 14)
              case _ => fail("Unexpected type for the left operand of Mul.")
            }
            mul.y match {
              case intL: IntL => intL.pos shouldBe (1, 16)
              case _ => fail("Unexpected type for the right operand of Mul.")
            }
          case _ => fail("Unexpected type for the second element of NewPair.")  
        }
      case _ => fail("Parsing didn't produce a NewPair object.") 
    }
  }

  it should "correctly parse positions for println statements" in {
    val p = parser.stmt.parse("println 1")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case printStmt: Println =>
        printStmt.pos shouldBe (1, 1)

        printStmt.x match {
          case intL: IntL => intL.pos shouldBe (1, 9)
          case _ => fail("Unexpected type within the Println argument.")
        }
      case _ => fail("Parsing didn't produce a Println statement.") 
    }
  }

  it should "correctly parse positions for free statements" in {
    val p = parser.stmt.parse("free x")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case freeStmt: Free =>
        freeStmt.pos shouldBe (1, 1)

        freeStmt.x match {
          case id: Ident => id.pos shouldBe (1, 6)
          case _ => fail("Unexpected type within Free statement.")
        }
      case _ => fail("Parsing didn't produce a Free statement.") 
    }
  }

  it should "correctly parse positions for fst expressions" in {
    val p = parser.rvalue.parse("fst x")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case fstExpr: First =>
        fstExpr.pos shouldBe (1, 1) 

        fstExpr.lv match { 
          case ident: LIdent => ident.pos shouldBe (1, 5)
          case _ => fail("Unexpected type within First expression argument.")
        }
     case _ => fail("Parsing didn't produce a First.") 
    }
  }

  it should "correctly parse positions for snd expressions" in {
    val p = parser.rvalue.parse("snd x")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case sndExpr: Second =>
        sndExpr.pos shouldBe (1, 1)

        sndExpr.lv match { 
          case ident: LIdent => ident.pos shouldBe (1, 5)
          case _ => fail("Unexpected type within Second expression argument.")
        }
     case _ => fail("Parsing didn't produce a Second.") 
    }
  }

  it should "correctly parse positions for skip statements" in {
    val p = parser.stmt.parse("skip")
    p.isSuccess shouldBe true
    val node = p.get

    node match {
      case skipStmt: Skip =>
        skipStmt.pos shouldBe (1, 1)
      case _ => fail("Parsing didn't produce a Skip statement.") 
    }
  }

  ignore should "correctly parse positions for a multi-dimensional array with single elements" in {
    val p = parser.arrayLiter.parse("[1][1]")
    p.isSuccess shouldBe true 
    val node = p.get
  }

  ignore should "correctly parse positions for a program" in {
    val p = parser.program.parse("begin end")
    p.isSuccess shouldBe true
    val node = p.get
  }

  ignore should "correctly parse positions for a program spanning several lines" in {
    val p = parser.program.parse("begin \n end")
    p.isSuccess shouldBe true
    val node = p.get
  }
}