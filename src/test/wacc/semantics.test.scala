package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class semanticsTests extends AnyFlatSpec with BeforeAndAfterEach {

    val sem = new Semantics("foo.txt")

    override protected def beforeEach(): Unit = {
        sem.curSymTable = new SymTable(None)
    }

    "Pair types" should "maintain that their type parameters are invariant" in {
        val p1 = S_PAIR(S_ARRAY(S_CHAR), S_INT)
        val p2 = S_PAIR(S_STRING, S_INT)
        val pAny = S_PAIR(S_ANY, S_ANY)
        sem.canWeakenTo(p1, p2) shouldBe false   
        sem.canWeakenTo(p1, pAny) shouldBe false 
    }
    
    it should "should be fully coercible with erased pair types" in {
        val erasedPair = S_ERASED
        val p1 = S_PAIR(S_STRING, S_INT)
        val p2 = S_PAIR(S_ANY, S_ERASED)
        sem.canWeakenTo(erasedPair, p1) shouldBe true
        sem.canWeakenTo(erasedPair, p1) shouldBe true
        sem.canWeakenTo(erasedPair, p2) shouldBe true
        sem.canWeakenTo(erasedPair, p2) shouldBe true
    }

    "Array types" should "maintain that the type parameters of arrays are invariant" in {
        val arr1 = S_ARRAY(S_ARRAY(S_CHAR))
        val arr2 = S_ARRAY(S_STRING)
        val arr3 = S_ARRAY(S_ANY)
        sem.canWeakenTo(arr1, arr2) shouldBe false
        sem.canWeakenTo(arr2, arr3) shouldBe false
    } 
    
    it should "unidirectionally allow char[] to be weakened to string" in {
        val chArr = S_ARRAY(S_CHAR)
        sem.canWeakenTo(chArr, S_STRING) shouldBe true
        sem.canWeakenTo(S_STRING, chArr) shouldBe false
    }

    // Lots to check for this... probably unnecessary since these are trivial
    "Operators" should "have the correct return types" in {
        sem.isSemCorrect(Not(BoolL(true)(0,0))(0,0)) shouldBe true
        sem.isSemCorrect(Not(IntL(1)(0,0))(0,0)) shouldBe false
        sem.isSemCorrect(Not(CharL('a')(0,0))(0,0)) shouldBe false
    }

    "Read statements" should "accept arguments of either type int or type char" in {
        sem.curSymTable.addSymbol("x", VARIABLE(S_INT)) shouldBe true
        sem.curSymTable.addSymbol("y", VARIABLE(S_CHAR)) shouldBe true
        sem.curSymTable.addSymbol("z", VARIABLE(S_STRING)) shouldBe true
        sem.curSymTable.addSymbol("pair", VARIABLE(S_PAIR(S_CHAR, S_INT))) shouldBe true

        sem.isSemCorrect(Read(LIdent("x")(0, 0))(0, 0)) shouldBe true
        sem.isSemCorrect(Read(LIdent("y")(0, 0))(0, 0)) shouldBe true
        sem.isSemCorrect(Read(LIdent("z")(0, 0))(0, 0)) shouldBe false

        sem.isSemCorrect(Read(LArrElem("x", List(IntL(5)(0, 0)))(0, 0))(0, 0)) shouldBe true
        sem.isSemCorrect(Read(LArrElem("y", List(IntL(5)(0, 0)))(0, 0))(0, 0)) shouldBe true // Not sure about this
        sem.isSemCorrect(Read(LArrElem("z", List(StrL("this is a string")(0, 0)))(0, 0))(0, 0)) shouldBe false

        // Not sure how to do this, will leave for later
        // sem.isSemCorrect(Read(First(LIdent("pair")(0, 0))(0, 0))(0, 0)) shouldBe true
    }

    "Declarations" should "have an rvalue that is compatible with the declaration type" in {
        sem.isSemCorrect(Decl(IntT()(0, 0), "e1", RExpr(IntL(1)(0, 0))(0, 0))(0, 0)) shouldBe true
        sem.isSemCorrect(Decl(IntT()(0, 0), "e2", RExpr(CharL('1')(0, 0))(0, 0))(0, 0)) shouldBe false
        sem.isSemCorrect(Decl(CharT()(0, 0), "e3", RExpr(IntL(1)(0, 0))(0, 0))(0, 0)) shouldBe false

        sem.isSemCorrect(Decl(ArrayT(IntT()(0, 0))(0, 0), "arr1", ArrL(List(IntL(1)(0, 0))))(0, 0)) shouldBe true
        sem.isSemCorrect(Decl(ArrayT(CharT()(0, 0))(0, 0), "arr2", ArrL(List(IntL(1)(0, 0))))(0, 0)) shouldBe false
        sem.isSemCorrect(Decl(IntT()(0, 0), "arr3", ArrL(List(IntL(1)(0, 0))))(0, 0)) shouldBe false

        sem.isSemCorrect(Decl(Pair(IntT()(0, 0), CharT()(0, 0))(0, 0), "p1", NewPair(IntL(1)(0, 0), CharL('a')(0, 0))(0, 0))(0, 0)) shouldBe true
        sem.isSemCorrect(Decl(Pair(IntT()(0, 0), StringT()(0, 0))(0, 0), "p2", NewPair(IntL(1)(0, 0), CharL('a')(0, 0))(0, 0))(0, 0)) shouldBe false
        sem.isSemCorrect(Decl(IntT()(0, 0), "p3", NewPair(IntL(1)(0, 0), CharL('a')(0, 0))(0, 0))(0, 0)) shouldBe false

        sem.curSymTable.addSymbol("x", VARIABLE(S_PAIR(S_INT, S_CHAR)))
        sem.isSemCorrect(Decl(IntT()(0, 0), "pe1", First(LIdent("x")(0, 0))(0, 0))(0, 0)) shouldBe true
        sem.isSemCorrect(Decl(CharT()(0, 0), "pe2", First(LIdent("x")(0, 0))(0, 0))(0, 0)) shouldBe false
        sem.isSemCorrect(Decl(CharT()(0, 0), "pe3", Second(LIdent("x")(0, 0))(0, 0))(0, 0)) shouldBe true
        sem.isSemCorrect(Decl(IntT()(0, 0), "pe4", Second(LIdent("x")(0, 0))(0, 0))(0, 0)) shouldBe false // TODO: array-elem and pair-elem checks

        // TODO: function call checks
    }

    "Assignments" should "have an rvalue that is compatible with its lvalue" in {

    }

    "A function call" should "return a type compatible with the left-hand type of the declaration/assignment" in {

    }

    it should "be provided with all its arguments, and every argument is compatible with the corresponding paramerter" in {
        
    }

    "Return statements" should "not be in the main body of the program" in {

    }

    it should "return a type that is compatible with the return type of the enclosing function" in {

    }



    // TODO test for different namespaces for functoins and variables
}