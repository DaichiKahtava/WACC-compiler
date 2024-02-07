package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class semanticsTests extends AnyFlatSpec with BeforeAndAfterEach {

    val sem = new Semantics("foo.txt")

    var symTable: SymTable = new SymTable(None)

    override protected def beforeEach(): Unit = {
        symTable = new SymTable(None)
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

    "Control flow statements" should "have a condition of type Bool" in {
        
    }

    "Free statements" should "accept arguments of pair or array type" in {

    }

    "Exit statements" should "accept arguments of type int" in {

    }

    "Read statements" should "accept arguments of either type int or type char" in {

    }

    "Return statements" should "not be in the main body of the program" in {

    }

    it should "return a type that is compatible with the return type of the enclosing function" in {

    }



    // TODO test for different namespaces for functoins and variables
}