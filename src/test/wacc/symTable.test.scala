package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class symTableTest extends AnyFlatSpec with BeforeAndAfterEach {

    var symTable: SymTable = new SymTable(None)

    override protected def beforeEach(): Unit = {
        symTable = new SymTable(None)
    }    

    "The symbol table" should "not have anything inside it at first" in {
        symTable.findGlobal("gta") shouldBe None
        symTable.findLocal("gta") shouldBe None
    }

    it should "be able to store and return a single variable" in {
        symTable.definedGlobal("gta") shouldBe false
        symTable.addVariable("gta", VARIABLE(S_INT)) shouldBe true
        symTable.definedLocal("gta") shouldBe true
        symTable.definedGlobal("gta") shouldBe true
        symTable.findGlobal("gta") shouldBe Some(VARIABLE(S_INT))
        symTable.findLocal("gta") shouldBe Some(VARIABLE(S_INT))
    }

    it should "allow an identifier to be used only once" in {
        symTable.definedGlobal("gta") shouldBe false
        symTable.addVariable("gta", VARIABLE(S_INT)) shouldBe true
        symTable.addVariable("gta", VARIABLE(S_STRING)) shouldBe false
        symTable.findGlobal("gta") shouldBe Some(VARIABLE(S_INT))
        symTable.findLocal("gta") shouldBe Some(VARIABLE(S_INT))
    }

    it should "recognise a variable from an enclosing scope" in {
        val symTable2 = new SymTable(Some(symTable))
        symTable.addVariable("gta", VARIABLE(S_INT)) shouldBe true
        symTable2.definedGlobal("gta") shouldBe true
        symTable2.addVariable("gta", VARIABLE(S_STRING)) shouldBe false
        symTable2.findGlobal("gta") shouldBe Some(VARIABLE(S_INT))
    }

    it should "not return variables from enclosing scopes for local methods" in {
        val symTable2 = new SymTable(Some(symTable))
        symTable.addVariable("gta", VARIABLE(S_INT)) shouldBe true
        symTable2.definedLocal("gta") shouldBe false
        symTable2.findLocal("gta") shouldBe None

    }

}