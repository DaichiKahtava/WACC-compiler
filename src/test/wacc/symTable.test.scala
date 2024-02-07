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
        symTable.findVarGlobal("gta") shouldBe None
        symTable.findVarLocal("gta") shouldBe None
    }

    it should "be able to store and return a single variable" in {
        symTable.varDefinedGlobal("gta") shouldBe false
        symTable.addSymbol("gta", VARIABLE(S_INT)) shouldBe true
        symTable.varDefinedLocal("gta") shouldBe true
        symTable.varDefinedGlobal("gta") shouldBe true
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_INT))
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT))
    }

    it should "allow an identifier to be used only once" in {
        symTable.varDefinedGlobal("gta") shouldBe false
        symTable.addSymbol("gta", VARIABLE(S_INT)) shouldBe true
        symTable.addSymbol("gta", VARIABLE(S_STRING)) shouldBe false
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_INT))
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT))
    }

    it should "recognise a variable from an enclosing scope" in {
        val symTable2 = new SymTable(Some(symTable))
        symTable.addSymbol("gta", VARIABLE(S_INT))
        symTable2.varDefinedGlobal("gta") shouldBe true

    }

    it should "shadow a variable from an enclosing scope" in {
        val symTable2 = new SymTable(Some(symTable))
        symTable.addSymbol("gta", VARIABLE(S_INT))
        symTable2.findVarLocal("gta") shouldBe None
        symTable2.addSymbol("gta", VARIABLE(S_STRING)) shouldBe true
        symTable2.findVarGlobal("gta") shouldBe Some(VARIABLE(S_STRING))
        symTable2.findVarLocal("gta") shouldBe Some(VARIABLE(S_STRING))
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_INT))
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT))
        //TODO: KEY CONSIDERATION! DOES THAT APPLY TO FUNCTIONS?
    }

    it should "not return variables from enclosing scopes for local methods" in {
        val symTable2 = new SymTable(Some(symTable))
        symTable.addSymbol("gta", VARIABLE(S_INT)) shouldBe true
        symTable2.varDefinedLocal("gta") shouldBe false
        symTable2.findVarLocal("gta") shouldBe None

    }

}