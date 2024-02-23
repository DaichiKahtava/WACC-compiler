package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class symTableTest extends AnyFlatSpec with BeforeAndAfterEach {

    var symTable: SymTable = new SymTable(None, None)

    override protected def beforeEach(): Unit = {
        symTable = new SymTable(None, None)
    }                                                                                                

    "The variable aspect of the symbol table" should "not have anything inside it at first" in {
        symTable.findVarGlobal("gta") shouldBe None
        symTable.findVarLocal("gta") shouldBe None
    }

    it should "be able to store and return a single variable" in {
        symTable.varDefinedGlobal("gta") shouldBe false
        symTable.addSymbol("gta", VARIABLE(S_INT, Undefined)) shouldBe true
        symTable.varDefinedLocal("gta") shouldBe true
        symTable.varDefinedGlobal("gta") shouldBe true
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_INT, Undefined))
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT, Undefined))
    }

    it should "allow an identifier to be used only once" in {
        symTable.varDefinedGlobal("gta") shouldBe false
        symTable.addSymbol("gta", VARIABLE(S_INT, Undefined)) shouldBe true
        symTable.addSymbol("gta", VARIABLE(S_STRING, Undefined)) shouldBe false
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_INT, Undefined))
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT, Undefined))
    }

    it should "recognise a variable from an enclosing scope" in {
        val symTable2 = new SymTable(Some(symTable), None)
        symTable.addSymbol("gta", VARIABLE(S_INT, Undefined))
        symTable2.varDefinedGlobal("gta") shouldBe true

    }

    it should "shadow a variable from an enclosing scope" in {
        val symTable2 = new SymTable(Some(symTable), None)
        symTable.addSymbol("gta", VARIABLE(S_INT, Undefined))
        symTable2.findVarLocal("gta") shouldBe None
        symTable2.addSymbol("gta", VARIABLE(S_STRING, Undefined)) shouldBe true
        symTable2.findVarGlobal("gta") shouldBe Some(VARIABLE(S_STRING, Undefined))
        symTable2.findVarLocal("gta") shouldBe Some(VARIABLE(S_STRING, Undefined))
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_INT, Undefined))
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT, Undefined))
        //TODO: KEY CONSIDERATION! DOES THAT APPLY TO FUNCTIONS?
    }

    it should "not return variables from enclosing scopes for local methods" in {
        val symTable2 = new SymTable(Some(symTable), None)
        symTable.addSymbol("gta", VARIABLE(S_INT, Undefined)) shouldBe true
        symTable2.varDefinedLocal("gta") shouldBe false
        symTable2.findVarLocal("gta") shouldBe None
    }

    it should "support redefinition of a symbol" in {
        symTable.addSymbol("gta", VARIABLE(S_CHAR, Undefined))
        symTable.addSymbol("gta", VARIABLE(S_INT, Undefined)) shouldBe false
        symTable.redefineSymbol("gta", VARIABLE(S_INT, Undefined))
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT, Undefined))
    }

    "The function aspect of the symbol table" should "not have anything inside it at first" in {
        symTable.findFunGlobal("gta") shouldBe None
        symTable.findFunLocal("gta") shouldBe None
    }

    it should "be able to store and return a single function" in {
        symTable.varDefinedGlobal("gta") shouldBe false
        symTable.addSymbol("gta", FUNCTION(S_INT)(new SymTable(Some(symTable), None))) shouldBe true
        symTable.funDefinedLocal("gta") shouldBe true
        symTable.funDefinedGlobal("gta") shouldBe true
        symTable.findFunGlobal("gta") shouldBe Some(FUNCTION(S_INT)(new SymTable(Some(symTable), None)))
        symTable.findFunLocal("gta") shouldBe Some(FUNCTION(S_INT)(new SymTable(Some(symTable), None)))
    }

    it should "allow an identifier to be used only once" in {
        symTable.varDefinedGlobal("gta") shouldBe false
        symTable.addSymbol("gta", FUNCTION(S_INT)(new SymTable(Some(symTable), None))) shouldBe true
        symTable.addSymbol("gta", FUNCTION(S_STRING)(new SymTable(Some(symTable), None))) shouldBe false
        symTable.findFunGlobal("gta") shouldBe Some(FUNCTION(S_INT)(new SymTable(Some(symTable), None)))
        symTable.findFunLocal("gta") shouldBe Some(FUNCTION(S_INT)(new SymTable(Some(symTable), None)))
    }

    it should "recognise a function from an enclosing scope" in {
        val symTable2 = new SymTable(Some(symTable), None)
        symTable.addSymbol("gta", FUNCTION(S_INT)(new SymTable(Some(symTable), None)))
        symTable2.funDefinedGlobal("gta") shouldBe true

    }

    it should "shadow a funcion from an enclosing scope" in {
        val symTable2 = new SymTable(Some(symTable), None)
        symTable.addSymbol("gta", FUNCTION(S_INT)(new SymTable(Some(symTable), None)))
        symTable2.findFunLocal("gta") shouldBe None
        symTable2.addSymbol("gta", FUNCTION(S_STRING)(new SymTable(Some(symTable), None))) shouldBe true
        symTable2.findFunGlobal("gta") shouldBe Some(FUNCTION(S_STRING)(new SymTable(Some(symTable), None)))
        symTable2.findFunLocal("gta") shouldBe Some(FUNCTION(S_STRING)(new SymTable(Some(symTable), None)))
        symTable.findFunGlobal("gta") shouldBe Some(FUNCTION(S_INT)(new SymTable(Some(symTable), None)))
        symTable.findFunLocal("gta") shouldBe Some(FUNCTION(S_INT)(new SymTable(Some(symTable), None)))
        //TODO: KEY CONSIDERATION! DOES THAT APPLY TO FUNCTIONS?
    }

    it should "not return functions from enclosing scopes for local methods" in {
        val symTable2 = new SymTable(Some(symTable), None)
        symTable.addSymbol("gta", FUNCTION(S_INT)(new SymTable(Some(symTable), None))) shouldBe true
        symTable2.varDefinedLocal("gta") shouldBe false
        symTable2.findFunLocal("gta") shouldBe None
    }

    it should "support redefinition of a symbol" in {
        symTable.addSymbol("gta", FUNCTION(S_CHAR)(symTable))
        symTable.addSymbol("gta", FUNCTION(S_INT)(symTable)) shouldBe false
        symTable.redefineSymbol("gta", FUNCTION(S_INT)(symTable))
        symTable.findFunLocal("gta") shouldBe Some(FUNCTION(S_INT)(symTable))
    }

    "The symbol table in general" should "accomodate a seperate namespace for functions" in {
        symTable.addSymbol("gta", FUNCTION(S_INT)(new SymTable(Some(symTable), None))) shouldBe true
        symTable.addSymbol("gta", VARIABLE(S_INT, Undefined)) shouldBe true
        symTable.findFunLocal("gta") shouldBe Some(FUNCTION(S_INT)(symTable))
        // The symTable in the second set of brackets is not involved in the comparison
        // So, it is fine to just put a random table hete.
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT, Undefined))
    }

    it should "allow shadowing of parameters" in {
        symTable.addParam("gta", VARIABLE(S_STRING, Undefined)) shouldBe true
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_STRING, Undefined))
        symTable.addSymbol("gta", VARIABLE(S_INT, Undefined)) shouldBe true
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_INT, Undefined))
    }



}