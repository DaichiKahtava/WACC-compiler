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
        val symTable2 = new SymTable(Some(symTable), None)
        symTable.addSymbol("gta", VARIABLE(S_INT))
        symTable2.varDefinedGlobal("gta") shouldBe true

    }

    it should "shadow a variable from an enclosing scope" in {
        val symTable2 = new SymTable(Some(symTable), None)
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
        val symTable2 = new SymTable(Some(symTable), None)
        symTable.addSymbol("gta", VARIABLE(S_INT)) shouldBe true
        symTable2.varDefinedLocal("gta") shouldBe false
        symTable2.findVarLocal("gta") shouldBe None
    }

    it should "support redefinition of a symbol" in {
        symTable.addSymbol("gta", VARIABLE(S_CHAR))
        symTable.addSymbol("gta", VARIABLE(S_INT)) shouldBe false
        symTable.redefineSymbol("gta", VARIABLE(S_INT))
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT))
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

    "The position assign functionality" should "assign the correct positions" in {
        symTable.addSymbol("var1", VARIABLE(S_INT))
        symTable.addSymbol("var2", VARIABLE(S_INT))
        symTable.addSymbol("var3", VARIABLE(S_INT))
        symTable.addSymbol("var4", VARIABLE(S_INT))
        symTable.addSymbol("var5", VARIABLE(S_INT))
        symTable.addSymbol("var6", VARIABLE(S_INT))
        symTable.addSymbol("var7", VARIABLE(S_INT))
        symTable.addSymbol("var8", VARIABLE(S_INT))
        symTable.addSymbol("var9", VARIABLE(S_INT))
        symTable.addSymbol("var10", VARIABLE(S_INT))
        symTable.addSymbol("var11", VARIABLE(S_INT))
        symTable.addSymbol("var12", VARIABLE(S_INT))
        symTable.addSymbol("var13", VARIABLE(S_INT))
        symTable.addSymbol("var14", VARIABLE(S_INT))

        symTable.addParam("par1", VARIABLE(S_INT))
        symTable.addParam("par2", VARIABLE(S_INT))
        symTable.addParam("par3", VARIABLE(S_INT))
        symTable.addParam("par4", VARIABLE(S_INT))
        symTable.addParam("par5", VARIABLE(S_INT))
        symTable.addParam("par6", VARIABLE(S_INT))
        symTable.addParam("par7", VARIABLE(S_INT))
        symTable.addParam("par8", VARIABLE(S_INT))
        symTable.addParam("par9", VARIABLE(S_INT))
        symTable.addParam("par10", VARIABLE(S_INT))
        symTable.addParam("par11", VARIABLE(S_INT))
        symTable.addParam("par12", VARIABLE(S_INT))
        symTable.addParam("par13", VARIABLE(S_INT))
        symTable.addParam("par14", VARIABLE(S_INT))

        symTable.assignPositions(new Aarch64_formatter) // shouldBe we will see!

        symTable.findVarGlobal("par1").get.pos shouldBe InRegister(0)
        symTable.findVarGlobal("par2").get.pos shouldBe InRegister(1)
        symTable.findVarGlobal("par3").get.pos shouldBe InRegister(2)
        symTable.findVarGlobal("par4").get.pos shouldBe InRegister(3)
        symTable.findVarGlobal("par5").get.pos shouldBe InRegister(4)
        symTable.findVarGlobal("par6").get.pos shouldBe InRegister(5)
        symTable.findVarGlobal("par7").get.pos shouldBe InRegister(6)
        symTable.findVarGlobal("par8").get.pos shouldBe InRegister(7)
        symTable.findVarGlobal("par9").get.pos shouldBe OnStack(0)
        symTable.findVarGlobal("par10").get.pos shouldBe OnStack(4)
        symTable.findVarGlobal("par11").get.pos shouldBe OnStack(8)
        symTable.findVarGlobal("par12").get.pos shouldBe OnStack(12)
        symTable.findVarGlobal("par13").get.pos shouldBe OnStack(16)
        symTable.findVarGlobal("par14").get.pos shouldBe OnStack(20)

        symTable.findVarGlobal("var1").get.pos shouldBe InRegister(19)
        symTable.findVarGlobal("var2").get.pos shouldBe InRegister(20)
        symTable.findVarGlobal("var3").get.pos shouldBe InRegister(21)
        symTable.findVarGlobal("var4").get.pos shouldBe InRegister(22)
        symTable.findVarGlobal("var5").get.pos shouldBe InRegister(23)
        symTable.findVarGlobal("var6").get.pos shouldBe InRegister(24)
        symTable.findVarGlobal("var7").get.pos shouldBe InRegister(25)
        symTable.findVarGlobal("var8").get.pos shouldBe InRegister(26)
        symTable.findVarGlobal("var9").get.pos shouldBe InRegister(27)
        symTable.findVarGlobal("var10").get.pos shouldBe InRegister(28)
        symTable.findVarGlobal("var11").get.pos shouldBe OnStack(24)
        symTable.findVarGlobal("var12").get.pos shouldBe OnStack(28)
        symTable.findVarGlobal("var13").get.pos shouldBe OnStack(32)
        symTable.findVarGlobal("var14").get.pos shouldBe OnStack(36)
    }
    
    it should "take different type sizes into consideration" in {
        symTable.addSymbol("var1", VARIABLE(S_INT))
        symTable.addSymbol("var2", VARIABLE(S_CHAR))
        symTable.addSymbol("var3", VARIABLE(S_INT))
        symTable.addSymbol("var4", VARIABLE(S_CHAR))
        symTable.addSymbol("var5", VARIABLE(S_INT))
        symTable.addSymbol("var6", VARIABLE(S_CHAR))
        symTable.addSymbol("var7", VARIABLE(S_CHAR))
        symTable.addSymbol("var8", VARIABLE(S_INT))
        symTable.addSymbol("var9", VARIABLE(S_CHAR))
        symTable.addSymbol("var10", VARIABLE(S_INT))
        symTable.addSymbol("var11", VARIABLE(S_CHAR))
        symTable.addSymbol("var12", VARIABLE(S_INT))
        symTable.addSymbol("var13", VARIABLE(S_CHAR))
        symTable.addSymbol("var14", VARIABLE(S_INT))
        
        symTable.addParam("par1", VARIABLE(S_INT))
        symTable.addParam("par2", VARIABLE(S_CHAR))
        symTable.addParam("par3", VARIABLE(S_INT))
        symTable.addParam("par4", VARIABLE(S_CHAR))
        symTable.addParam("par5", VARIABLE(S_INT))
        symTable.addParam("par6", VARIABLE(S_CHAR))
        symTable.addParam("par7", VARIABLE(S_CHAR))
        symTable.addParam("par8", VARIABLE(S_INT))
        symTable.addParam("par9", VARIABLE(S_CHAR))
        symTable.addParam("par10", VARIABLE(S_INT))
        symTable.addParam("par11", VARIABLE(S_CHAR))
        symTable.addParam("par12", VARIABLE(S_INT))
        symTable.addParam("par13", VARIABLE(S_CHAR))
        symTable.addParam("par14", VARIABLE(S_INT))

        symTable.assignPositions(new Aarch64_formatter) // shouldBe we will see!

        symTable.findVarGlobal("par1").get.pos shouldBe InRegister(0)
        symTable.findVarGlobal("par2").get.pos shouldBe InRegister(1)
        symTable.findVarGlobal("par3").get.pos shouldBe InRegister(2)
        symTable.findVarGlobal("par4").get.pos shouldBe InRegister(3)
        symTable.findVarGlobal("par5").get.pos shouldBe InRegister(4)
        symTable.findVarGlobal("par6").get.pos shouldBe InRegister(5)
        symTable.findVarGlobal("par7").get.pos shouldBe InRegister(6)
        symTable.findVarGlobal("par8").get.pos shouldBe InRegister(7)
        symTable.findVarGlobal("par9").get.pos shouldBe OnStack(0)
        symTable.findVarGlobal("par10").get.pos shouldBe OnStack(1)
        symTable.findVarGlobal("par11").get.pos shouldBe OnStack(5)
        symTable.findVarGlobal("par12").get.pos shouldBe OnStack(6)
        symTable.findVarGlobal("par13").get.pos shouldBe OnStack(10)
        symTable.findVarGlobal("par14").get.pos shouldBe OnStack(11)

        symTable.findVarGlobal("var1").get.pos shouldBe InRegister(19)
        symTable.findVarGlobal("var2").get.pos shouldBe InRegister(20)
        symTable.findVarGlobal("var3").get.pos shouldBe InRegister(21)
        symTable.findVarGlobal("var4").get.pos shouldBe InRegister(22)
        symTable.findVarGlobal("var5").get.pos shouldBe InRegister(23)
        symTable.findVarGlobal("var6").get.pos shouldBe InRegister(24)
        symTable.findVarGlobal("var7").get.pos shouldBe InRegister(25)
        symTable.findVarGlobal("var8").get.pos shouldBe InRegister(26)
        symTable.findVarGlobal("var9").get.pos shouldBe InRegister(27)
        symTable.findVarGlobal("var10").get.pos shouldBe InRegister(28)
        symTable.findVarGlobal("var11").get.pos shouldBe OnStack(15)
        symTable.findVarGlobal("var12").get.pos shouldBe OnStack(16)
        symTable.findVarGlobal("var13").get.pos shouldBe OnStack(20)
        symTable.findVarGlobal("var14").get.pos shouldBe OnStack(21)
    }

    "The symbol table in general" should "accomodate a seperate namespace for functions" in {
        symTable.addSymbol("gta", FUNCTION(S_INT)(new SymTable(Some(symTable), None))) shouldBe true
        symTable.addSymbol("gta", VARIABLE(S_INT)) shouldBe true
        symTable.findFunLocal("gta") shouldBe Some(FUNCTION(S_INT)(symTable))
        // The symTable in the second set of brackets is not involved in the comparison
        // So, it is fine to just put a random table hete.
        symTable.findVarLocal("gta") shouldBe Some(VARIABLE(S_INT))
    }

    it should "allow shadowing of parameters" in {
        symTable.addParam("gta", VARIABLE(S_STRING)) shouldBe true
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_STRING))
        symTable.addSymbol("gta", VARIABLE(S_INT)) shouldBe true
        symTable.findVarGlobal("gta") shouldBe Some(VARIABLE(S_INT))
    }



}