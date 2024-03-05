package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach
import java.io.File

class aarch64FormTest extends AnyFlatSpec with BeforeAndAfterEach {

    var frm = new Aarch64_formatter()
    val tempFile = File.createTempFile("test", ".s") // Create a temporary file for testing.
    tempFile.deleteOnExit() // Ensure the file is deleted when the JVM exits.

    override protected def beforeEach(): Unit = {
        frm = new Aarch64_formatter()
    }

    "The arm64 formatter" should "process a simple mov command with registers" in {
        frm.generateAssembly(List(Move(RegisterX(1), RegisterX(2))), tempFile.getAbsolutePath)
        // Read the content of the file and turn it into a string, done similarly for all tests below.
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nmov\tX2, X1\n"
    }

    it should "process a simple mov with an immediate" in {
        frm.generateAssembly(List(Move(ImmNum(1), RegisterX(2))), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nmov\tX2, #1\n"
    }

    it should "process an add" in {
        frm.generateAssembly(List(AddI(RegisterX(1), RegisterX(2))), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nadds\tX2, X2, X1\n"
    }

    it should "process a list of multiple instructions" in {
        val instructions = List(
            Move(RegisterX(1), RegisterX(2)),
            AddI(RegisterX(1), RegisterX(2)),
            SubI(RegisterX(1), RegisterX(2))
        )

        frm.generateAssembly(instructions, tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        val expected = ".align 4\n.text\n.global main\nmov\tX2, X1\nadds\tX2, X2, X1\nsubs\tX2, X2, X1\n"
        
        result shouldBe expected
    }

    it should "process a sub instruction" in {
        frm.generateAssembly(List(SubI(RegisterX(1), RegisterX(2))), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nsubs\tX2, X2, X1\n"
    }

    it should "process a Load instruction" in {
        frm.generateAssembly(List(Load(BaseA(RegisterX(1)), RegisterX(2))), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nldr\tX2, [X1]\n"
    }

    it should "process a LoadByte instruction" in {
        frm.generateAssembly(List(LoadByte(BaseA(RegisterX(1)), RegisterX(2), false)), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nldrb\tX2, [X1]\n"
    }

    it should "process a return instruction" in {
        frm.generateAssembly(List(ReturnI), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nret\n"
    }

    it should "process a BranchCond instruction with EqI condition" in {
        frm.generateAssembly(List(BranchCond("label", EqI)), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nb.eq\tlabel\n"
    }

    it should "process a BranchCond instruction with NeI condition" in {
        frm.generateAssembly(List(BranchCond("label", NeI)), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nb.ne\tlabel\n"
    }

    it should "process a BranchCond instruction with CsI condition" in {
        frm.generateAssembly(List(BranchCond("label", CsI)), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nb.cs\tlabel\n"
    }

    it should "process a Store instruction" in {
        frm.generateAssembly(List(Store(RegisterX(1), BaseA(RegisterX(2)))), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nstr\tX1, [X2]\n"
    }

    it should "process a StoreByte instruction" in {
        frm.generateAssembly(List(StoreByte(RegisterX(1), BaseA(RegisterX(2)))), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nstrb\tX1, [X2]\n"
    }

    it should "process a BranchLink instruction" in {
        frm.generateAssembly(List(BranchLink("label")), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\nbl\tlabel\n"
    }

    it should "process a Compare instruction" in {
        frm.generateAssembly(List(Compare(RegisterX(1), RegisterX(2))), tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        result shouldBe ".align 4\n.text\n.global main\ncmp\tX1, X2\n"
    }

    it should "process a sequence of instructions with dependencies" in {
        val instructions = List(
            Move(ImmNum(10), RegisterX(1)),
            AddI(RegisterX(1), RegisterX(2)),
            SubI(RegisterX(5), RegisterX(2)),
            Store(RegisterX(2), BaseA(RegisterX(3)))
        )

        frm.generateAssembly(instructions, tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        val expected = ".align 4\n.text\n.global main\nmov\tX1, #10\nadds\tX2, X2, X1\nsubs\tX2, X2, X5\nstr\tX2, [X3]\n"
        
        result shouldBe expected
    }

    it should "process a sequence of instructions with branches and conditions" in {
        val instructions = List(
            Compare(RegisterX(1), RegisterX(0)),
            BranchCond("zero", EqI),
            Move(ImmNum(1), RegisterX(1)),
            Label("zero"),
            Move(ImmNum(0), RegisterX(1))
        )

        frm.generateAssembly(instructions, tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
    }

    it should "process a sequence of instructions with function calls" in {
        val instructions = List(
            BranchLink("function"),
            Move(RegisterXR, RegisterX(1)),
            ReturnI
        )

        frm.generateAssembly(instructions, tempFile.getAbsolutePath)
        val result = scala.io.Source.fromFile(tempFile).mkString
        val expected = ".align 4\n.text\n.global main\nbl\tfunction\nmov\tX1, X8\nret\n"
        
        result shouldBe expected
    }

    // No longer part of the formatter
//     it should "add error checking for division" in {
//         frm.generateAssembly(List(DivI(ImmNum(0), RegisterX(2)))) shouldBe 
//         ".align 4\n.text\n.global main\ncmp\tXZR, #0\nb.eq\t_errDivZero\nsdiv\tX2, X2, #0\n" +
// """// Division by zero error handler as seen in the ref. compiler
// 	.word 40
// .L._errDivZero_str0:
// 	.asciz "fatal error: division or modulo by zero\n"
// .align 4
// _errDivZero:
// adrp	X0, .L._errDivZero_str0
// add	X0, X0, :lo12:.L._errDivZero_str0
// bl	_prints
// mov	W0, #-1
// bl	exit
// """
//     } 

}
