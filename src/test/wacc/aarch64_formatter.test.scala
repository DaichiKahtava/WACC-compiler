package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class aarch64FormTest extends AnyFlatSpec with BeforeAndAfterEach {

    var frm = new Aarch64_formatter()

    override protected def beforeEach(): Unit = {
        frm = new Aarch64_formatter()
    }

    "The arm64 formatter" should "process a simple mov command with registers" in {
        frm.generateAssembly(List(Move(RegisterX(1), RegisterX(2)))) shouldBe ".align 4\n.text\n.global main\nmov\tX2, X1\n"
    }

    it should "process a simple mov with an immediate" in {
        frm.generateAssembly(List(Move(ImmNum(1), RegisterX(2)))) shouldBe ".align 4\n.text\n.global main\nmov\tX2, #1\n"
    }

    it should "process an add" in {
        frm.generateAssembly(List(AddI(ImmNum(1), RegisterX(2)))) shouldBe ".align 4\n.text\n.global main\nadd\tX2, X2, #1\n"
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
