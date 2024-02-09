package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.BeforeAndAfterEach

class semErrorReporterTest  extends AnyFlatSpec with BeforeAndAfterEach {

    "The semantic report tool" should "be able to handle non existing files" in {
        val er = new SemErrorReporter("foo.txt")
        er.printSourceSection((1, 1))
        er.toString() shouldBe """In file: foo.txt at position (1, 1)
*** Could not display the source file: foo.txt ***
-----
Found 0 semantic errors.
"""
    }

    it should "correctly display a sourcefile location" in {
        val er = new SemErrorReporter("compile")
        er.printSourceSection((4, 4))
        er.toString() shouldBe """In file: compile at position (4, 4)
| # You are free to change the language used for this script,
| # but do *not* change its name.
     ^
| 
-----
Found 0 semantic errors.
"""
    }

    it should "correctly display something in the first line" in {
        val er = new SemErrorReporter("compile")
        er.printSourceSection((1, 1))
        er.toString() shouldBe """In file: compile at position (1, 1)
| #!/bin/bash
  ^
| # Bash front-end for your compiler.
-----
Found 0 semantic errors.
"""
    }

    it should "correctly display something in the last line" in {
        val er = new SemErrorReporter("compile")
        er.printSourceSection((9, 1))
        er.toString() shouldBe """In file: compile at position (9, 1)
| 
| exit $?
  ^
-----
Found 0 semantic errors.
"""
    }
}