package wacc

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.StringBuilder
import scala.io.Source
import scala.util.Using
import scala.util.Try

class SemErrorReporter(fileName: String) {
    val errors = new StringBuilder
    var numErrors = 0
    val fileLines: Try[List[String]] = Try {
        Source.fromFile(fileName).getLines().toList
    }
    
    
    def printSourceSection(pos: (Int, Int)) = {
        // Remember that location is stored begining from 1
        // The list indexing begins from 0

        // If the last line is the empty line (to comply with POSIX standards)
        // It will not be included in the list.
        // PRE: The position will never point to this line
        if (fileLines.isFailure) {
            errors.addAll("*** Could not display the source file: " + fileName + " ***\n")
        } else {
            errors.addAll("In file: " + fileName + " at position (" + pos._1 + ", " + pos._2 + ")\n")
            if (pos._1 != 1) {
                errors.addAll("| " + fileLines.get(pos._1 - 2) + "\n")
            }
            errors.addAll("| " + fileLines.get(pos._1 - 1) + "\n")
            errors.addAll(" " * pos._2 + " ^\n")
            if (pos._1 < fileLines.get.length) {
                errors.addAll("| "+ fileLines.get(pos._1) + "\n")
            }
        }
    }

    def addError(desc: String, pos: (Int, Int)) = {
        errors.addAll(desc + "\n")
        numErrors += 1
        printSourceSection(pos)
    }

    def addTypeMismatch(got: S_TYPE, expected: S_TYPE, pos: (Int, Int)) = {
        addError("Unexpected type! Expected " + explainSemType(expected) +
         ". Got " + explainSemType(got) + "instead.", pos)
    }

    def addTypeMismatch(got: S_TYPE, expected: List[S_TYPE], pos: (Int, Int)) = {
        addError("Unexpected type! Expected one of" +
            expected.map(explainSemType(_)).mkString(" ") +
            ". Got " + explainSemType(got) + "instead.", pos)
    }

    def explainSemType(tp : S_TYPE): String = tp match {
        case S_INT => "int"
        case S_CHAR => "char"
        case S_BOOL => "bool"
        case S_ERASED => "erased pair"
        case S_STRING => "string"
        case S_ARRAY(tp) => "array of (" + explainSemType(tp) + ")" 
        case S_PAIR(tp1, tp2) => "pair of (" + explainSemType(tp1) + ") and (" +
          explainSemType(tp2) + ")"
        case S_ANY => "something of arbitrary type" // Should not be actually invoked...
    }

    override def toString(): String = {
        errors.addAll("-----\nFound " + numErrors + " semantic errors.\n")
        return errors.result()
    }

}
