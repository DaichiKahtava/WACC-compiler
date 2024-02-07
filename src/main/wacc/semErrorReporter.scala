package wacc

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.StringBuilder
import scala.io.Source
import scala.util.Using
import scala.util.Try

class SemErrorReporter(fileName: String) {
    val errors = new StringBuilder
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
        printSourceSection(pos)
    }

    override def toString(): String = {
        return errors.result()
    }

}
