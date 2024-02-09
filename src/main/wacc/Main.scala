package wacc

import parsley.{Success, Failure}
import java.io.File

object Main {
    def main(args: Array[String]): Unit = {
        if (args.length != 1) {
            println("Input is null.")
            System.exit(300)
        }

        val inputFile = new File(args(0))
        if (!inputFile.exists() || !inputFile.isFile) {
            println("Input file does not exist.")
            System.exit(400)
        }

        val input = scala.io.Source.fromFile(inputFile).mkString
        val parseResult = parser.parse(input)

        parseResult match {
            case Failure(_) =>
                println("Parsing failed.")
                System.exit(100)
            case Success(ast) =>
                if (!isSemCorrect(ast)) {
                    println("Semantic check failed.")
                    System.exit(200)
                } else {
                    println("Parsed successfully")
                    // TODO BACKEND LATER.
                }
        }
    }

    def isSemCorrect(ast: Any): Boolean = {
        true // TODO.
    }
    
}
