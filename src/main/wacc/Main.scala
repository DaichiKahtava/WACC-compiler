package wacc

import parsley.{Success, Failure}

object Main {
    def main(args: Array[String]): Unit = {
        val input = scala.io.Source.fromFile(args(0)).mkString
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
