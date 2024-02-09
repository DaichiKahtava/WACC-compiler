package wacc

import parsley.{Success, Failure}

object Main {
    def main(args: Array[String]): Unit = {
        if (args.length != 1) {
            println("Input is null.")
            System.exit(300)
        }        
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
                    // TODO BACKEND LATER.
                }
        }
    }

    def isSemCorrect(ast: Any): Boolean = {
      true // TODO.
    }
    
}
