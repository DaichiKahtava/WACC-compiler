package wacc

import parsley.{Success, Failure}

object Main {
    def main(args: Array[String]): Unit = {
        val input = scala.io.Source.fromFile(args(0)).mkString
        val parseResult = parser.parse(input)
        val sem = new Semantics(input)

        parseResult match {
            case Failure(_) =>
                println("Parsing failed.")
                System.exit(100)
            case Success(ast) =>
                if (!sem.isSemCorrect(ast)) {
                    println("Semantic check failed.")
                    System.exit(200)
                } else {
                    // TODO BACKEND LATER.
                }
        }
    }
}
