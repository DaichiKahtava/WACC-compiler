package wacc

import parsley.{Success, Failure}
import java.io.File
import java.io.FileWriter

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
        val sem = new Semantics(args(0)) // [tm1722] Should we change it to Semantics(input) ?
        val tw = new TreeWalker(sem.curSymTable) // Passes the global symbol table to the treeWalker
        

        parseResult match {
            case Failure(_) =>
                println("Parsing failed.")
                System.exit(100)
            case Success(ast) =>
                if (!sem.isSemCorrect(ast)) {
                    println("Semantic check failed.")
                    System.exit(200)
                } else {
                    println("Parsed successfully")
                    // TODO BACKEND LATER.
                    val treeWalker = new TreeWalker(sem.curSymTable)
                    val writer = new FileWriter(inputFile.getName() + ".s")
                    val str = aarch64_formatter.generateAssembly(treeWalker.translate(ast))
                    printf(str)
                    writer.append(str)
                    writer.flush()
                }   
        }
    }
}
