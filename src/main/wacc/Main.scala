package wacc

import parsley.{Success, Failure}
import java.io.File
import java.io.FileWriter
import org.apache.commons.io.FilenameUtils

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
                        // println(ast)
                        // Passes the global symbol table and semantic data to the treeWalker
                        val frm = new Aarch64_formatter()
                        val tw = new TreeWalker(sem, frm)
                        val instructions = tw.translate(ast)

                        // Generate the filename for the output file.
                        val filename = FilenameUtils.getBaseName(inputFile.getName()) + ".s"
                        
                        // Call generateAssembly with the list of instructions and the filename.
                        frm.generateAssembly(instructions, filename)

                        // val writer = new FileWriter(FilenameUtils.getBaseName(inputFile.getName()) + ".s")
                        // val str = frm.generateAssembly(tw.translate(ast))
                        // //print(str)
                        // writer.append(str)
                        // writer.flush()
                }   
        }
    }
}
