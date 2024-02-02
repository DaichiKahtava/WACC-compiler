package wacc

import parsley.{Success, Failure}
import scala.util.Random

object Main {
    val random = new Random

    def main(args: Array[String]): Unit = {
    //     println("hello WACC!")
        
    //     if (args.length < 1) {
    //         println("Please provide a file path as an argument")
    //         return
    //     }

    //     val source = scala.io.Source.fromFile(args(0))
    //     val lines = try source.mkString finally source.close()
    //     lines.headOption match {
    //         case Some(expr) => parser.parse(expr.toString()) match {
    //             case Success(x) => println(s"$expr = $x")
    //             case Failure(msg) => println(msg)
    //         }
    //         case None => println("File is empty")
    //     }
        val exitSelector = random.nextFloat();
        if (exitSelector <= 0.3) {
            sys.exit(0)
        } else if (exitSelector <= 0.6) {
            sys.exit(100)
        } else {
            sys.exit(200)
        }
    }
}
