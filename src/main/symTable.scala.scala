package wacc

import scala.collection.Map

// Symbol table class inspired from week 4 Compiler's lectures
class SymTable {
    val symDict = Map.empty[String, TableEntry]

}

sealed trait TableEntry 

case class VARIABLE(tp: TYPE)
case class PARAM(tp: TYPE)
case class FUNCTION(tp: TYPE, PARAM: Array[VARIABLE], st: SymTable)

sealed trait TYPE
case class INT() extends TYPE
case class BOOL() extends TYPE
case class STRING() extends TYPE
case class CHAR() extends TYPE
case class ARRAY(tp: TYPE, size: BigInt) extends TYPE
case class PAIR(tp1: TYPE, tp2: TYPE) extends TYPE


