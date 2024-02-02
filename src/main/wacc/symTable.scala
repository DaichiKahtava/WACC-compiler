package wacc

import scala.collection.mutable.Map

// Symbol table class inspired from week 4 Compiler's lectures
class SymTable(parentTable: Option[SymTable]) {
    val symDict = Map.empty[String, TableEntry]


    // Adding variables
    def addVariable(id: String, te: TableEntry): Boolean = {
        if (definedGlobal(id)) {
            return false
        }
        else {
            symDict.addOne(id, te)
            return true
        }
    }

    // Finding variables
    def findLocal(id: String): Option[TableEntry] = {
        return symDict.get(id)
    }

    def findGlobal(id: String): Option[TableEntry] = {
        return symDict.get(id) match {
            case None => {
                if (parentTable != None) {parentTable.get.findGlobal(id)} else {None}
            }
            case te@Some(_) => te
        }
    }

    def definedGlobal(id: String): Boolean = {
        return findGlobal(id) != None
    }

    def definedLocal(id: String): Boolean = {
        return findLocal(id) != None
    }
}

sealed trait TableEntry 

case class VARIABLE(tp: TYPE) extends TableEntry
case class PARAM(tp: TYPE) extends TableEntry
case class FUNCTION(tp: TYPE, PARAM: Array[VARIABLE], st: SymTable) extends TableEntry

// Nested scopes may be implemented with multiple symbol tables
// Symbol tables 

sealed trait TYPE
case class INT() extends TYPE
case class BOOL() extends TYPE
case class STRING() extends TYPE
case class CHAR() extends TYPE
case class ARRAY(tp: TYPE, size: BigInt) extends TYPE
case class PAIR(tp1: TYPE, tp2: TYPE) extends TYPE
case class ANY() extends TYPE
