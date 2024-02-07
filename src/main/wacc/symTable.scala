package wacc

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

// Symbol table class inspired from week 4 Compiler's lectures
class SymTable(parentTable: Option[SymTable]) {
    val symDict = Map.empty[String, TableEntry]

    val childScopes = ListBuffer.empty[SymTable]

    def parent() = parentTable

    // Adding variables
    def addSymbol(id: String, te: TableEntry): Boolean = {
        if (definedLocal(id)) {
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

    def unamedScope(): SymTable = {
        // TODO: Needs to get tested!
        val st = new SymTable(Some(this))
        childScopes.append(st)
        return st;
    }
}

sealed trait TableEntry 

case class VARIABLE(tp: S_TYPE) extends TableEntry
case class FUNCTION(tp: S_TYPE, st: SymTable) extends TableEntry

// Nested scopes may be implemented with multiple symbol tables
// Symbol tables 

//Semantic types
sealed trait S_TYPE
case object S_INT extends S_TYPE
case object S_BOOL extends S_TYPE
case object S_STRING extends S_TYPE
case object S_CHAR extends S_TYPE
case class S_ARRAY(tp: S_TYPE) extends S_TYPE
case class S_PAIR(tp1: S_TYPE, tp2: S_TYPE) extends S_TYPE
case object S_ERASED extends S_TYPE
case object S_ANY extends S_TYPE
