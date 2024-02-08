package wacc

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

// Symbol table class inspired from week 4 Compiler's lectures
class SymTable(parentTable: Option[SymTable]) {
    val varDict = Map.empty[String, VARIABLE]
    val funDict = Map.empty[String, FUNCTION]
    val childScopes = ListBuffer.empty[SymTable]

    def parent() = parentTable

    // Adding variables and functions
    def addSymbol(id: String, te: VARIABLE): Boolean = {
        if (varDefinedLocal(id)) {
            return false
        }
        else {
            varDict.addOne(id, te)
            return true
        }
    }

    def addSymbol(id: String, te: FUNCTION): Boolean = {
        if (funDefinedLocal(id)) {
            return false
        }
        else {
            funDict.addOne(id, te)
            return true
        }
    }

    // Redefining variables or functions if needed
    def redefineSymbol(id: String, te: VARIABLE): Unit = {
        varDict(id) = te
    }

    def redefineSymbol(id: String, te: FUNCTION): Unit = {
        funDict(id) = te
    }

    // Finding variables
    
    def findVarLocal(id: String): Option[VARIABLE] = {
        return varDict.get(id)
    }

    def findVarGlobal(id: String): Option[VARIABLE] = {
        return varDict.get(id) match {
            case None => {
                if (parentTable != None) {parentTable.get.findVarGlobal(id)} else {None}
            }
            case te@Some(_) => te
        }
    }

    def findFunLocal(id: String): Option[FUNCTION] = {
        return funDict.get(id)
    }

    def findFunGlobal(id: String): Option[FUNCTION] = {
        return funDict.get(id) match {
            case None => {
                if (parentTable != None) {parentTable.get.findFunGlobal(id)} else {None}
            }
            case te@Some(_) => te
        }
    }

    // Defined functions
    def varDefinedGlobal(id: String): Boolean = {
        return findVarGlobal(id) != None
    }

    def varDefinedLocal(id: String): Boolean = {
        return findVarLocal(id) != None
    }

    def funDefinedGlobal(id: String): Boolean = {
        return findFunGlobal(id) != None
    }

    def funDefinedLocal(id: String): Boolean = {
        return findFunLocal(id) != None
    }

    def newUnamedScope(): SymTable = {
        // TODO: Needs to get tested!
        val st = new SymTable(Some(this))
        childScopes.append(st)
        return st;
    }
}
 

case class VARIABLE(tp: S_TYPE)
case class FUNCTION(tp: S_TYPE)(val st: SymTable)

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
case object S_EMPTYARR extends S_TYPE
