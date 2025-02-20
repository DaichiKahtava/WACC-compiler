package wacc

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap

// Symbol table class inspired from week 4 Compiler's lectures
class SymTable(parentTable: Option[SymTable], returnType: Option[S_TYPE], val anonymous: Boolean) {
    // LinkedHashMap used to preserve insersion order
    val varDict = LinkedHashMap.empty[String, VARIABLE] 
    val parDict = LinkedHashMap.empty[String, VARIABLE] 
    val funDict = Map.empty[String, FUNCTION]
    val childScopes = ListBuffer.empty[SymTable]

    var childScopesCounter = -1

    var stackAllocVars: Int = 0  

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

    def addParam(id: String, te: VARIABLE): Boolean = {
        if (parDefinedLocal(id)) {
            return false
        }
        else {
            parDict.addOne(id, te)
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
                parDict.get(id) match {
                    case te@Some(_) => te
                    case None => if (parentTable != None) {parentTable.get.findVarGlobal(id)} else {None}
                }
                
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

    def parDefinedLocal(id: String): Boolean = {
        return parDict.contains(id)
    }

    def funDefinedGlobal(id: String): Boolean = {
        return findFunGlobal(id) != None
    }

    def funDefinedLocal(id: String): Boolean = {
        return findFunLocal(id) != None
    }

    def newUnamedScope(): SymTable = {
        // TODO: Needs to get tested!
        val st = new SymTable(Some(this), returnType, true)
        childScopes.append(st)
        return st;
    }

    def getReturnType(): Option[S_TYPE] = {
        return returnType
    }

    def assignPositions(formatter: Aarch64_formatter): Int = {
        val itemsize = formatter.regConf.stackAlign.max(formatter.getSize(S_ANY))
        return assignPositionsFrom(formatter, 0, 0, itemsize, -itemsize, itemsize)
    }

    // Assigns the location where each variable and parameter is stored and 
    // returns the offset that needs to be applied for the stack
    def assignPositionsFrom(formatter: Aarch64_formatter,
        argRegsIndx: Int,
        varRegIndx: Int,
        argOffset: Int, // starting from 0 (the last pushed element from parent funciton)
        varOffset: Int, // starting from the first empty stack point.
        itemsize: Int): Int = {
            var argRegsIndx_ = argRegsIndx
            var varRegIndx_ = varRegIndx
            var argOffset_ = argOffset
            var varOffset_ = varOffset
            parDict.foreach(e => {
                if (argRegsIndx_ < formatter.regConf.argRegs.length) {
                    // Parameters asigned to positions R0-R7
                    e._2.pos = InRegister(formatter.regConf.argRegs(argRegsIndx_))
                    argRegsIndx_ += 1
                } else {
                    // Parameters assigned to stack
                    e._2.pos = OnStack(argOffset_)
                    argOffset_ += itemsize
                }
            })
            varDict.foreach(e => {
                if (varRegIndx_ < formatter.regConf.variabRegs.length) {
                    // Variables assigned to positions R19-R28
                    e._2.pos = InRegister(formatter.regConf.variabRegs(varRegIndx_))
                    varRegIndx_ += 1
                } else {
                    // Variables assigned to stack  
                    e._2.pos = OnStack(varOffset_)
                    varOffset_ -= itemsize
                }
            })

            if (childScopes.isEmpty) {
                stackAllocVars = varOffset_
                return varOffset_
            } else {
                stackAllocVars = childScopes.map((st) => 
                    st.assignPositionsFrom(formatter, argRegsIndx_, varRegIndx_, argOffset_, varOffset_, itemsize)
                ).max
                // Propagate the number of variables to the anonymous children
                // This is for situation such as returning from an if statment
                childScopes.foreach((st) => st.stackAllocVars = stackAllocVars)
                return stackAllocVars
            }
    }

    def getNextChildSymbolTable() = {
        // Invariant:
        // The function will be called every time an anonymous scope is
        // cound in the treeWalker. By the semantic checks, the number of invocations
        // chould be exactly equals to the number of anonymous scopes.     
        childScopesCounter += 1
        childScopes(childScopesCounter)
    }
}
 

case class VARIABLE(tp: S_TYPE) {
    var pos: Position = Undefined
}
case class FUNCTION(tp: S_TYPE)(val st: SymTable)

sealed trait Position
case object Undefined extends Position
case class InRegister(r: Int) extends Position
case class OnStack(offset: Int) extends Position // Offset from the frame pointer

case class OnTempStack(regNum: Int) extends Position 
// Temporary location for caller saved argument
// The offset is calculated from the registerNum using the pinterReg (X16)

// Note that OnStack uses the size of the data
// while OnTempStack always uses the register size. 

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
