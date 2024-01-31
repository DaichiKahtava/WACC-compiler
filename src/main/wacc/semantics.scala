package wacc

object semantics {
    def expressionType(e: Expr): Type = {
        return e match {
            case IntL(_) => IntT()
            case BoolL(_) => BoolT()
            case CharL(_) => CharT()
            case StrL(_) => StringT()
            case PairL() => Pair(AnyPet(), AnyPet())
            case UnExpr(op, x) => op match {
                case Not(_) => BoolT()
                case Neg(_) => IntT()
                case Len(_) => IntT()
                case Ord(_) => IntT()
                case Chr(_) => CharT()
            }
            case BinExpr(x1, op, x2) => op match {
                case Mul(_, _) => IntT()
                case Div(_, _) => IntT()
                case Mod(_, _) => IntT()
                case Add(_, _) => IntT()
                case Minus(_, _) => IntT()
                case GrT(_, _) => BoolT()
                case GrEqT(_, _) => BoolT()
                case LsT(_, _) => BoolT()
                case LsEqT(_, _) => BoolT()
                case Eq(_, _) => BoolT()
                case NEq(_, _) => BoolT()
                case And(_, _) => BoolT()
                case Or(_, _) => BoolT()
            }
            case ArrElem(_, List(x))  => expressionType(x)
            case BinExpr(x1, op, x2) => expressionType(x1)
            case BinExpr(x1, op, x2) => expressionType(x2)
        }
    }

    def checkForSemanticsError(e: Expr): Boolean = {
        return e match {
            case IntL(_) => true
            case BoolL(_) => true
            case CharL(_) => true
            case StrL(_) => true
            case PairL() => true
            case UnExpr(op, x) => op match {
                case Not(_) => checkForSemanticsError(x) && expressionType(x) == BoolT()
                case Neg(_) => checkForSemanticsError(x) && expressionType(x) == IntT()
                case Len(_) => checkForSemanticsError(x) && expressionType(x) == ArrayT(AnyT())
                case Ord(_) => checkForSemanticsError(x) && expressionType(x) == CharT()
                case Chr(_) => checkForSemanticsError(x) && expressionType(x) == IntT()
            }
            case BinExpr(x1, op, x2) => op match {
                case Mul(_, _) => (checkForSemanticsError(x1) && expressionType(x1) == IntT()) && (checkForSemanticsError(x2) && expressionType(x2) == IntT())
                case Div(_, _) => (checkForSemanticsError(x1) && expressionType(x1) == IntT()) && (checkForSemanticsError(x2) && expressionType(x2) == IntT())
                case Mod(_, _) => (checkForSemanticsError(x1) && expressionType(x1) == IntT()) && (checkForSemanticsError(x2) && expressionType(x2) == IntT())
                case Add(_, _) => (checkForSemanticsError(x1) && expressionType(x1) == IntT()) && (checkForSemanticsError(x2) && expressionType(x2) == IntT())
                case Minus(_, _) => (checkForSemanticsError(x1) && expressionType(x1) == IntT()) && (checkForSemanticsError(x2) && expressionType(x2) == IntT())
                case GrT(_, _) => (checkForSemanticsError(x1) && (expressionType(x1) == IntT() || expressionType(x1) == CharT())) && (checkForSemanticsError(x2) && (expressionType(x2) == IntT() || expressionType(x2) == CharT()))
                case GrEqT(_, _) => (checkForSemanticsError(x1) && (expressionType(x1) == IntT() || expressionType(x1) == CharT())) && (checkForSemanticsError(x2) && (expressionType(x2) == IntT() || expressionType(x2) == CharT()))
                case LsT(_, _) => (checkForSemanticsError(x1) && (expressionType(x1) == IntT() || expressionType(x1) == CharT())) && (checkForSemanticsError(x2) && (expressionType(x2) == IntT() || expressionType(x2) == CharT()))
                case LsEqT(_, _) => (checkForSemanticsError(x1) && (expressionType(x1) == IntT() || expressionType(x1) == CharT())) && (checkForSemanticsError(x2) && (expressionType(x2) == IntT() || expressionType(x2) == CharT()))
                case Eq(_, _) => (checkForSemanticsError(x1) && expressionType(x1) == AnyT()) && (checkForSemanticsError(x2) && expressionType(x2) == AnyT())
                case NEq(_, _) => (checkForSemanticsError(x1) && expressionType(x1) == AnyT()) && (checkForSemanticsError(x2) && expressionType(x2) == AnyT())
                case And(_, _) => (checkForSemanticsError(x1) && expressionType(x1) == BoolT()) && (checkForSemanticsError(x2) && expressionType(x2) == BoolT())
                case Or(_, _) => (checkForSemanticsError(x1) && expressionType(x1) == BoolT()) && (checkForSemanticsError(x2) && expressionType(x2) == BoolT())
            }
            case ArrElem(_, List(x))  => checkForSemanticsError(x)
            case BinExpr(x1, op, x2) => checkForSemanticsError(x1)
            case BinExpr(x1, op, x2) => checkForSemanticsError(x2)
        }
    }
}
