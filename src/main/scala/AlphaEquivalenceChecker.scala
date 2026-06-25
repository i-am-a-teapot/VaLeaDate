import leo.datastructures.TPTP


object AlphaEquivalenceChecker {
    var renamingMap: Map[String, String] = Map.empty
    var counter: Int = 0
    
    def checkAlphaEquivalence(formula1: TPTP.FOF.Formula, formula2: TPTP.FOF.Formula): Boolean = {
        val renamedFormula1 = renameVariablesFOF(formula1)
        val renamedFormula2 = renameVariablesFOF(formula2)
        Logger.println(s"Renamed formula 1: ${renamedFormula1.pretty}")
        Logger.println(s"Renamed formula 2: ${renamedFormula2.pretty}")
        renamedFormula1 == renamedFormula2
    }

    def renameVariablesFOF(formula: TPTP.FOF.Formula): TPTP.FOF.Formula = {
        renamingMap = Map.empty
        counter = 0
        renameVariablesFOFRec(formula)   
    }
    
    private def renameVariablesFOFTerm(term: TPTP.FOF.Term) : TPTP.FOF.Term = {
        term match {
            case TPTP.FOF.AtomicTerm(f, args) =>
                TPTP.FOF.AtomicTerm(f, args.map(arg => renameVariablesFOFTerm(arg)))
            case TPTP.FOF.Variable(v) =>
                renamingMap.get(v) match {
                    case Some(newName) => TPTP.FOF.Variable(newName)
                    case None => TPTP.FOF.Variable(v)
                }
            case TPTP.FOF.NumberTerm(n) => TPTP.FOF.NumberTerm(n)
            case _ => term
        }
    }

    private def renameVariablesFOFRec(formula: TPTP.FOF.Formula): TPTP.FOF.Formula = {
        formula match {
            case TPTP.FOF.AtomicFormula(pred, args) =>
                TPTP.FOF.AtomicFormula(pred, args.map(arg => renameVariablesFOFTerm(arg)))
            case TPTP.FOF.UnaryFormula(connective, inner) =>
                TPTP.FOF.UnaryFormula(connective, renameVariablesFOFRec(inner))
            case TPTP.FOF.BinaryFormula(connective,left, right) =>
                TPTP.FOF.BinaryFormula(connective, renameVariablesFOFRec(left), renameVariablesFOFRec(right))
            case TPTP.FOF.QuantifiedFormula(quantifier, vars, inner) =>
                var newVars = Set.empty[String]
                val oldRenamingMap = renamingMap
                for(v <- vars){
                    val newName = s"v${counter}"
                    counter += 1
                    renamingMap = renamingMap + (v -> newName)
                    newVars += newName
                }
                val renamedForm = TPTP.FOF.QuantifiedFormula(quantifier, newVars.toSeq, renameVariablesFOFRec(inner))
                renamingMap = oldRenamingMap
                renamedForm
            case TPTP.FOF.Equality(left, right) =>
                TPTP.FOF.Equality(renameVariablesFOFTerm(left), renameVariablesFOFTerm(right))
            case TPTP.FOF.Inequality(left, right) =>
                TPTP.FOF.Inequality(renameVariablesFOFTerm(left), renameVariablesFOFTerm(right))
            case _ => formula
        }   
    }
}
