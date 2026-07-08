import leo.datastructures.TPTP

object AlphaEquivalenceChecker {
  var renamingMap: Map[String, String] = Map.empty
  var counter: Int = 0

  def checkAlphaEquivalence(
      formula1: TPTP.FOF.Formula,
      formula2: TPTP.FOF.Formula,
      functionsNeglectingOrder : Set[String] = Set.empty
  ): Boolean = {
    val renamedFormula1 = renameVariablesFOF(formula1, functionsNeglectingOrder)
    val renamedFormula2 = renameVariablesFOF(formula2, functionsNeglectingOrder)
    Logger.println(s"Renamed formula 1: ${renamedFormula1.pretty}")
    Logger.println(s"Renamed formula 2: ${renamedFormula2.pretty}")
    renamedFormula1 == renamedFormula2
  }

  def renameVariablesFOF(formula: TPTP.FOF.Formula, functionsNeglectingOrder: Set[String] = Set.empty): TPTP.FOF.Formula = {
    renamingMap = Map.empty
    counter = 0
    renameVariablesFOFRec(formula, functionsNeglectingOrder)
  }

  private def renameVariablesFOFTerm(term: TPTP.FOF.Term, functionsNeglectingOrder: Set[String] = Set.empty): TPTP.FOF.Term = {
    term match {
      case TPTP.FOF.AtomicTerm(f, args) =>
        var res = args.map(arg => renameVariablesFOFTerm(arg, functionsNeglectingOrder))
        if(functionsNeglectingOrder.contains(f)) {
          res = res.sortBy(_.pretty)
          Logger.println(s"Sorting arguments of function $f for alpha equivalence check")
          Logger.println(s"Sorted arguments: ${res.map(_.pretty).mkString(", ")}")
        }
        TPTP.FOF.AtomicTerm(f, res)
      case TPTP.FOF.Variable(v) =>
        renamingMap.get(v) match {
          case Some(newName) => TPTP.FOF.Variable(newName)
          case None          => TPTP.FOF.Variable(v)
        }
      case TPTP.FOF.NumberTerm(n) => TPTP.FOF.NumberTerm(n)
      case _                      => term
    }
  }

  private def renameVariablesFOFRec(
      formula: TPTP.FOF.Formula,
      functionsNeglectingOrder: Set[String] = Set.empty
  ): TPTP.FOF.Formula = {
    formula match {
      case TPTP.FOF.AtomicFormula(pred, args) =>
        TPTP.FOF.AtomicFormula(
          pred,
          args.map(arg => renameVariablesFOFTerm(arg, functionsNeglectingOrder))
        )
      case TPTP.FOF.UnaryFormula(connective, inner) =>
        TPTP.FOF.UnaryFormula(connective, renameVariablesFOFRec(inner, functionsNeglectingOrder))
      case TPTP.FOF.BinaryFormula(connective, left, right) =>
        TPTP.FOF.BinaryFormula(
          connective,
          renameVariablesFOFRec(left, functionsNeglectingOrder),
          renameVariablesFOFRec(right, functionsNeglectingOrder)
        )
      case TPTP.FOF.QuantifiedFormula(quantifier, vars, inner) =>
        var newVars = Set.empty[String]
        val oldRenamingMap = renamingMap
        for (v <- vars) {
          val newName = s"v${counter}"
          counter += 1
          renamingMap = renamingMap + (v -> newName)
          newVars += newName
        }
        val renamedForm = TPTP.FOF.QuantifiedFormula(
          quantifier,
          newVars.toSeq,
          renameVariablesFOFRec(inner, functionsNeglectingOrder)
        )
        renamingMap = oldRenamingMap
        renamedForm
      case TPTP.FOF.Equality(left, right) =>
        TPTP.FOF.Equality(
          renameVariablesFOFTerm(left, functionsNeglectingOrder),
          renameVariablesFOFTerm(right, functionsNeglectingOrder)
        )
      case TPTP.FOF.Inequality(left, right) =>
        TPTP.FOF.Inequality(
          renameVariablesFOFTerm(left, functionsNeglectingOrder),
          renameVariablesFOFTerm(right, functionsNeglectingOrder)
        )
      case _ => formula
    }
  }
}
