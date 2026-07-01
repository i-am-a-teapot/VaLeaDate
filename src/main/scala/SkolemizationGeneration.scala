import leo.datastructures.TPTP
import leo.datastructures.TPTP.FOF
import java.io.PrintWriter

object SkolemizationGeneration {

  private def renderSkolemFunctionName(name: String): String = {
    if (name.startsWith("_")) name else "_" + name
  }

  def generateSkolemization(
      writer: PrintWriter,
      stepName: String,
      inputFormula: FOF.Formula,
      variableToSkolemize: String,
      skolemFunctionName: String,
      resultStepName: String,
      outputFormula: FOF.Formula
  ): Unit = {
    val (boundVariables, foundVariable) =
      findBoundVariablesUpToExistential(inputFormula, variableToSkolemize)
    if (foundVariable.isEmpty) {
      throw new ProofErrorException(
        s"Could not find existential variable $variableToSkolemize in formula for step $stepName"
      )
    }
    val previousState = LeanPrettyPrinter.variablesAsTemplates
    LeanPrettyPrinter.variablesAsTemplates = true
    writer.println(s"  conv at step_$stepName =>")
    writer.println(
      s"    pattern " + LeanPrettyPrinter.prettyLeanFOFFormula(
        foundVariable.get
      )
    )
    writer.println(s"    rw [← exists'_eq_exists]")
    writer.println(s"  existspr_prenex at step_$stepName")
    writer.println(s"  simp only [exists'_eq_exists] at step_$stepName")
    writer.println(
      s"  let ⟨${renderSkolemFunctionName(skolemFunctionName)}, step_$resultStepName'⟩ := step_$stepName"
    )
    LeanPrettyPrinter.variablesAsTemplates = previousState
    writer.println(
      s"  have step_$resultStepName : ${LeanPrettyPrinter.prettyLeanFOFFormula(outputFormula)} := by (first | exact step_$resultStepName' | clearExcept step_$resultStepName'; simp_all | clearExcept step_$resultStepName'; grind only)"
    )
  }

  def findBoundVariablesUpToExistential(
      formula: FOF.Formula,
      skolemizedVariable: String
  ): (Set[String], Option[FOF.Formula]) = {
    formula match {
      case TPTP.FOF.AtomicFormula(f, args) => (Set.empty[String], None)
      case TPTP.FOF.QuantifiedFormula(q, variables, body) => {
        if (q == TPTP.FOF.!) {
          val (boundVars, exists) =
            findBoundVariablesUpToExistential(body, skolemizedVariable)
          return (boundVars ++ variables, exists)
        } else {
          if (variables.exists(_ == skolemizedVariable)) {
            return (Set.empty[String], Some(formula))
          } else {
            return findBoundVariablesUpToExistential(body, skolemizedVariable)
          }
        }
      }
      case TPTP.FOF.UnaryFormula(connective, body) =>
        findBoundVariablesUpToExistential(body, skolemizedVariable)
      case TPTP.FOF.BinaryFormula(connective, left, right) => {
        val (leftVars, leftExists) =
          findBoundVariablesUpToExistential(left, skolemizedVariable)
        if (!leftExists.isEmpty) {
          return (leftVars, leftExists)
        } else {
          val (rightVars, rightExists) =
            findBoundVariablesUpToExistential(right, skolemizedVariable)
          if (!rightExists.isEmpty) {
            return (rightVars, rightExists)
          } else {
            return (leftVars ++ rightVars, None)
          }
        }
      }
      case TPTP.FOF.Equality(left, right)   => (Set.empty[String], None)
      case TPTP.FOF.Inequality(left, right) => (Set.empty[String], None)
    }
  }

  def checkSkolemizationDetailsAreConsistent(
      dag: ProofDag.Dag
  ): Map[String, Int] = {
    var skolemFunctionArities = Map.empty[String, Int]
    var skolemFunctionsDefined = Set.empty[String]
    dag.nodes.values
      .filter(node =>
        AnnotationInformationHelpers
          .containsRuleStep("skolemize", node.additionalInfo)
      )
      .foreach(node => {
        Logger.println(s"Checking skolemization details for node ${node.name}")
        val details = AnnotationInformationHelpers
          .getSkolemizationInformation(node.additionalInfo)
        details.newSymbols.foreach { sym =>
          Logger.println(s"Checking skolem function $sym")
          if (
            skolemFunctionsDefined.contains(sym) || skolemFunctionArities
              .contains(sym)
          ) {
            throw new ProofErrorException(
              s"Skolem function $sym is defined multiple times in the proof DAG"
            )
          }
          skolemFunctionsDefined += sym
          details.skolemDefinitions.find(_._2 == sym).foreach {
            case (variable, function, args) =>
              val (boundVariables, _) = findBoundVariablesUpToExistential(
                node.formula.formula.asInstanceOf[TPTP.FOF.Logical].formula,
                variable
              )
              Logger.println(
                s"Skolem function $sym is defined for variable $variable with variables ${args.mkString(", ")} and formula has ${boundVariables.mkString(", ")} as bound variables"
              )
              if (args.size != boundVariables.size) {
                throw new ProofErrorException(
                  s"Skolem function $sym is defined with inconsistent arities in the proof DAG"
                )
              }
              Logger.println(
                s"Adding skolem function $sym with arity ${args.size} to the map of skolem function arities"
              )
              skolemFunctionArities += (sym -> args.size)
          }
        }
      })
    return skolemFunctionArities
  }
}
