import leo.datastructures.TPTP
import leo.datastructures.TPTP.FOF
import java.io.PrintWriter
import scala.collection.mutable.LinkedHashSet

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
      skolemFunctionArguments: Seq[String],
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

    var switchedOrders = false
    if (skolemFunctionArguments != boundVariables.toSeq) {
      //check if they are the same set
      if (skolemFunctionArguments.toSet != boundVariables.toSet) {
        throw new ProofErrorException(
          s"Skolem function arguments ${skolemFunctionArguments.mkString(", ")} are not the same as the bound variables ${boundVariables.mkString(", ")} for step $stepName"
        )
      } else {
        switchedOrders = true
      }
    }
    if(!switchedOrders){
      writer.println(
        s"  let ⟨${renderSkolemFunctionName(skolemFunctionName)}, step_$resultStepName'⟩ := step_$stepName"
      )
    } else {
      writer.println(
        s"  let ⟨${renderSkolemFunctionName(skolemFunctionName)}', step_$resultStepName'⟩ := step_$stepName"
      )
      writer.println(
        s"  let ${renderSkolemFunctionName(skolemFunctionName)} := fun ${skolemFunctionArguments.mkString(" ")} => ${renderSkolemFunctionName(skolemFunctionName)}' ${boundVariables.mkString(" ")}"
      )
    }
    LeanPrettyPrinter.variablesAsTemplates = previousState
    writer.println(
      s"  have step_$resultStepName : ${LeanPrettyPrinter.prettyLeanFOFFormula(outputFormula)} := by (first | exact step_$resultStepName' | clearExcept step_$resultStepName'; simp_all | clearExcept step_$resultStepName'; grind only)"
    )
  }
  
  def findBoundVariablesUpToExistential(
      formula: FOF.Formula,
      skolemizedVariable: String
  ): (LinkedHashSet[String], Option[FOF.Formula]) = {
    formula match {
      case TPTP.FOF.AtomicFormula(f, args) => (LinkedHashSet.empty[String], None)
      case TPTP.FOF.QuantifiedFormula(q, variables, body) => {
        if (q == TPTP.FOF.!) {
          val (boundVars, exists) =
            findBoundVariablesUpToExistential(body, skolemizedVariable)
          return (LinkedHashSet.from(variables) ++ boundVars, exists)
        } else {
          if (variables.exists(_ == skolemizedVariable)) {
            return (LinkedHashSet.empty[String], Some(formula))
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
      case TPTP.FOF.Equality(left, right)   => (LinkedHashSet.empty[String], None)
      case TPTP.FOF.Inequality(left, right) => (LinkedHashSet.empty[String], None)
    }
  }

  def skolemizeFormulaWithSingleVariable(formula: TPTP.FOF.Formula, skolemizedVariable: String, skolemFunctionName: String, dependentVariables: Seq[String]): TPTP.FOF.Formula = {
    
    val (boundVars, exists) = findBoundVariablesUpToExistential(formula, skolemizedVariable)
    Logger.println(s"Found bound variables for skolemization: ${boundVars.mkString(", ")}")
    val skolemFunction = TPTP.FOF.AtomicTerm(
      skolemFunctionName,
      boundVars.toSeq.map(v => TPTP.FOF.Variable(v))
    )
    val skolemFormula = replaceVarWithTermAndDelQuant(formula, skolemizedVariable, skolemFunction)
    skolemFormula
  }

  def replaceVarWithTermAndDelQuant(formula: TPTP.FOF.Formula, variable: String, term: FOF.Term): TPTP.FOF.Formula = {
    formula match {
      case TPTP.FOF.AtomicFormula(f, args) =>
        TPTP.FOF.AtomicFormula(f, args.map(arg => replaceVArWithTermAndDelQuant(arg, variable, term)))
      case TPTP.FOF.QuantifiedFormula(q, variables, body) =>
        if (variables.contains(variable)) {
          if(variables.size == 1){
            //remove the quantifier
            replaceVarWithTermAndDelQuant(body, variable, term)
          } else {
            //remove the variable from the quantifier
            TPTP.FOF.QuantifiedFormula(q, variables.filterNot(_ == variable), replaceVarWithTermAndDelQuant(body, variable, term))
          }
        } else {
          TPTP.FOF.QuantifiedFormula(q, variables, replaceVarWithTermAndDelQuant(body, variable, term))
        }
      case TPTP.FOF.UnaryFormula(connective, body) =>
        TPTP.FOF.UnaryFormula(connective, replaceVarWithTermAndDelQuant(body, variable, term))
      case TPTP.FOF.BinaryFormula(connective, left, right) =>
        TPTP.FOF.BinaryFormula(connective,
          replaceVarWithTermAndDelQuant(left, variable, term),
          replaceVarWithTermAndDelQuant(right, variable, term)
        )
      case TPTP.FOF.Equality(left, right) =>
        TPTP.FOF.Equality(
          replaceVArWithTermAndDelQuant(left, variable, term),
          replaceVArWithTermAndDelQuant(right, variable, term)
        )
      case TPTP.FOF.Inequality(left, right) =>
        TPTP.FOF.Inequality(
          replaceVArWithTermAndDelQuant(left, variable, term),
          replaceVArWithTermAndDelQuant(right, variable, term)
        )
    }
  }
  def replaceVArWithTermAndDelQuant(term: TPTP.FOF.Term, variable: String, replacement: TPTP.FOF.Term): TPTP.FOF.Term = {
    term match {
      case TPTP.FOF.AtomicTerm(f, args) =>
        TPTP.FOF.AtomicTerm(f, args.map(arg => replaceVArWithTermAndDelQuant(arg, variable, replacement)))
      case TPTP.FOF.NumberTerm(n) => term
      case TPTP.FOF.Variable(v) =>
        if (v == variable) replacement else term
      case x@TPTP.FOF.DistinctObject(str) => x
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
        val parentNode = dag.nodes(node.parents.head)
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
              Logger.println(s"$variable , $function , ${args.mkString(", ")}")
              Logger.println(parentNode.formula.pretty)
              val (boundVariables, _) = findBoundVariablesUpToExistential(
                parentNode.formula.formula.asInstanceOf[TPTP.FOF.Logical].formula,
                variable
              )
              Logger.println(
                s"Skolem function $sym is defined for variable $variable with variables ${args.mkString(", ")} and formula has ${boundVariables.mkString(", ")} as bound variables", Logger.VERBOSITY_MEDIUM
              )
              if (args.size != boundVariables.size) {
                throw new ProofErrorException(
                  s"node ${node.name}: Skolem function $sym is defined with inconsistent arities in the proof annotation: ${args.size} vs formula ${boundVariables.size}"
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
