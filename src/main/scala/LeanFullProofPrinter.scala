
import SkolemizationGeneration.generateSkolemization
import java.io.{FileWriter, PrintWriter}
import leo.datastructures.TPTP

object LeanFullProofPrinter {
    def writeLeanPreamble(writer: PrintWriter): Unit = {
    writer.println(
"""
import VampLean
universe u
set_option maxHeartbeats 0
set_option maxRecDepth 100000000""")
  }

  private def writeLeanProofSteps(writer: PrintWriter, proof: ProofDag.Dag, usedParents: Map[String, Seq[String]]): Unit = {
    proof.topologicalSort.foreach { nodeName =>
      val node = proof.nodes(nodeName)
      val containsConjectures = !proof.conjectures.isEmpty
      if (!(node.role == "axiom" || (containsConjectures && node.role == "conjecture") || (!containsConjectures && node.role == "negated_conjecture") || AnnotationInformationHelpers.isCth(node.additionalInfo))) {
        if(AnnotationInformationHelpers.containsRuleStep("skolemize", node.additionalInfo)){ 
          val skolemizationDetails = AnnotationInformationHelpers.getSkolemizationInformation(node.additionalInfo)
          val parents = node.parents.map(parentName => proof.nodes(parentName))
          if(parents.size != 1){
            throw new IllegalArgumentException(s"Expected exactly one parent for skolemization step $nodeName, found ${parents.size}")
          }
          val parent = parents.head
          val parentFormula = parent.formula.formula match {
            case TPTP.FOF.Logical(formula) => formula
            case _ => throw new IllegalArgumentException(s"Expected FOF formula for parent of skolemization step $nodeName")
          }
          val skolemizedVariable = skolemizationDetails.skolemDefinitions.headOption.map(_._1).getOrElse {
            throw new IllegalArgumentException(s"No skolemized variable found in details for node $nodeName")
          }
          val skolemFunctionName = skolemizationDetails.newSymbols.headOption.getOrElse {
            throw new IllegalArgumentException(s"No new symbols found in skolemization details for node $nodeName")
          }
          val resultStepName = node.name
          if(! node.formula.isInstanceOf[TPTP.FOFAnnotated]){
            throw new IllegalArgumentException(s"Expected FOF formula for skolemization step $nodeName, found ${node.formula.getClass}")
          }
          generateSkolemization(writer, parent.name, parentFormula, skolemizedVariable, skolemFunctionName, resultStepName, node.formula.formula.asInstanceOf[TPTP.FOF.Logical].formula)
        } else {
          writer.print("  have step_" + nodeName + " := " + nodeName + "." + nodeName + "_fullProof ")
          writer.print(usedParents.getOrElse(nodeName, Seq.empty).map(parent => "step_" + parent).mkString(" "))
          writer.println(" -- role:" + node.role)
        }
      } else if (AnnotationInformationHelpers.isCth(node.additionalInfo)) {
        writer.println("  apply Classical.byContradiction; intro step_" + nodeName)
      }
    }
  }

  private def writeLeanSources(
      writer: PrintWriter,
      sources: Seq[String],
      leanFormulasByNodeName: Map[String, String],
      dag: ProofDag.Dag
  ): Unit = {
    sources.foreach { sourceName =>
        writer.print(LeanPrettyPrinter.prettyLeanSyntax(dag.nodes(sourceName).formula))
        //leanFormulasByNodeName.get(sourceName) match {
        //  case Some(formula) => writer.print(formula)
        //  case None => throw new ProofErrorException(s"No formula found for source node $sourceName in input formula map")
        //}
        writer.print(" → ")
    }
  }

  private def writeLeanConjecture(
      writer: PrintWriter,
      dag: ProofDag.Dag,
      leanFormulasByNodeName: Map[String, String],
  ): Unit = {
    if (dag.conjectures.length > 1) {
      throw new IllegalArgumentException(s"Multiple conjecture nodes found in DAG: ${dag.conjectures.mkString(", ")}. Only one conjecture node is supported.")
    }

    if (dag.conjectures.isEmpty) {
      writer.print("False")
    } else {
      dag.conjectures.foreach { conjectureName =>
        writer.print(LeanPrettyPrinter.prettyLeanSyntax(dag.nodes(conjectureName).formula))
        //leanFormulasByNodeName.get(conjectureName) match {
        //  case Some(formula) => writer.print(formula)
        //  case None => throw new ProofErrorException(s"No formula found for conjecture node $conjectureName in input formula map")
        //}
      }
    }
  }

  def writeFullProof(writer: PrintWriter, dag: ProofDag.Dag, leanFormulasByProblemNodeName: Map[String, String], usedParents: Map[String, Seq[String]]): Unit = {
    writer.println("theorem fullFullProof : ")

    var containsConjectures = !dag.conjectures.isEmpty
    var sourcesToPrint = dag.sources.filter(sourceName => dag.nodes(sourceName).role == "axiom" || (!containsConjectures && dag.nodes(sourceName).role == "negated_conjecture"))
    writeLeanSources(writer, sourcesToPrint, leanFormulasByProblemNodeName,dag)
    writeLeanConjecture(writer, dag, leanFormulasByProblemNodeName)
    writer.println(" := by")
    writer.print("  intro ")
    sourcesToPrint.foreach { sourceName => writer.print("step_" + sourceName + " ") } 
    writer.println("")
    writeLeanProofSteps(writer, dag, usedParents)
    writer.println("  exact step_" + dag.topologicalSort.lastOption.getOrElse {
      throw new IllegalArgumentException("DAG is empty, no nodes found")
    })
  }
}
