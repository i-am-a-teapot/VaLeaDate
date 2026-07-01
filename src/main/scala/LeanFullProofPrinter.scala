
import SkolemizationGeneration.generateSkolemization
import java.io.{FileWriter, PrintWriter}
import leo.datastructures.TPTP
import java.nio.file.Path

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
            throw new ProofErrorException(s"Expected exactly one parent for skolemization step $nodeName, found ${parents.size}")
          }
          val parent = parents.head
          val parentFormula = parent.formula.formula match {
            case TPTP.FOF.Logical(formula) => formula
            case _ => throw new ProofErrorException(s"Expected FOF formula for parent of skolemization step $nodeName")
          }
          val skolemizedVariable = skolemizationDetails.skolemDefinitions.headOption.map(_._1).getOrElse {
            throw new ProofErrorException(s"No skolemized variable found in details for node $nodeName")
          }
          val skolemFunctionName = skolemizationDetails.newSymbols.headOption.getOrElse {
            throw new ProofErrorException(s"No new symbols found in skolemization details for node $nodeName")
          }
          val resultStepName = node.name
          if(! node.formula.isInstanceOf[TPTP.FOFAnnotated]){
            throw new ProofUnsureException(s"Expected FOF formula for skolemization step $nodeName, found ${node.formula.getClass}")
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
      dag: ProofDag.Dag
  ): Unit = {
    sources.foreach { sourceName =>
        writer.print(LeanPrettyPrinter.prettyLeanSyntax(dag.nodes(sourceName).formula))
        writer.print(" → ")
    }
  }

  private def writeLeanConjecture(
      writer: PrintWriter,
      dag: ProofDag.Dag,
  ): Unit = {
    if (dag.conjectures.length > 1) {
      throw new ProofUnsureException(s"Multiple conjecture nodes found in DAG: ${dag.conjectures.mkString(", ")}. Only one conjecture node is supported.")
    }
    if (dag.conjectures.isEmpty) {
      writer.print("False")
    } else {
      dag.conjectures.foreach { conjectureName =>
        writer.print(LeanPrettyPrinter.prettyLeanSyntax(dag.nodes(conjectureName).formula))
      }
    }
  }

  def writeFullProof(writer: PrintWriter, dag: ProofDag.Dag, usedParents: Map[String, Seq[String]]): Unit = {
    writer.println("theorem fullFullProof : ")

    var containsConjectures = !dag.conjectures.isEmpty
    var sourcesToPrint = dag.sources.filter(sourceName => dag.nodes(sourceName).role == "axiom" || (!containsConjectures && dag.nodes(sourceName).role == "negated_conjecture"))
    writeLeanSources(writer, sourcesToPrint ,dag)
    writeLeanConjecture(writer, dag)
    writer.println(" := by")
    writer.print("  intro ")
    sourcesToPrint.foreach { sourceName => writer.print("step_" + sourceName + " ") } 
    writer.println("")
    writeLeanProofSteps(writer, dag, usedParents)
    writer.println("  exact step_" + dag.topologicalSort.lastOption.getOrElse {
      throw new ProofUnsureException("DAG is empty, no nodes found")
    })
  }

  def writeLeanOutputFile(
      outputFile: Path,
      translatedVariables: String,
      theoremCheckResults: Map[String, JobScheduler.ProcessResult],
      additionalObligationCheckResults: Map[String, JobScheduler.ProcessResult],
      dag: ProofDag.Dag,
      introducedVariables: Map[String, Int] = Map.empty,
      usedParents: Map[String, Seq[String]] = Map.empty
  ): Unit = {
    val writer = new PrintWriter(outputFile.toFile())
    try {
      LeanFullProofPrinter.writeLeanPreamble(writer)
      writer.println(translatedVariables)
      writer.println("variable [Data]")
      writer.println("variable [Inhabited Data.ι]")
      val variablesByArity =
        introducedVariables.groupBy(_._2).view.mapValues(_.keys.toSeq)
      variablesByArity.foreach { case (arity, variables) =>
        writer.print("variable {")
        writer.print(
          variables.map(node => s"_${node.toString()}").mkString(" ")
        )
        writer.println(s" : ${List.fill(arity)("ι →").mkString} ι}")
      }
      theoremCheckResults.foreach { case (node, result) =>
        writer.write("namespace " + node + "\n")
        writer.write(result.stdout)
        writer.write("end " + node + "\n")
      }
      additionalObligationCheckResults.foreach { case (node, result) =>
        writer.write("namespace " + node + "\n")
        writer.write(result.stdout)
        writer.write("end " + node + "\n")
      }

      LeanFullProofPrinter.writeFullProof(writer, dag, usedParents)

    } finally {
      writer.close()
    }
  }

  def writeLeanOutputFiles(
      outputDir: Path,
      translatedVariables: String,
      theoremCheckResults: Map[String, JobScheduler.ProcessResult],
      additionalObligationCheckResults: Map[String, JobScheduler.ProcessResult],
      dag: ProofDag.Dag,
      introducedVariables: Map[String, Int] = Map.empty,
      usedParents: Map[String, Seq[String]] = Map.empty,
      batchingSize: Int = 1
  ): (Path, Seq[Path], Path) = {
    // output data file
    val dataPath = outputDir.resolve("data.lean")
    var writer = new PrintWriter(new FileWriter(dataPath.toFile()))
    LeanFullProofPrinter.writeLeanPreamble(writer)
    writer.println(translatedVariables)
    writer.println("variable [Data]")
    writer.println("variable [Inhabited Data.ι]")
    writer.close();
    var batchNumber = 0
    var currentBatchSize = batchingSize + 1
    var outputFiles = Seq.empty[Path]
    for ((node, result) <- theoremCheckResults ++ additionalObligationCheckResults) {
      if (currentBatchSize >= batchingSize) {
        batchNumber += 1
        currentBatchSize = 0
        writer.close()
        val path = outputDir.resolve("batch" + batchNumber.toString + ".lean")
        outputFiles = outputFiles :+ path
        writer = new PrintWriter(new FileWriter(path.toFile()))
        writer.println("import data")
        LeanFullProofPrinter.writeLeanPreamble(writer)
        writer.println("variable [Data]")
        writer.println("variable [Inhabited Data.ι]")
        val variablesByArity =
          introducedVariables.groupBy(_._2).view.mapValues(_.keys.toSeq)
        variablesByArity.foreach { case (arity, variables) =>
          writer.print("variable {")
          writer.print(
            variables.map(node => s"_${node.toString()}").mkString(" ")
          )
          writer.println(s" : ${List.fill(arity)("ι →").mkString} ι}")
        }
      }
      writer.write("namespace " + node + "\n")
      writer.write(result.stdout)
      writer.write("end " + node + "\n")
      currentBatchSize += 1
    }
    writer.close()
    // write full proof file
    val fullProofPath = outputDir.resolve("full_proof.lean")
    val fullProofWriter = new PrintWriter(
      new FileWriter(fullProofPath.toFile())
    )
    fullProofWriter.println("import data")
    for (file <- outputFiles) {
      fullProofWriter.println(
        "import " + file.getFileName.toString.stripSuffix(".lean")
      )
    }
    LeanFullProofPrinter.writeLeanPreamble(fullProofWriter)
    fullProofWriter.println("variable [Data]")
    fullProofWriter.println("variable [Inhabited Data.ι]")
    val variablesByArity =
      introducedVariables.groupBy(_._2).view.mapValues(_.keys.toSeq)
    variablesByArity.foreach { case (arity, variables) =>
      fullProofWriter.print("variable {")
      fullProofWriter.print(
        variables.map(node => s"_${node.toString()}").mkString(" ")
      )
      fullProofWriter.println(s" : ${List.fill(arity)("ι →").mkString} ι}")
    }
    LeanFullProofPrinter.writeFullProof(fullProofWriter, dag, usedParents)
    fullProofWriter.close()
    (dataPath, outputFiles, fullProofPath)
  }

}
