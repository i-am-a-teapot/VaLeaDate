import scopt.OParser
import scala.io.Source
import java.io.{FileWriter, PrintWriter}
import leo.modules.input.TPTPParser
import leo.datastructures.TPTP
import scala.concurrent.Await
import java.nio.file.Paths
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import LeanFullProofPrinter.{writeLeanPreamble, writeFullProof}
import java.util.Timer
import java.util.TimerTask
import scala.concurrent.duration._

object Main {

  case class Settings(
    var vampireBinary : String = "",
    var leanLibraryPath : String = "",
    var leanBinary: String = "",
    var tptpDirectory : String = ""
  );

  case class Config(
    inputProblemFile: String = "",
    inputFile: String = "",
    verbosity: Int = 0,
    output: String = "",
    timeout: Int = 30,
    leanBinary: String = "",
    vampireBinary: String = "",
    leanLibraryPath: String = "",
    tptpDirectory: String = "",
    verifyWithLean: Boolean = true,
    pathForLeanOutput: Option[String] = None,
    assumeThm : Boolean = false,
    treatNegatedConjectureAsAxiom : Boolean = false,
    allowSyntacticMismatchOfAxioms : Boolean = false
  )

  private val settings : Settings = Settings();

  private var additionalProofObligations : Vector[TPTPProblemGenerator.Inference] = Vector.empty

  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Config]
    settings.leanLibraryPath = scala.util.Properties.envOrElse("VAMPLEAN_PATH", "vamplean/.lake/build/lib/lean")
    settings.leanBinary = scala.util.Properties.envOrElse("LEAN_BINARY", "requirements/elan/toolchains/leanprover--lean4---v4.31.0/bin/lean")
    settings.vampireBinary = scala.util.Properties.envOrElse("VAMPIRE_BINARY", "vampire/build/vampire")
    settings.tptpDirectory = scala.util.Properties.envOrElse("TPTP", Paths.get(System.getProperty("user.home"),"TPTP-v9.2.1").toString())

    val parser = {
      import builder._
      OParser.sequence(
        programName("VaLeaDate"),
        head("VaLeaDate", "0.1.0"),
        arg[String]("<input_proof>")
          .required()
          .action((x, c) => c.copy(inputFile = x))
          .text("input TPTP file"),
        opt[String]('o', "output")
          .action((x, c) => c.copy(output = x))
          .text("output file for Graphviz DOT (optional)"),
        opt[String]('i', "input-problem")
          .action((x, c) => c.copy(inputProblemFile = x))
          .text("input TPTP problem file"),
        opt[Unit]("v")
          .action((x, c) => c.copy(verbosity = 1))
          .text("enable verbose output"),
        opt[Unit]("vv")
          .action((x, c) => c.copy(verbosity = 2 ))
          .text("enable very verbose output"),
        opt[String]('l', "lean-binary")
          .action((x, c) => { c.copy(leanBinary = x) })
          .text("path to the Lean binary"),
        opt[String]('V', "vampire-binary")
          .action((x, c) => { c.copy(vampireBinary = x) })
          .text("path to the Vampire binary"),
        opt[String]('L', "lean-library-path")
          .action((x, c) => { c.copy(leanLibraryPath = x) })
          .text("path to the Lean library"),
        opt[Unit]('n', "no-lean-check")
          .action((_, c) => c.copy(verifyWithLean = false))
          .text("disable verification with Lean"),
        opt[String]('p', "lean-output-path")
          .action((x, c) => c.copy(pathForLeanOutput = Some(x)))
          .text("path to write Lean output file (optional)"),
        opt[Unit]("assume-thm")
          .action((_, c) => c.copy(assumeThm = true))
          .text("assume all formulas without status are theorems"),
        opt[Unit]("negc-as-thm")
          .action((_, c) => c.copy(treatNegatedConjectureAsAxiom = true))
          .text("treat negated conjecture formulas as theorems"),
        opt[Unit]("allow-axiom-mismatch")
          .action((_, c) => c.copy(allowSyntacticMismatchOfAxioms = true))
          .text("allow syntactic mismatch of axioms between proof and input problem"),
        opt[String]("tptp-directory")
          .action((x, c) => c.copy(tptpDirectory = x))
          .text("path to the TPTP directory"),
        opt[Int]('t', "timeout")
          .action((x, c) => c.copy(timeout = x))
          .text("timeout in seconds (default: 30)"),
        help("help")
          .text("print this help message")
      )
    }

    OParser.parse(parser, args, Config()) match {
      case Some(config) =>
        if(config.leanBinary.nonEmpty) settings.leanBinary = config.leanBinary
        if(config.vampireBinary.nonEmpty) settings.vampireBinary = config.vampireBinary
        if(config.leanLibraryPath.nonEmpty) settings.leanLibraryPath = config.leanLibraryPath
        if(config.tptpDirectory.nonEmpty) settings.tptpDirectory = config.tptpDirectory
        val timeout = config.timeout.seconds
        val watcher = new Timer(true)
        watcher.schedule(new TimerTask {
          def run(): Unit = {
            println("%SZS status: Timeout")
            killChildProcesses()
            System.exit(1)
          }
        }, timeout.toMillis)
        runConfigurationChecks(settings, config)
        run(config)
      case _ =>
        System.exit(1)
    }
  }  

  private def runConfigurationChecks(settings : Settings, config : Config) : Unit = {
    val leanBinaryPath = Paths.get(settings.leanBinary)
    if(!Files.exists(leanBinaryPath) || !Files.isExecutable(leanBinaryPath)) {
      throw new IllegalArgumentException(s"Lean binary not found or not executable at path: ${settings.leanBinary}")
    }
    val vampireBinaryPath = Paths.get(settings.vampireBinary)
    if(!Files.exists(vampireBinaryPath) || !Files.isExecutable(vampireBinaryPath)) {
      throw new IllegalArgumentException(s"Vampire binary not found or not executable at path: ${settings.vampireBinary}")
    }
    val leanLibraryPath = Paths.get(settings.leanLibraryPath)
    if(!Files.isDirectory(leanLibraryPath)) {
      throw new IllegalArgumentException(s"Lean library path is not a directory: ${settings.leanLibraryPath}")
    }
    val tptpDirectoryPath = Paths.get(settings.tptpDirectory)
    if(!Files.isDirectory(tptpDirectoryPath)) {
      throw new IllegalArgumentException(s"TPTP directory is not a directory: ${settings.tptpDirectory}")
    }
  }

  def killChildProcesses(): Unit = {
    val currentProcess = ProcessHandle.current()
    // Get a stream of all descendants (children, grandchildren, etc.)
    currentProcess.descendants().forEach { handle =>
      if (handle.isAlive) {
        Logger.println(s"Killing child process PID: ${handle.pid()} -> ${handle.info().command().orElse("Unknown")}")
        handle.destroyForcibly() // Equivalent to 'kill -9'
      }
    }
  }

  private def loadAnnotatedFormulas(path: String, baseDir: String, settings: Settings): Seq[TPTP.AnnotatedFormula] = {
    var fullPath = Paths.get(path)
    
    if(!fullPath.isAbsolute){
      val alternativePath = Paths.get(baseDir, path)
      if(Files.exists(alternativePath) && Files.isRegularFile(alternativePath)){
        fullPath = alternativePath
      } else {
        val alternativePath2 = Paths.get(settings.tptpDirectory, path)
        if(Files.exists(alternativePath2) && Files.isRegularFile(alternativePath2)){
          fullPath = alternativePath2
        } else {
          throw new IllegalArgumentException(s"Input file not found: $path, tried baseDir: $baseDir and TPTP directory: ${settings.tptpDirectory}")
        }
      }
    }

    val source = Source.fromFile(fullPath.toFile)
    var allFormulas = Seq.empty[TPTP.AnnotatedFormula]
    try {
      val problem = TPTPParser.problem(source)
      for(includes <- problem.includes){
        var formulas = loadAnnotatedFormulas(includes._1, baseDir, settings)
        allFormulas = allFormulas ++ formulas
      }
      allFormulas ++ problem.formulas
    } finally {
      source.close()
    }
  }

  private def formulasAreSyntacticallyEqual(left: TPTP.AnnotatedFormula, right: TPTP.AnnotatedFormula): Boolean = {
    (left, right) match {
      case (TPTP.FOFAnnotated(_, _, leftForm, _), TPTP.FOFAnnotated(_, _, rightForm, _)) => leftForm == rightForm
      case (TPTP.CNFAnnotated(_, _, leftForm, _), TPTP.CNFAnnotated(_, _, rightForm, _)) => leftForm == rightForm
      case _ => false
    }
  }

  private def rewriteProofAxiomIfNotEqToInputProb(
      dag: ProofDag.Dag,
      nodeName: String,
      problemFormulas: Seq[TPTP.AnnotatedFormula]
  ): ProofDag.Dag = {
    val node = dag.nodes(nodeName)
    val problemFormulaName = AnnotationInformationHelpers.fileParentInformation(node.additionalInfo) match {
      case Some(FileInformation(_, name)) => name
      case _ => throw new IllegalArgumentException(s"Expected FileInformation for axiom node $nodeName in DAG")
    }

    val problemFormula = problemFormulas.find(_.name == problemFormulaName)
    val isSyntacticallyEqual = problemFormula.exists(formulasAreSyntacticallyEqual(node.formula, _))
    if (isSyntacticallyEqual) {
      dag
    } else {
      Logger.println("Syntactic mismatch found for axiom node " + nodeName)
      val replacementNodeName = nodeName + "_inputTransf"
      val replacementFormula = 
        problemFormula match {
          case Some(TPTP.CNFAnnotated(name, role, form, annotations)) =>
            TPTP.FOFAnnotated(replacementNodeName, role, TPTPProblemGenerator.cnfStatementToFOF(form), annotations)
          case Some(TPTP.FOFAnnotated(name, role, form, annotations)) =>
            TPTP.FOFAnnotated(replacementNodeName, role, form, annotations)
          case what => {
            Logger.println(what)
             throw new IllegalArgumentException(s"Expected CNF or FOF formula for problem formula $problemFormulaName")
          }
        }
      
      val replacementNode = ProofDag.Node(
        replacementNodeName,
        "axiom",
        replacementFormula,
        node.additionalInfo
      )

      additionalProofObligations :+= TPTPProblemGenerator.Inference("rev_obligation_axiom_"+node.name,
        Seq(node.formula),
        replacementFormula
      )
      Logger.println(s"Added additional proof obligation for axiom node $nodeName: ${replacementFormula.name}")
      var updatedNodes = dag.nodes
      val rewrittenNode = updatedNodes(node.name).copy(
        role = "plain",
        additionalInfo = InferenceInformation("thm", "thm", Seq(NamedParentInformation(replacementNodeName)))
      )
      updatedNodes = updatedNodes.updated(node.name, rewrittenNode)
      updatedNodes = updatedNodes + (replacementNodeName -> replacementNode)

      val updatedDag = ProofDag.Dag(updatedNodes)
      Logger.println(s"Adding node: ${replacementNodeName} -> ${nodeName} to list")
      //Logger.println(s"leanFormulasByNodeName: ${leanFormulasByNodeName.keys.mkString(", ")}")
      updatedDag
    }
  }
  
  private def negateFormula(formula: TPTP.AnnotatedFormula): TPTP.FOF.Formula = {
    formula match {
      case TPTP.FOFAnnotated(_, _, statement, _) =>
        val innerFormula = statement match {
          case TPTP.FOF.Logical(formula) => formula
          case _ => throw new IllegalArgumentException(s"Expected logical FOF statement for parent formula ${formula.name}")
        }
        TPTP.FOF.UnaryFormula(TPTP.FOF.~, innerFormula)
      case _ => throw new IllegalArgumentException(s"Expected logical FOF statement for parent formula ${formula.name}")
    }
  }

  private def rewriteNegatedConjectureIfNotEqToSyntacticNeg(
      dag: ProofDag.Dag,
      nodeName: String,
      problemFormulas: Seq[TPTP.AnnotatedFormula]
  ): ProofDag.Dag = {
    val node = dag.nodes(nodeName)
    val parentNames = node.parents

    if (parentNames.size != 1) {
      throw new IllegalArgumentException(s"Expected exactly one parent for negated conjecture node $nodeName in DAG, found ${parentNames.size}")
    }

    val parentName = parentNames.head
    val parentFormula = dag.nodes(parentName).formula
    val negatedParentFormula = negateFormula(parentFormula)

    val isSyntacticNegation = node.formula match {
      case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(actualFormula), _) => negatedParentFormula == actualFormula
      case _ => false 
    }

    if (isSyntacticNegation) {
      dag
    } else {

      additionalProofObligations :+= TPTPProblemGenerator.Inference("rev_obligation_neg_"+node.name,
        Seq(node.formula),
        TPTP.FOFAnnotated("rev_obligation_neg_"+node.name, "conjecture", TPTP.FOF.Logical(negatedParentFormula), None)
      )

      Logger.println("Syntactic mismatch found for negated conjecture node " + nodeName)
      val rewrittenParentFormula = parentFormula
      
      val replacementNodeName = nodeName + "_inputTrans"
      val replacementNode = ProofDag.Node(
        replacementNodeName,
        "negated_conjecture",
        TPTP.FOFAnnotated(replacementNodeName, "negated_conjecture", TPTP.FOF.Logical(negatedParentFormula), None),
        InferenceInformation("cth", "negated_conjecture", Seq(NamedParentInformation(parentName)))
      )

      var updatedNodes = dag.nodes
      val rewrittenNode = updatedNodes(node.name).copy(
        role = "plain",
        formula = node.formula,
        additionalInfo = InferenceInformation("thm", "thm", Seq(NamedParentInformation(replacementNodeName)))
      )
      updatedNodes = updatedNodes.updated(node.name, rewrittenNode)
      updatedNodes = updatedNodes.updated(parentName, updatedNodes(parentName).copy(formula = rewrittenParentFormula))
      updatedNodes = updatedNodes + (replacementNodeName -> replacementNode)

      ProofDag.Dag(updatedNodes)
    }
  }

  private def addInferencesIfSyntacticMismatch(
      dag: ProofDag.Dag,
      problemFormulas: Seq[TPTP.AnnotatedFormula],
      allowSyntacticMismatchOfAxioms: Boolean
  ): ProofDag.Dag = {
    var currentDag = dag
    if(allowSyntacticMismatchOfAxioms)
    {
      val (updatedDag) = currentDag.axioms.foldLeft(currentDag) {
        case (currentDag, nodeName) =>
          rewriteProofAxiomIfNotEqToInputProb(currentDag, nodeName, problemFormulas)
      }
      currentDag = updatedDag
    }

    val (updatedDag) = currentDag.countersatisfiable.foldLeft(currentDag) {
      case (currentDag, nodeName) =>
        rewriteNegatedConjectureIfNotEqToSyntacticNeg(currentDag, nodeName, problemFormulas)
    }
    currentDag = updatedDag
    return currentDag
  }


  private def collectTheoremInferences(dag: ProofDag.Dag, assumeThm: Boolean): Seq[TPTPProblemGenerator.Inference] = {
    val theoremNodes = dag.nodes.values.filter { node =>
      AnnotationInformationHelpers.isThm(node.additionalInfo, assumeThm) && (node.role =="plain")
    }.toSeq
    

    theoremNodes.map { theoremNode =>
      val theoremParents = theoremNode.parents.map(dag.nodes)
      TPTPProblemGenerator.Inference(theoremNode.name, theoremParents.map(_.formula), theoremNode.formula)
    }
  }

  private def buildVampireJobSpec(inputProblemFile: String, proofFileBasePath: String): JobScheduler.JobSpec = {
    var fullPath = Paths.get(inputProblemFile)
    if(!fullPath.isAbsolute){
      fullPath = Paths.get(proofFileBasePath, inputProblemFile)
    }
    JobScheduler.JobSpec(Seq(settings.vampireBinary, fullPath.toString(), "--mode", "translate", "--include", "/home/jonas/TPTP-v9.2.1/","--output_axiom_names","on"))
  }

  private def buildTheoremCheckJobSpec(inference: TPTPProblemGenerator.Inference): JobScheduler.JobSpec = {
    val command = Seq(
      settings.vampireBinary,
      "--proof_extra",
      "lean",
      "--proof",
      "leancheck",
      "-om",
      "lean",
      "--skolemization",
      "syntactic",
      "-lpp",
      inference.name + "_",
      "-wvo",
      "introduced_only",
    )
    val problemText = TPTPProblemGenerator.generateProblemFromInference(inference)
    JobScheduler.JobSpec(command, stdin = problemText)
  }

  private def writeLeanOutputFile(
      outputFile: Path,
      translatedVariables: String,
      theoremCheckResults: Map[String, JobScheduler.ProcessResult],
      additionalObligationCheckResults: Map[String, JobScheduler.ProcessResult],
      dag: ProofDag.Dag,
      introducedVariables : Map[String, Int] = Map.empty,
      usedParents: Map[String, Seq[String]] = Map.empty,
  ): Unit = {
    val writer = new PrintWriter(outputFile.toFile())
    try {
      LeanFullProofPrinter.writeLeanPreamble(writer)
      writer.println(translatedVariables)
      writer.println("variable [Data]")
      writer.println("variable [Inhabited Data.ι]")
      val variablesByArity = introducedVariables.groupBy(_._2).view.mapValues(_.keys.toSeq)
      variablesByArity.foreach { case (arity, variables) =>
        writer.print("variable {")
        writer.print(variables.map(node => s"_${node.toString()}").mkString(" "))
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

  

  private def runLeanCheck(outputFile: Path): JobScheduler.ProcessResult = {
    val leanCheckSpec = JobScheduler.JobSpec(Seq(settings.leanBinary, "-s", "16000", outputFile.toString()), "", Map("LEAN_PATH" -> settings.leanLibraryPath))
    val leanCheckFuture = JobScheduler.runFuture(leanCheckSpec)(scala.concurrent.ExecutionContext.global)
    Await.result(leanCheckFuture, scala.concurrent.duration.Duration.Inf)
  }

  def run(config: Config): Unit = {
    Logger.setVerbose(config.verbosity)
    try {
      Logger.println(s"Reading input from: ${config.inputFile}")
      Logger.println("Parsing input proof file...")
      val proofPath = Paths.get(config.inputFile)
      val proofFileBasePath = proofPath.getParent.toAbsolutePath.toString
      val proofFileName = proofPath.getFileName.toString
      val proofFormulas = loadAnnotatedFormulas(proofFileName, proofFileBasePath, settings)

      Logger.println("Building proof DAG...")
      var dag = ProofDag.fromProof(proofFormulas, config.assumeThm, config.treatNegatedConjectureAsAxiom)

      var formulaFileInfos = dag.sources.map(source => {
          AnnotationInformationHelpers.fileParentInformation(dag.nodes(source).additionalInfo) match {
            case Some(fileInfo) => Some((source, fileInfo.fileName, fileInfo.formulaName))
            case _ => None
          }
        }
      ).filter(_.isDefined).map(_.get)
      var problemFiles = formulaFileInfos.map(_._2).distinct
      var problemFileToProofFileNames = formulaFileInfos.map(info => info._3 -> info._1).toMap

      Logger.println("Parse referenced problem files: " + problemFiles.mkString(", "))
      Logger.println("Parsing input problem file...")

      var problemFormulas : Seq[TPTP.AnnotatedFormula] = Seq.empty;
      var translationJobs = Seq.empty[JobScheduler.JobSpec]
      if(problemFiles.nonEmpty){
        for(problemFile <- problemFiles){
          var formulas = loadAnnotatedFormulas(problemFile, proofFileBasePath, settings)
          problemFormulas = problemFormulas ++ formulas
          var translationJob = buildVampireJobSpec(problemFile, proofFileBasePath)
          translationJobs = translationJobs :+ translationJob
        }
      }

      val translationResultsFuture = JobScheduler.runFutures(translationJobs)(scala.concurrent.ExecutionContext.global)
      val renderedDag = dag.toDot

      // Output results
      if (config.output.nonEmpty) {
        val writer = new PrintWriter(new FileWriter(config.output))
        try {
          writer.write(renderedDag)
        } finally {
          writer.close()
        }
        Logger.println(s"Proof DAG written to: ${config.output}")
      } 
      
      //Logger.println(dag.edges)
      //Logger.println(dag.toDot)
      Logger.println("Performing basic checks on proof DAG...")
      BasicChecks.performAllBasicChecks(dag, problemFormulas, config.allowSyntacticMismatchOfAxioms)

      Logger.println("Extracting skolemization details from proof DAG...")
      val skolemFunctionArities = SkolemizationGeneration.checkSkolemizationDetailsAreConsistent(dag)
      Logger.println(skolemFunctionArities)
      val translationResult = Await.result(translationResultsFuture, scala.concurrent.duration.Duration.Inf)
      //Logger.println("Parsing translation result...")
      //Logger.println(translationResult.map(_.stdout).mkString("\n\n"))
      //var leanFormulasByProblemNodeName : Map[String, String] = Map.empty
      var translatedVariables : String = ""
      for(result <- translationResult){
        val parsedTranslation = TranslationResult.parseTranslationResult(result.stdout)
        translatedVariables += parsedTranslation.variableDeclarations + "\n"
      }

      //var leanFormulasByNodeName = leanFormulasByProblemNodeName.map { case (nodeName, formula) =>
      //  var axiomNameInProof = problemFileToProofFileNames.getOrElse(nodeName, nodeName)
      //  (axiomNameInProof, formula)
      //}.toMap

      
      Logger.println("Checking for syntactic mismatches between proof DAG and input problem...")
      dag = addInferencesIfSyntacticMismatch(dag, problemFormulas, config.allowSyntacticMismatchOfAxioms)

      //Logger.println("Lean formulas by node name:", verbosity = Logger.VERBOSITY_HIGH)
      //Logger.println(leanFormulasByNodeName.keys.mkString(", "), verbosity = Logger.VERBOSITY_HIGH)
      //write dag to dot file after adding additional proof obligations
      val renderedDagAfterAdditionalObligations = dag.toDot
      if (config.output.nonEmpty) {
        val writer = new PrintWriter(new FileWriter(config.output))
        try {
          writer.write(renderedDagAfterAdditionalObligations)
        } finally {
          writer.close()
        }
        Logger.println(s"Proof DAG after adding additional proof obligations written to: ${config.output}")
      }

      val theoremInferences = collectTheoremInferences(dag, config.assumeThm)
      Logger.print(theoremInferences.map(_.name).mkString(", "))
      val theoremCheckResultsFuture = JobScheduler.runNodes(theoremInferences, parallelism = 8)(buildTheoremCheckJobSpec)
      val additionalObligationCheckResultsFuture = JobScheduler.runNodes(additionalProofObligations, parallelism = 8)(buildTheoremCheckJobSpec)

      val theoremCheckResults = Await.result(theoremCheckResultsFuture, scala.concurrent.duration.Duration.Inf)
      val additionalObligationCheckResults = Await.result(additionalObligationCheckResultsFuture, scala.concurrent.duration.Duration.Inf)

      val nonRefutedNodes = theoremCheckResults.filter { case (_, result) =>
        !result.stdout.contains("-- Termination reason: Refutation")
      }.keys.toSeq

      val additionalObligationCheckFailures = additionalObligationCheckResults.filter { case (_, result) =>
        !result.stdout.contains("-- Termination reason: Refutation")
      }.keys.toSeq

      if (additionalObligationCheckFailures.nonEmpty) {
        for(node <- additionalObligationCheckFailures){
          Logger.println(s"Additional proof obligation node $node did not produce a refutation. Input was:", verbosity = Logger.VERBOSITY_LOW)
          Logger.println(additionalObligationCheckResults(node).stdin, verbosity = Logger.VERBOSITY_LOW)
          Logger.println(s"Vampire output was:", verbosity = Logger.VERBOSITY_LOW)
          Logger.println(additionalObligationCheckResults(node).stdout, verbosity = Logger.VERBOSITY_LOW)
        }
        throw new ProofErrorException(s"Some additional proof obligations did not produce a refutation (are possibly unsound): ${additionalObligationCheckFailures.mkString(", ")}")
      }

      if (nonRefutedNodes.nonEmpty) {
        for(node <- nonRefutedNodes){
          Logger.println(s"Theorem node $node did not produce a refutation. Input was:", verbosity = Logger.VERBOSITY_LOW)
          Logger.println(theoremCheckResults(node).stdin, verbosity = Logger.VERBOSITY_LOW)
          Logger.println(s"Vampire output was:", verbosity = Logger.VERBOSITY_LOW)
          Logger.println(theoremCheckResults(node).stdout, verbosity = Logger.VERBOSITY_LOW)
        }
        throw new ProofErrorException(s"Some theorem nodes did not produce a refutation (are possibly unsound): ${nonRefutedNodes.mkString(", ")}")
      }

      val usedParents = theoremCheckResults.map { case (name, result) =>
        name -> result.usedParents
      }
      val outputFile: Path = if(config.pathForLeanOutput.isDefined){
        Paths.get(config.pathForLeanOutput.get)
      } else {
        Files.createTempFile("lean_output", ".lean")
      }
      
      writeLeanOutputFile(outputFile, translatedVariables,
                  theoremCheckResults, additionalObligationCheckResults, dag, 
                  skolemFunctionArities, usedParents)
      val leanCheckResult = runLeanCheck(outputFile)
      if(config.pathForLeanOutput.isEmpty){
        Files.delete(outputFile)
      }
      Logger.println(leanCheckResult.durationMillis)
  
      if (leanCheckResult.exitCode != 0) {
        throw new ProofErrorException(s"Lean check failed with exit code ${leanCheckResult.exitCode}.")
      } 

      println(s"%SZS status VerifiedGood")
    } catch {
      case e: ProofErrorException =>
        println(s"%SZS status VerifiedBad : ${e.getMessage}")
      case e: IllegalArgumentException =>
        println(s"%SZS status Unknown : ${e.getMessage}")
      case e: Exception =>
        println(s"%SZS status Unknown : ${e.getMessage}")
    }

  }
}
