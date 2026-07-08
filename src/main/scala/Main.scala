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
import TPTPProblemGenerator.buildTheoremCheckJobSpec
import java.util.Timer
import java.util.TimerTask
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

object Main {

  case class Settings(
      var vampireBinary: String = "",
      var leanLibraryPath: String = "",
      var leanBinary: String = "",
      var tptpDirectory: String = ""
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
      assumeThm: Boolean = false,
      parallel: Int = 8,
      treatNegatedConjectureAsAxiom: Boolean = false,
      allowSyntacticMismatchOfAxioms: Boolean = false,
      compileWithMultipleLeanFiles: Boolean = false,
      autoSwitchToMultiThresholdSetManually: Boolean = false,
      autoSwitchToMultiThreshold: Int = 64 * 1024 * 8,
      batchSizeLeanFiles: Int = 64 * 1024
  )

  private val settings: Settings = Settings();

  def main(args: Array[String]): Unit = {
    val builder = OParser.builder[Config]

    // Try to get the base path of the binary
    var binaryBasePath = ""
    try {
      var binaryPath = Files.readSymbolicLink(Path.of("/proc/self/exe"))
      binaryBasePath = binaryPath.getParent.toAbsolutePath.toString
    } catch {
      case e: Exception => {}
    }
    // These are the default paths when installed via quickinstall script.
    settings.leanLibraryPath = scala.util.Properties.envOrElse(
      "VAMPLEAN_PATH",
      Paths.get(binaryBasePath, "vamplean/.lake/build/lib/lean").toString()
    )
    settings.leanBinary = scala.util.Properties.envOrElse(
      "LEAN_BINARY",
      Paths
        .get(
          binaryBasePath,
          "requirements/elan/toolchains/leanprover--lean4---v4.31.0/bin/lean"
        )
        .toString()
    )
    settings.vampireBinary = scala.util.Properties.envOrElse(
      "VAMPIRE_BINARY",
      Paths.get(binaryBasePath, "vampire/build/vampire").toString()
    )
    settings.tptpDirectory = scala.util.Properties.envOrElse(
      "TPTP",
      Paths.get(System.getProperty("user.home"), "TPTP-v9.2.1").toString()
    )

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
          .action((x, c) => c.copy(verbosity = 2))
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
          .text(
            "allow syntactic mismatch of axioms between proof and input problem"
          ),
        opt[String]("tptp-directory")
          .action((x, c) => c.copy(tptpDirectory = x))
          .text("path to the TPTP directory"),
        opt[Int]('t', "timeout")
          .action((x, c) => c.copy(timeout = x))
          .text("timeout in seconds (default: 30)"),
        opt[Int]('j', "parallel")
          .action((x, c) => c.copy(parallel = x))
          .text(
            "number of parallel processes for theorem checking (default: 8)"
          )
          .validate(x =>
            if (x >= 0) success
            else failure("Value <parallel> must be a positive integer")
          ),
        opt[Unit]("multiple-lean-files")
          .action((_, c) => c.copy(compileWithMultipleLeanFiles = true))
          .text("compile Lean output with multiple files instead of one file"),
        opt[Int]("batch-size")
          .action((x, c) => c.copy(batchSizeLeanFiles = x * 1024))
          .text(
            "batch size (in kiB) for Lean files when compiling with multiple files"
          ),
        opt[Int]("auto-switch-to-multi-threshold")
          .action((x, c) => {
            c.copy(
              autoSwitchToMultiThreshold = x * 1024,
              autoSwitchToMultiThresholdSetManually = true
            )
          })
          .text(
            "if the combined size (in kiB) of theorem outputs exceeds this threshold, automatically switch to compiling with multiple Lean files (default: batch-size*parallel)"
          ),
        help("help")
          .text("print this help message")
      )
    }

    OParser.parse(parser, args, Config()) match {
      case Some(config) =>
        val conf = if (!config.autoSwitchToMultiThresholdSetManually) {
          config.copy(autoSwitchToMultiThreshold =
            config.batchSizeLeanFiles * config.parallel
          )
        } else { config }

        if (conf.leanBinary.nonEmpty) settings.leanBinary = conf.leanBinary
        if (conf.vampireBinary.nonEmpty)
          settings.vampireBinary = conf.vampireBinary
        if (conf.leanLibraryPath.nonEmpty)
          settings.leanLibraryPath = conf.leanLibraryPath
        if (conf.tptpDirectory.nonEmpty)
          settings.tptpDirectory = conf.tptpDirectory
        val timeout = conf.timeout.seconds
        val watcher = new Timer(true)
        try {
          watcher.schedule(
            new TimerTask {
              def run(): Unit = {
                println("%SZS status: Timeout")
                killChildProcesses()
                System.exit(1)
              }
            },
            timeout.toMillis
          )
          runConfigurationChecks(settings, conf)
          val stackSizeInBytes = 1024 * 1024 * 1024

          val appRunnable = new Runnable {
            override def run(): Unit = {
              try {
                runApp(conf)
              } catch {
                case e: Throwable =>
                  e.printStackTrace()
                  sys.exit(1)
              }
            }
          }

          // Spawn a new thread with the explicitly requested stack size
          val mainThreadOverride = new Thread(
            null,
            appRunnable,
            "scala-main-override",
            stackSizeInBytes
          )

          mainThreadOverride.start()
          mainThreadOverride
            .join() // Block the musl main thread until your app finishes
        } finally {
          watcher.cancel()
        }
      case _ =>
        System.exit(1)
    }
  }

  private def runConfigurationChecks(
      settings: Settings,
      config: Config
  ): Unit = {
    val leanBinaryPath = Paths.get(settings.leanBinary)
    if (!Files.exists(leanBinaryPath) || !Files.isExecutable(leanBinaryPath)) {
      throw new IllegalArgumentException(
        s"Lean binary not found or not executable at path: ${settings.leanBinary}"
      )
    }
    val vampireBinaryPath = Paths.get(settings.vampireBinary)
    if (
      !Files.exists(vampireBinaryPath) || !Files.isExecutable(vampireBinaryPath)
    ) {
      throw new IllegalArgumentException(
        s"Vampire binary not found or not executable at path: ${settings.vampireBinary}"
      )
    }
    val leanLibraryPath = Paths.get(settings.leanLibraryPath)
    if (!Files.isDirectory(leanLibraryPath)) {
      throw new IllegalArgumentException(
        s"Lean library path is not a directory: ${settings.leanLibraryPath}"
      )
    }
    val tptpDirectoryPath = Paths.get(settings.tptpDirectory)
    if (!Files.isDirectory(tptpDirectoryPath)) {
      throw new IllegalArgumentException(
        s"TPTP directory is not a directory: ${settings.tptpDirectory}"
      )
    }
  }

  def killChildProcesses(): Unit = {
    val currentProcess = ProcessHandle.current()
    // Get a stream of all descendants (children, grandchildren, etc.)
    currentProcess.descendants().forEach { handle =>
      if (handle.isAlive) {
        Logger.println(
          s"Killing child process PID: ${handle.pid()} -> ${handle.info().command().orElse("Unknown")}"
        )
        handle.destroyForcibly() // Equivalent to 'kill -9'
      }
    }
  }

  private def getCorrectPath(
      path: String,
      baseDir: String,
      settings: Settings
  ): Path = {
    var fullPath = Paths.get(path)
    if (!fullPath.isAbsolute) {
      val alternativePath = Paths.get(baseDir, path)
      if (
        Files.exists(alternativePath) && Files.isRegularFile(alternativePath)
      ) {
        fullPath = alternativePath
      } else {
        val alternativePath2 = Paths.get(settings.tptpDirectory, path)
        if (
          Files
            .exists(alternativePath2) && Files.isRegularFile(alternativePath2)
        ) {
          fullPath = alternativePath2
        } else {
          throw new IllegalArgumentException(
            s"Input file not found: $path, tried baseDir: $baseDir and TPTP directory: ${settings.tptpDirectory}"
          )
        }
      }
    }
    fullPath
  }

  private def loadAnnotatedFormulas(
      path: String,
      baseDir: String,
      settings: Settings
  ): Seq[TPTP.AnnotatedFormula] = {

    val fullPath = getCorrectPath(path, baseDir, settings)
    val source = Source.fromFile(fullPath.toFile)
    var allFormulas = Seq.empty[TPTP.AnnotatedFormula]
    try {
      val problem = TPTPParser.problem(source)
      for (includes <- problem.includes) {
        var formulas = loadAnnotatedFormulas(includes._1, baseDir, settings)
        allFormulas = allFormulas ++ formulas
      }
      allFormulas ++ problem.formulas
    } finally {
      source.close()
    }
  }

  private def addInferencesIfSyntacticMismatch(
      dag: ProofDag.Dag,
      problemFormulas: Seq[TPTP.AnnotatedFormula],
      allowSyntacticMismatchOfAxioms: Boolean
  ): (ProofDag.Dag, Seq[TPTPProblemGenerator.Inference]) = {
    val (updatedDag, newObligations) =
      ProofRewriter.addInferencesIfSyntacticMismatch(
        dag,
        problemFormulas,
        allowSyntacticMismatchOfAxioms
      )
    (updatedDag, newObligations)
  }

  private def collectTheoremInferences(
      dag: ProofDag.Dag,
      assumeThm: Boolean
  ): Seq[TPTPProblemGenerator.Inference] = {
    val theoremNodes = dag.nodes.values.filter { node =>
      AnnotationInformationHelpers.isThm(
        node.additionalInfo,
        assumeThm
      ) && (node.role == "plain")
    }.toSeq

    theoremNodes.map { theoremNode =>
      val theoremParents = theoremNode.parents.map(dag.nodes)
      TPTPProblemGenerator.Inference(
        theoremNode.name,
        theoremParents.map(_.formula),
        theoremNode.formula
      )
    }
  }

  def runApp(config: Config): Unit = {
    Logger.setVerbose(config.verbosity)
    val stackSizeInBytes = 4 * 1024 * 1024 // 4MB, adjust as needed
    val threadFactory = new ThreadFactory {
      private val threadNumber = new AtomicInteger(1)
      private val group = Thread.currentThread().getThreadGroup

      override def newThread(r: Runnable): Thread = {
        // Thread constructor: (ThreadGroup, Runnable, Name, StackSize)
        val t = new Thread(
          group,
          r,
          s"custom-pool-thread-${threadNumber.getAndIncrement()}",
          stackSizeInBytes
        )
        if (t.isDaemon) t.setDaemon(false)
        if (t.getPriority != Thread.NORM_PRIORITY)
          t.setPriority(Thread.NORM_PRIORITY)
        t
      }
    }

    val executionContext = ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(config.parallel, threadFactory)
    )
    try {
      Logger.println(s"Reading input from: ${config.inputFile}")
      Logger.println("Parsing input proof file...")
      val proofPath = Paths.get(config.inputFile)
      val proofFileBasePath = Option(proofPath.getParent)
        .getOrElse(Paths.get("."))
        .toAbsolutePath
        .normalize
        .toString()
      val proofFileName = proofPath.getFileName.toString
      val proofFormulas =
        loadAnnotatedFormulas(proofFileName, proofFileBasePath, settings)

      Logger.println("Building proof DAG...")
      var dag = ProofDag.fromProof(
        proofFormulas,
        config.assumeThm,
        config.treatNegatedConjectureAsAxiom
      )

      var formulaFileInfos = dag.sources
        .map(source => {
          AnnotationInformationHelpers.fileParentInformation(
            dag.nodes(source).additionalInfo
          ) match {
            case Some(fileInfo) =>
              Some((source, fileInfo.fileName, fileInfo.formulaName))
            case _ => None
          }
        })
        .filter(_.isDefined)
        .map(_.get)
      var problemFiles = formulaFileInfos.map(_._2).distinct

      Logger.println(
        "Parse referenced problem files: " + problemFiles.mkString(", ")
      )
      Logger.println("Parsing input problem file...")

      var problemFormulas: Seq[TPTP.AnnotatedFormula] = Seq.empty;
      var translationJobs = Seq.empty[JobScheduler.JobSpec]
      if (problemFiles.nonEmpty) {
        for (problemFile <- problemFiles) {
          var formulas =
            loadAnnotatedFormulas(problemFile, proofFileBasePath, settings)
          problemFormulas = problemFormulas ++ formulas
          var correctPath =
            getCorrectPath(problemFile, proofFileBasePath, settings)
          var translationJob =
            TPTPProblemGenerator.buildVampireJobSpec(
              correctPath.toString(),
              proofFileBasePath,
              settings.vampireBinary,
              settings.tptpDirectory
            )
          translationJobs = translationJobs :+ translationJob
        }
      }

      val translationResultsFuture = JobScheduler.runFutures(translationJobs)(
        executionContext
      )

      Logger.println("Performing basic checks on proof DAG...")
      BasicChecks.performAllBasicChecks(
        dag,
        problemFormulas,
        config.allowSyntacticMismatchOfAxioms
      )

      

      Logger.println("Waiting for Vampire translation jobs to complete...")
      val translationResult = Await.result(
        translationResultsFuture,
        scala.concurrent.duration.Duration.Inf
      )

      Logger.println(
        translationResult.map(_.stdout).mkString("\n"),
        verbosity = Logger.VERBOSITY_MEDIUM
      )

      var translatedVariables: String = ""
      for (result <- translationResult) {
        val parsedTranslation =
          TranslationResult.parseTranslationResult(result)
        translatedVariables += parsedTranslation.variableDeclarations + "\n"
      }

      Logger.println(
        "Checking for syntactic mismatches between proof DAG and input problem..."
      )
      val res = addInferencesIfSyntacticMismatch(
        dag,
        problemFormulas,
        config.allowSyntacticMismatchOfAxioms
      )
      dag = res._1
      val additionalProofObligations = res._2

      Logger.println("Extracting skolemization details from proof DAG...")
      val skolemFunctionArities =
        SkolemizationGeneration.checkSkolemizationDetailsAreConsistent(dag)

      if (config.output.nonEmpty) {
        val renderedDagAfterAdditionalObligations = dag.toDot
        val writer = new PrintWriter(new FileWriter(config.output))
        try {
          writer.write(renderedDagAfterAdditionalObligations)
        } finally {
          writer.close()
        }
        Logger.println(
          s"Proof DAG after adding additional proof obligations written to: ${config.output}"
        )
      }

      val theoremInferences = collectTheoremInferences(dag, config.assumeThm)
      Logger.println(
        s"Checking ${theoremInferences.size} theorem inferences with Vampire..."
      )
      val theoremCheckResultsFuture =
        JobScheduler.runNodes(theoremInferences)(x =>
          TPTPProblemGenerator.buildTheoremCheckJobSpec(
            x,
            settings.vampireBinary,
            settings.tptpDirectory,
            config.timeout
          )
        )(executionContext)

      val additionalObligationCheckResultsFuture = JobScheduler.runNodes(
        additionalProofObligations
      )(x =>
        TPTPProblemGenerator.buildTheoremCheckJobSpec(
          x,
          settings.vampireBinary,
          settings.tptpDirectory,
          config.timeout
        )
      )(executionContext)

      val theoremCheckResults = Await.result(
        theoremCheckResultsFuture,
        scala.concurrent.duration.Duration.Inf
      )
      val additionalObligationCheckResults = Await.result(
        additionalObligationCheckResultsFuture,
        scala.concurrent.duration.Duration.Inf
      )

      val nonRefutedNodes = theoremCheckResults
        .filter { case (_, result) =>
          !result.stdout.contains("-- Termination reason: Refutation")
        }
        .keys
        .toSeq

      val additionalObligationCheckFailures = additionalObligationCheckResults
        .filter { case (_, result) =>
          !result.stdout.contains("-- Termination reason: Refutation")
        }
        .keys
        .toSeq

      if (additionalObligationCheckFailures.nonEmpty) {
        for (node <- additionalObligationCheckFailures) {
          Logger.println(
            s"Additional proof obligation node $node did not produce a refutation. Input was:",
            verbosity = Logger.VERBOSITY_LOW
          )
          Logger.println(
            additionalObligationCheckResults(node).stdin,
            verbosity = Logger.VERBOSITY_LOW
          )
          Logger.println(
            s"Vampire output was:",
            verbosity = Logger.VERBOSITY_LOW
          )
          Logger.println(
            additionalObligationCheckResults(node).stdout,
            verbosity = Logger.VERBOSITY_LOW
          )
        }
        throw new ProofErrorException(
          s"Some additional proof obligations did not produce a refutation (are possibly unsound): ${additionalObligationCheckFailures.mkString(", ")}"
        )
      }
      if (nonRefutedNodes.nonEmpty) {
        for (node <- nonRefutedNodes) {
          Logger.println(
            s"Theorem node $node did not produce a refutation. Input was:",
            verbosity = Logger.VERBOSITY_LOW
          )
          Logger.println(
            theoremCheckResults(node).stdin,
            verbosity = Logger.VERBOSITY_LOW
          )
          Logger.println(
            s"Vampire output was:",
            verbosity = Logger.VERBOSITY_LOW
          )
          Logger.println(
            theoremCheckResults(node).stdout,
            verbosity = Logger.VERBOSITY_LOW
          )
        }
        throw new ProofErrorException(
          s"Some theorem nodes did not produce a refutation (are possibly unsound): ${nonRefutedNodes.mkString(", ")}"
        )
      }

      Logger.println("Assembling Lean input file(s)...")

      val usedParents = theoremCheckResults.map { case (name, result) =>
        name -> result.usedParents
      }

      var compiledWithMultipleFiles = config.compileWithMultipleLeanFiles
      val combinedFileSize = theoremCheckResults.values.map(_.stdout.length).sum
      val numTheoremNodes =
        theoremCheckResults.size + additionalObligationCheckResults.size
      if (combinedFileSize > config.autoSwitchToMultiThreshold) {
        if (config.parallel > 1) {
          Logger.println(
            s"Combined size of theorem outputs ($combinedFileSize) exceeds auto-switch threshold"
          )
          Logger.println(s"Switching to compiling with multiple Lean files")
          compiledWithMultipleFiles = true
        }
      }
      if (compiledWithMultipleFiles) {
        val outputDir: Path = if (config.pathForLeanOutput.isDefined) {
          var dir = Paths.get(config.pathForLeanOutput.get)
          if (!Files.isDirectory(dir)) {
            val filename = dir.getFileName()
            // strip filename of extension and make it a directory
            val dirName = filename.toString().split('.').head
            dir =
              Option(dir.getParent).getOrElse(Paths.get(".")).resolve(dirName)
          }
          if (!Files.exists(dir)) {
            Files.createDirectories(dir)
          }
          dir
        } else {
          Files.createTempDirectory("lean_output_")
        }
        val files = LeanFullProofPrinter.writeLeanOutputFiles(
          outputDir,
          translatedVariables,
          theoremCheckResults,
          additionalObligationCheckResults,
          dag,
          skolemFunctionArities,
          usedParents,
          config.batchSizeLeanFiles
        )
        if (config.verifyWithLean) {
          Logger.println("Try checking with lean...")
          val leanCheckFuture = LeanRunner.compileMultipleFiles(
            files,
            config.parallel,
            settings.leanBinary,
            settings.leanLibraryPath
          )(executionContext)
          val leanCheckOutput = Await.result(
            leanCheckFuture,
            scala.concurrent.duration.Duration.Inf
          )
          LeanRunner.checkLeanResult(leanCheckOutput)
          if (config.pathForLeanOutput.isEmpty) {
            Logger.println("Deleting temporary output files...")
            Files.delete(files._1)
          }
        }
      } else {
        val outputFile: Path = if (config.pathForLeanOutput.isDefined) {

          Paths.get(config.pathForLeanOutput.get)
        } else {
          Files.createTempFile("lean_output_", ".lean")
        }

        LeanFullProofPrinter.writeLeanOutputFile(
          outputFile,
          translatedVariables,
          theoremCheckResults,
          additionalObligationCheckResults,
          dag,
          skolemFunctionArities,
          usedParents
        )
        if (config.verifyWithLean) {
          val leanCheckFuture = LeanRunner.runLeanCheck(
            outputFile,
            config.parallel,
            settings.leanBinary,
            settings.leanLibraryPath
          )(executionContext)
          val leanCheckResult = Await.result(
            leanCheckFuture,
            scala.concurrent.duration.Duration.Inf
          )
          if (config.pathForLeanOutput.isEmpty) {
            Files.delete(outputFile)
          }
          LeanRunner.checkLeanResult(leanCheckResult)
        }
      }
      println(s"%SZS status VerifiedGood")
    } catch {
      case e: ProofErrorException =>
        println(s"%SZS status VerifiedBad : ${e.getMessage}")
      case e: ProofUnsureException =>
        println(s"%SZS status Unknown : ${e.getMessage}")
      case e: IllegalArgumentException =>
        println(s"%SZS status Unknown : Usage error: ${e.getMessage}")
      case e: Exception =>
        println(s"%SZS status Unknown : ${e.getMessage}")
    } finally {
      Logger.println(
        "Shutting down execution context and killing child processes..."
      )
      executionContext.shutdownNow()
      killChildProcesses()
      executionContext.close()
    }
  }
}
