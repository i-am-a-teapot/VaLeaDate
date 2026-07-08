import java.nio.file.{Files, Path, Paths}
import scala.concurrent.{ExecutionContext, Future}

object LeanRunner {

  def compileMultipleFiles(
      outputFile: (Path, Seq[Path], Path),
      parallelism: Int,
      leanBinary: String,
      leanLibraryPath: String
  )(implicit ec: ExecutionContext): Future[JobScheduler.ProcessResult] = {
    val (dataFile, batchFiles, fullProofFile) = outputFile
    val baseDir = dataFile.getParent.toAbsolutePath.toString
    val compileDir = Path.of(baseDir, "build")

    if (Files.exists(compileDir)) {
      Logger.println(
        s"Cleaning up existing compile directory: ${compileDir.toAbsolutePath.toString}"
      )
      Files
        .walk(compileDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(path => Files.delete(path))
      Files.createDirectories(compileDir)
    } else {
      Files.createDirectories(compileDir)
    }

    val leanCompileData = JobScheduler.JobSpec(
      Seq(
        leanBinary,
        "-j",
        parallelism.toString,
        "-o",
        Path.of(compileDir.toString, "data.olean").toString,
        "-R",
        baseDir,
        dataFile.toString()
      ),
      "",
      Map("LEAN_PATH" -> leanLibraryPath)
    )

    val dataFuture = JobScheduler.runFuture(leanCompileData)

    dataFuture.flatMap { dataResult =>
      if (dataResult.exitCode != 0) {
        Future.successful(dataResult)
      } else {
        Logger.println("First file compiled, now compiling batched files...")
        val compileBatchFutures = batchFiles.map { batchFile =>
          val leanCompileBatch = JobScheduler.JobSpec(
            Seq(
              leanBinary,
              "-j",
              "1",
              "-o",
              Path
                .of(
                  compileDir.toString,
                  batchFile
                    .getFileName()
                    .toString()
                    .stripSuffix(".lean") + ".olean"
                )
                .toString,
              "-R",
              baseDir,
              batchFile.toString()
            ),
            "",
            Map(
              "LEAN_PATH" -> (leanLibraryPath + ":" + compileDir.toAbsolutePath
                .toString())
            )
          )
          JobScheduler.runFuture(leanCompileBatch)
        }

        Future.sequence(compileBatchFutures).flatMap { batchResults =>
          if (batchResults.forall(_.exitCode == 0)) {
            Logger.println("All batch files compiled successfully.")
          } else {
            
            Logger.println(
              "Some batch files failed to compile. Check the output for details."
            )
          }

          val leanCompileFullProof = JobScheduler.JobSpec(
            Seq(
              leanBinary,
              "-j",
              "1",
              "-o",
              compileDir.resolve("full_proof.olean").toString,
              "-R",
              baseDir,
              fullProofFile.toString()
            ),
            "",
            Map(
              "LEAN_PATH" -> (leanLibraryPath + ":" + compileDir.toAbsolutePath
                .toString())
            )
          )

          JobScheduler.runFuture(leanCompileFullProof)
        }
      }
    }
  }

  def runLeanCheck(
      outputFile: Path,
      parallelism: Int,
      leanBinary: String,
      leanLibraryPath: String
  )(implicit ec: ExecutionContext): Future[JobScheduler.ProcessResult] = {
    val leanCheckSpec = JobScheduler.JobSpec(
      Seq(
        leanBinary,
        "-j",
        parallelism.toString,
        outputFile.toString()
      ),
      "",
      Map("LEAN_PATH" -> leanLibraryPath)
    )
    JobScheduler.runFuture(leanCheckSpec)
  }

  def checkLeanResult(leanCheckResult: JobScheduler.ProcessResult): Unit = {
    Logger.println(leanCheckResult.durationMillis)
    if (leanCheckResult.exitCode != 0) {
      Logger.println(leanCheckResult.stdout)
      Logger.println(leanCheckResult.stderr)
    }
    if (leanCheckResult.exitCode == 1) {
      throw new ProofErrorException(
        s"Lean check failed with exit code ${leanCheckResult.exitCode}."
      )
    }
    if (leanCheckResult.exitCode != 0) {
      throw new ProofUnsureException(
        s"Lean check failed with exit code ${leanCheckResult.exitCode}."
      )
    }
  }
}
