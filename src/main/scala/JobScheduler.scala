import java.io.File
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import ProofDag._

object JobScheduler {
  final case class JobSpec(cmd: Seq[String], stdin: String = "", env: Map[String, String] = Map.empty, cwd: Option[File] = None, timeout: Option[FiniteDuration] = None)

  final case class ProcessResult(exitCode: Int, stdin: String, stdout: String, stderr: String, durationMillis: Long, timedOut: Boolean, jobSpec: JobSpec = null, usedParents: Seq[String] = Seq.empty)

  private def runProcess(spec: JobSpec): ProcessResult = {
    val tIn = Files.createTempFile("valeadeate-in-", ".tmp")
    val tOut = Files.createTempFile("valeadeate-out-", ".tmp")
    val tErr = Files.createTempFile("valeadeate-err-", ".tmp")

    try {
      Files.write(tIn, spec.stdin.getBytes(StandardCharsets.UTF_8))

      val pb = new ProcessBuilder(spec.cmd: _*)
      pb.redirectInput(tIn.toFile)
      pb.redirectOutput(tOut.toFile)
      pb.redirectError(tErr.toFile)
      spec.env.foreach { case (k, v) => pb.environment().put(k, v) }
      spec.cwd.foreach(pb.directory)

      val start = System.currentTimeMillis()
      val p = pb.start()

      val timedOut = spec.timeout match {
        case Some(dur) => !p.waitFor(dur.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
        case None => try { p.waitFor(); false } catch { case _: InterruptedException => true }
      }

      if (timedOut) {
        try p.destroyForcibly()
        catch { case _: Throwable => () }
      }

      val exit = try p.exitValue() catch { case _: IllegalThreadStateException => -1 }
      val duration = System.currentTimeMillis() - start
      val stdout = new String(Files.readAllBytes(tOut), StandardCharsets.UTF_8)
      val stderr = new String(Files.readAllBytes(tErr), StandardCharsets.UTF_8)

      ProcessResult(exit, spec.stdin, stdout, stderr, duration, timedOut, spec)
    } finally {
      try Files.deleteIfExists(tIn) catch { case _: Throwable => () }
      try Files.deleteIfExists(tOut) catch { case _: Throwable => () }
      try Files.deleteIfExists(tErr) catch { case _: Throwable => () }
    }
  }

  /**
    * Run jobs for the given nodes in parallel.
    * The `jobBuilder` receives the node and its parents.
    * Returns a Future completing with a map from node name -> ProcessResult when all specified nodes are done.
    */
  def runNodes(inferencesToRun: Seq[TPTPProblemGenerator.Inference])(jobBuilder: (TPTPProblemGenerator.Inference) => JobSpec)(implicit ec: ExecutionContext): Future[Map[String, ProcessResult]] = {
    val jobFutures = inferencesToRun.map { inference =>
      Future {
        val spec = jobBuilder(inference)
        val res = runProcess(spec)
        //extract the list of used parents by scanning the stdout for lines: -- VaLeaDATE info: 1 2
        val usedParents = res.stdout.linesIterator.filter(_.startsWith("-- VaLeaDATE info:")).flatMap { line =>
          line.stripPrefix("-- VaLeaDATE info:").trim.split("\\s+").filter(_.nonEmpty)
        }.toSeq
        (inference.name, res.copy(usedParents = usedParents.map(parentNo => AnnotatedFormulaHelpers.sanitizeName(inference.premises(parentNo.toInt - 1).name))))
      }
    }
    Future.sequence(jobFutures).map(_.toMap)
  }

  def runFuture(spec: JobSpec)(implicit ec: ExecutionContext): Future[ProcessResult] = Future {
    runProcess(spec)
  }

  def runFutures(specs: Seq[JobSpec])(implicit ec: ExecutionContext): Future[Seq[ProcessResult]] = Future.sequence(specs.map(runFuture))
  
}
