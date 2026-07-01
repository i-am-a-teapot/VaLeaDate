final case class TranslationResult(
    variableDeclarations: String,
    formulasByName: Map[String, String]
)

object TranslationResult {
  private def sectionBetween(text: String, beginMarker: String, endMarker: String): String = {
    text.split(beginMarker)(1).split(endMarker)(0).trim
  }
  
  def parseTranslationResult(res: JobScheduler.ProcessResult): TranslationResult = {
    //if(res.exitCode != 0) {
    //  throw new ProofUnsureException(
    //    s"Vampire translation failed with exit code ${res.exitCode}.\nstdout:\n${res.stdout}\nstderr:\n${res.stderr}"
    //  )
    //}
    val result = res.stdout
    val variableDeclarations = sectionBetween(result, "% BEGIN Variable Declarations", "% END Variable Declarations")
    val inputFormulas = sectionBetween(result, "% BEGIN Input Formulas", "% END Input Formulas\n")
    val inputFormulaLines = inputFormulas.split("\n").map(_.trim).filter(_.nonEmpty)

    Logger.println(inputFormulaLines.mkString("\n"))
    val formulasByIndex: Map[String, String] = inputFormulaLines.grouped(2).collect {
      case Array(header, formula) if header.startsWith("%") =>
        val headerParts = header.stripPrefix("%").split("%").map(_.trim)
        if (headerParts.length >= 2) {
          Some(AnnotatedFormulaHelpers.sanitizeName(headerParts(0).trim) -> formula)
        } else {
          None
        }
    }.flatten.toMap

    TranslationResult(variableDeclarations, formulasByIndex)
  }
}