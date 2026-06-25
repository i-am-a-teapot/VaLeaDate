import leo.datastructures.TPTP
import scala.io.Source
import leo.modules.input.TPTPParser

object ProblemParser {
    def parseProblemFileToMap(filePath: String, tptpBasePath: Option[String]): Map[String, TPTP.AnnotatedFormula] = {
      val sourceProblem = Source.fromFile(filePath)
      val parsedProblem = TPTPParser.problem(sourceProblem)
      sourceProblem.close()
      var formulaMap = Map.empty[String, TPTP.AnnotatedFormula]
      for(formula <- parsedProblem.formulas) {
        formulaMap += (formula.name -> formula) 
      }
      return formulaMap
    }
}