import leo.datastructures.TPTP
import java.nio.file.Paths

object TPTPProblemGenerator {
  case class Inference(
      name: String,
      premises: Seq[TPTP.AnnotatedFormula],
      conclusion: TPTP.AnnotatedFormula
  )

  private def stringFromFormulaWithRole(
      formula: TPTP.AnnotatedFormula,
      role: String
  ): String = formula match {
    case TPTP.FOFAnnotated(name, _, form, _) =>
      (TPTP.FOFAnnotated(name, role, form, None)).pretty
    case TPTP.CNFAnnotated(name, _, form, _) =>
      TPTP.FOFAnnotated(name, role, cnfStatementToFOF(form), None).pretty
    case _ =>
      throw new ProofUnsureException(
        s"Unsupported formula type for ${formula.getClass}"
      )
  }

  def generateProblemFromInference(inference: Inference): String = {
    val strBuilder = new StringBuilder
    for (parent <- inference.premises) {
      val premise = stringFromFormulaWithRole(parent, "axiom") + "\n"
      strBuilder.append(premise)
    }
    val conjecture =
      stringFromFormulaWithRole(inference.conclusion, "conjecture") + "\n"
    strBuilder.append(conjecture)
    return strBuilder.toString()
  }

  // This code comes from: https://github.com/leoprover/tptp-utils/blob/master/tptp-utils-runtime/src/main/scala/leo/modules/tptputils/SyntaxTransform.scala
  // which is available under the MIT License,
  // if needed the full tptp-utils repository may be used in the future

  final def cnfStatementToFOF(
      statement: TPTP.CNF.Statement
  ): TPTP.FOF.Statement = {
    import TPTP.{CNF, FOF}
    statement match {
      case CNF.Logical(formula) => FOF.Logical(cnfLogicFormulaToFOF(formula))
    }
  }

  type CNFFreeVars = Set[String]
  final def cnfLogicFormulaToFOF(
      formula: TPTP.CNF.Formula
  ): TPTP.FOF.Formula = {
    import TPTP.FOF
    formula match {
      case Seq() =>
        FOF.AtomicFormula(
          "$false",
          Seq.empty
        ) // Should never happen, but just to be on the safe side
      case _ =>
        val (transformedLiterals, freeVars) =
          mapAndAccumulate(formula, cnfLiteralToFOF)
        val intermediate =
          transformedLiterals.reduceRight(FOF.BinaryFormula(FOF.|, _, _))
        if (freeVars.isEmpty) intermediate
        else FOF.QuantifiedFormula(FOF.!, freeVars.toSeq, intermediate)
    }
  }

  final def cnfLiteralToFOF(
      literal: TPTP.CNF.Literal
  ): (TPTP.FOF.Formula, CNFFreeVars) = {
    import TPTP.{CNF, FOF}
    literal match {
      case CNF.PositiveAtomic(CNF.AtomicFormula(f, args)) =>
        val (translatedArgs, freeVars) = mapAndAccumulate(args, cnfTermToFOF)
        (FOF.AtomicFormula(f, translatedArgs), freeVars)
      case CNF.NegativeAtomic(CNF.AtomicFormula(f, args)) =>
        val (translatedArgs, freeVars) = mapAndAccumulate(args, cnfTermToFOF)
        (
          FOF.UnaryFormula(FOF.~, FOF.AtomicFormula(f, translatedArgs)),
          freeVars
        )
      case CNF.Equality(left, right) =>
        val (translatedLeft, fvsLeft) = cnfTermToFOF(left)
        val (translatedRight, fvsRight) = cnfTermToFOF(right)
        (FOF.Equality(translatedLeft, translatedRight), fvsLeft union fvsRight)
      case CNF.Inequality(left, right) =>
        val (translatedLeft, fvsLeft) = cnfTermToFOF(left)
        val (translatedRight, fvsRight) = cnfTermToFOF(right)
        (
          FOF.Inequality(translatedLeft, translatedRight),
          fvsLeft union fvsRight
        )
    }
  }

  final def cnfTermToFOF(term: TPTP.CNF.Term): (TPTP.FOF.Term, CNFFreeVars) = {
    import TPTP.{CNF, FOF}
    term match {
      case CNF.AtomicTerm(f, args) =>
        val (translatedArgs, freeVars) = mapAndAccumulate(args, cnfTermToFOF)
        (FOF.AtomicTerm(f, translatedArgs), freeVars)
      case CNF.Variable(name)       => (FOF.Variable(name), Set(name))
      case CNF.DistinctObject(name) => (FOF.DistinctObject(name), Set.empty)
    }
  }

  private[this] final def mapAndAccumulate[A, B, C](
      list: Seq[A],
      f: A => (B, Set[C])
  ): (Seq[B], Set[C]) = {
    var mapResult: Seq[B] = Seq.empty
    var accResult: Set[C] = Set.empty
    list foreach { x =>
      val fResult = f(x)
      mapResult = mapResult :+ fResult._1
      accResult = accResult union fResult._2
    }
    (mapResult, accResult)
  }

  final def buildVampireJobSpec(
      inputProblemFile: String,
      proofFileBasePath: String,
      vampireBinary: String,
      tptpPath: String
  ): JobScheduler.JobSpec = {
    var fullPath = Paths.get(inputProblemFile)
    if (!fullPath.isAbsolute) {
      fullPath = Paths.get(proofFileBasePath, inputProblemFile)
    }
    JobScheduler.JobSpec(
      Seq(
        vampireBinary,
        fullPath.toString(),
        "--mode",
        "translate",
        "--output_axiom_names",
        "on"
      ),
      env = Map("TPTP" -> tptpPath)
    )
  }

  final def buildTheoremCheckJobSpec(
      inference: Inference,
      vampireBinary: String,
      tptpPath: String
  ): JobScheduler.JobSpec = {
    val command = Seq(
      vampireBinary,
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
      "-t",
      "0"
    )
    val problemText =
      TPTPProblemGenerator.generateProblemFromInference(inference)
    JobScheduler.JobSpec(
      command,
      stdin = problemText,
      env = Map("TPTP" -> tptpPath)
    )
  }
}
