import leo.datastructures.TPTP

object AnnotatedFormulaHelpers {
  def gatherKeywordsInTerm(
      gt: TPTP.GeneralTerm,
      keywords: Set[String]
  ): Seq[TPTP.MetaFunctionData] = {
    def rec(term: TPTP.GeneralTerm): Seq[TPTP.MetaFunctionData] = {
      val fromList = term.list.toSeq.flatten.flatMap(rec)
      val fromData = term.data.toSeq.flatMap {
        case m @ TPTP.MetaFunctionData(f, _) if keywords.contains(f) => Seq(m)
        case TPTP.MetaFunctionData(_, args) => args.flatMap(rec)
        case _                              => Seq.empty
      }
      fromData ++ fromList
    }
    rec(gt)
  }

  def findSingleInference(
      annotation: TPTP.Annotations,
      keywords: Set[String] = Set("inference")
  ): Option[TPTP.MetaFunctionData] = {
    val all = annotation match {
      case Some((gt, _)) => gatherKeywordsInTerm(gt, keywords)
      case None          => Seq.empty
    }
    if (all.size > 1)
      throw new ProofUnsureException(
        s"Multiple sections from $keywords found in annotation"
      )
    all.headOption
  }

  def parentNamesFromTerm(term: TPTP.GeneralTerm): Seq[String] = {
    if (term.list.isDefined) Seq.empty
    else {
      term.data match {
        case Seq(TPTP.MetaFunctionData("inference", args)) if args.size >= 3 =>
          args(2).list.toSeq.flatten.flatMap(parentNamesFromTerm)
        case Seq(TPTP.MetaFunctionData(name, Seq())) => Seq(name)
        case Seq(TPTP.NumberData(number))            => Seq(number.pretty)
        case _                                       => Seq.empty
      }
    }
  }

  def sanitizeName(name: String): String =
    name
      .replace("(", "")
      .replace(")", "")
      .replace(" ", "_")
      .replace("'", "")
      .replace(",", "")

  def collectQuantifiedFormulaVariables(
      formula: TPTP.FOF.Formula,
      quantifier: TPTP.FOF.Quantifier
  ): Set[String] = {
    formula match {
      case TPTP.FOF.AtomicFormula(_, args)                    => Set.empty
      case TPTP.FOF.QuantifiedFormula(quant, variables, body) =>
        (if (quant == quantifier) variables.toSet
         else Set.empty) ++ collectQuantifiedFormulaVariables(body, quantifier)
      case TPTP.FOF.UnaryFormula(_, body) =>
        collectQuantifiedFormulaVariables(body, quantifier)
      case TPTP.FOF.BinaryFormula(_, left, right) =>
        collectQuantifiedFormulaVariables(
          left,
          quantifier
        ) ++ collectQuantifiedFormulaVariables(right, quantifier)
      case TPTP.FOF.Equality(left, right)   => Set.empty
      case TPTP.FOF.Inequality(left, right) => Set.empty
    }
  }

  def checkFormulaIsInNNF(formula: TPTP.FOF.Formula): Boolean = {
    formula match {
      case TPTP.FOF.AtomicFormula(_, _)            => true
      case TPTP.FOF.QuantifiedFormula(_, _, body)  => checkFormulaIsInNNF(body)
      case TPTP.FOF.UnaryFormula(TPTP.FOF.~, body) =>
        body match {
          case TPTP.FOF.AtomicFormula(_, _)        => true
          case TPTP.FOF.QuantifiedFormula(_, _, _) => false
          case TPTP.FOF.UnaryFormula(_, _)         => false
          case TPTP.FOF.BinaryFormula(_, _, _)     => false
          case TPTP.FOF.Equality(_, _)             => true
          case TPTP.FOF.Inequality(_, _)           => true
        }
      case TPTP.FOF.BinaryFormula(_, left, right) =>
        checkFormulaIsInNNF(left) && checkFormulaIsInNNF(right)
      case TPTP.FOF.Equality(_, _)   => true
      case TPTP.FOF.Inequality(_, _) => true
    }
  }

  def transformFormulaToNNF(formula: TPTP.FOF.Formula) : TPTP.FOF.Formula = {
    formula match {
      case TPTP.FOF.AtomicFormula(_, _)            => formula
      case TPTP.FOF.QuantifiedFormula(q, vars, body) =>
        TPTP.FOF.QuantifiedFormula(q, vars, transformFormulaToNNF(body))
      case TPTP.FOF.UnaryFormula(TPTP.FOF.~, body) =>
        body match {
          case TPTP.FOF.AtomicFormula(_, _)        => formula
          case TPTP.FOF.QuantifiedFormula(q, vars, b) =>
            val newQuant = if (q == TPTP.FOF.!) TPTP.FOF.? else TPTP.FOF.!
            val newBody = transformFormulaToNNF(TPTP.FOF.UnaryFormula(TPTP.FOF.~, b))
            TPTP.FOF.QuantifiedFormula(newQuant, vars, newBody)
          case TPTP.FOF.UnaryFormula(TPTP.FOF.~ , b) =>
            transformFormulaToNNF(b)
          case TPTP.FOF.BinaryFormula(connective, left, right) =>
            val newLeft = transformFormulaToNNF(TPTP.FOF.UnaryFormula(TPTP.FOF.~ , left))
            val newRight = transformFormulaToNNF(TPTP.FOF.UnaryFormula(TPTP.FOF.~ , right))
            connective match {
              case TPTP.FOF.| => TPTP.FOF.BinaryFormula(TPTP.FOF.&, newLeft, newRight)
              case TPTP.FOF.& => TPTP.FOF.BinaryFormula(TPTP.FOF.|, newLeft, newRight)
              case TPTP.FOF.Impl => TPTP.FOF.BinaryFormula(TPTP.FOF.&, left, newRight)
              case TPTP.FOF.<= => TPTP.FOF.BinaryFormula(TPTP.FOF.&, newLeft, right)
              case TPTP.FOF.<=> => TPTP.FOF.BinaryFormula(TPTP.FOF.|, TPTP.FOF.BinaryFormula(TPTP.FOF.&, left, newRight), TPTP.FOF.BinaryFormula(TPTP.FOF.&, newLeft, right))
              case TPTP.FOF.<~> => TPTP.FOF.BinaryFormula(TPTP.FOF.|, TPTP.FOF.BinaryFormula(TPTP.FOF.&, left, right), TPTP.FOF.BinaryFormula(TPTP.FOF.&, newLeft, newRight))
            }
          case TPTP.FOF.Equality(_, _)   => formula
          case TPTP.FOF.Inequality(_, _) => formula
        }
      case TPTP.FOF.BinaryFormula(connective, left, right) =>
        val newLeft = transformFormulaToNNF(left)
        val newRight = transformFormulaToNNF(right)
        TPTP.FOF.BinaryFormula(connective, newLeft, newRight)
      case TPTP.FOF.Equality(_, _)   => formula
      case TPTP.FOF.Inequality(_, _) => formula
    }
  }
}
