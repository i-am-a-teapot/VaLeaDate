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

  def transformFormulaToNNF(formula: TPTP.FOF.Formula): TPTP.FOF.Formula = {
    formula match {
      case TPTP.FOF.AtomicFormula(_, _)              => formula
      case TPTP.FOF.QuantifiedFormula(q, vars, body) =>
        TPTP.FOF.QuantifiedFormula(q, vars, transformFormulaToNNF(body))
      case TPTP.FOF.UnaryFormula(TPTP.FOF.~, body) =>
        body match {
          case TPTP.FOF.AtomicFormula(_, _)           => formula
          case TPTP.FOF.QuantifiedFormula(q, vars, b) =>
            val newQuant = if (q == TPTP.FOF.!) TPTP.FOF.? else TPTP.FOF.!
            val newBody = transformFormulaToNNF(
              TPTP.FOF.UnaryFormula(TPTP.FOF.~, b)
            )
            TPTP.FOF.QuantifiedFormula(newQuant, vars, newBody)
          case TPTP.FOF.UnaryFormula(TPTP.FOF.~, b) =>
            transformFormulaToNNF(b)
          case TPTP.FOF.BinaryFormula(connective, left, right) =>
            val newLeft = transformFormulaToNNF(
              TPTP.FOF.UnaryFormula(TPTP.FOF.~, left)
            )
            val newRight = transformFormulaToNNF(
              TPTP.FOF.UnaryFormula(TPTP.FOF.~, right)
            )
            connective match {
              case TPTP.FOF.| =>
                TPTP.FOF.BinaryFormula(TPTP.FOF.&, newLeft, newRight)
              case TPTP.FOF.& =>
                TPTP.FOF.BinaryFormula(TPTP.FOF.|, newLeft, newRight)
              case TPTP.FOF.~& =>
                TPTP.FOF.BinaryFormula(TPTP.FOF.&, left, right)
              case TPTP.FOF.~| =>
                TPTP.FOF.BinaryFormula(TPTP.FOF.|, left, right)
              case TPTP.FOF.Impl =>
                TPTP.FOF.BinaryFormula(TPTP.FOF.&, left, newRight)
              case TPTP.FOF.<= =>
                TPTP.FOF.BinaryFormula(TPTP.FOF.&, newLeft, right)
              case TPTP.FOF.<=> =>
                TPTP.FOF.BinaryFormula(
                  TPTP.FOF.|,
                  TPTP.FOF.BinaryFormula(TPTP.FOF.&, left, newRight),
                  TPTP.FOF.BinaryFormula(TPTP.FOF.&, newLeft, right)
                )
              case TPTP.FOF.<~> =>
                TPTP.FOF.BinaryFormula(
                  TPTP.FOF.|,
                  TPTP.FOF.BinaryFormula(TPTP.FOF.&, left, right),
                  TPTP.FOF.BinaryFormula(TPTP.FOF.&, newLeft, newRight)
                )
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

  sealed abstract class SymbolId(val name: String, val arity: Int)
  case class Pred(override val name: String, override val arity: Int)
    extends SymbolId(name, arity)
  case class Func(override val name: String, override val arity: Int)
    extends SymbolId(name, arity)

  def getSymbolsWithArity(formula: TPTP.FOF.Formula): Seq[SymbolId] = {
    formula match {
      case TPTP.FOF.AtomicFormula(pred, args) =>
        Seq(Pred(pred, args.size)) ++ args.flatMap(getSymbolsWithArity)
      case TPTP.FOF.QuantifiedFormula(_, _, body) =>
        getSymbolsWithArity(body)
      case TPTP.FOF.UnaryFormula(_, body) =>
        getSymbolsWithArity(body)
      case TPTP.FOF.BinaryFormula(_, left, right) =>
        getSymbolsWithArity(left) ++ getSymbolsWithArity(right)
      case TPTP.FOF.Equality(left, right) =>
        getSymbolsWithArity(left) ++ getSymbolsWithArity(right)
      case TPTP.FOF.Inequality(left, right) =>
        getSymbolsWithArity(left) ++ getSymbolsWithArity(right)
    }
  }
  def getSymbolsWithArity(term: TPTP.FOF.Term): Seq[SymbolId] = {
    term match {
      case TPTP.FOF.AtomicTerm(fun, args) =>
        Seq(Func(fun, args.size)) ++ args.flatMap(getSymbolsWithArity)
      case TPTP.FOF.Variable(_)       => Seq.empty
      case TPTP.FOF.NumberTerm(_)     => Seq.empty
      case TPTP.FOF.DistinctObject(_) => Seq.empty
    }
  }
  def getSymbolsWithArity(formula: TPTP.CNF.Formula): Seq[SymbolId] = {
    formula.flatMap(clause =>
      clause match {
        case TPTP.CNF.PositiveAtomic(formula) => getSymbolsWithArity(formula)
        case TPTP.CNF.NegativeAtomic(formula) => getSymbolsWithArity(formula)
        case TPTP.CNF.Equality(left, right)   =>
          getSymbolsWithArity(left) ++ getSymbolsWithArity(right)
        case TPTP.CNF.Inequality(left, right) =>
          getSymbolsWithArity(left) ++ getSymbolsWithArity(right)
      }
    )
  }
  def getSymbolsWithArity(term: TPTP.CNF.Term): Seq[SymbolId] = {
    term match {
      case TPTP.CNF.AtomicTerm(fun, args) =>
        Seq(Func(fun, args.size)) ++ args.flatMap(getSymbolsWithArity)
      case TPTP.CNF.Variable(_)       => Seq.empty
      case TPTP.CNF.DistinctObject(_) => Seq.empty
    }
  }
  def getSymbolsWithArity(
      atomicFormula: TPTP.CNF.AtomicFormula
  ): Seq[SymbolId] = {
    val funSym = atomicFormula.f
    val args = atomicFormula.args
    Seq(Func(funSym, args.size)) ++ args.flatMap(getSymbolsWithArity)
  }

  def getSymbolsWithArity(
      formula: TPTP.AnnotatedFormula
  ): Seq[SymbolId] = {
    formula match {
      case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(f), _) =>
        getSymbolsWithArity(f)
      case TPTP.CNFAnnotated(_, _, TPTP.CNF.Logical(f), _) =>
        getSymbolsWithArity(f)
      case _ =>
        throw new ProofUnsureException(
          "getSymbolsWithArity is only implemented for FOF and CNF formulas"
        )
    }
  }

  def isFalseFormula(formula: TPTP.AnnotatedFormula): Boolean = {
    formula match {
      case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(value), _) =>
        value match {
          case TPTP.FOF.AtomicFormula("$false", _) => true
          case _                                   => false
        }
      case TPTP.CNFAnnotated(_, _, TPTP.CNF.Logical(value), _) =>
        if (value.isEmpty) {
          true
        } else if (value.size == 1) {
          value.head match {
            case TPTP.CNF
                  .PositiveAtomic(TPTP.CNF.AtomicFormula("$false", _)) =>
              true
            case TPTP.CNF
                  .NegativeAtomic(TPTP.CNF.AtomicFormula("$true", _)) =>
              true
            case _ =>
              false
          }
        } else {
          false
        }
      case _ =>
        throw new ProofUnsureException(
          "isFalseFormula is only implemented for FOF and CNF formulas"
        )
    }
  }
}
