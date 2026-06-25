import leo.datastructures.TPTP

object LeanPrettyPrinter {
  var variablesAsTemplates: Boolean = false
  
  def prettyLeanSyntax(value: TPTP.AnnotatedFormula): String = value match {
    case TPTP.FOFAnnotated(_, _, formula, _) => prettyLeanFOFStatement(formula)
    case TPTP.CNFAnnotated(_, _, formula, _) => prettyLeanCNFStatement(formula)
    case _ => throw new IllegalArgumentException(s"Unsupported annotated formula type: ${value.getClass}")
  }

  def prettyLeanFOFStatement(statement: TPTP.FOF.Statement): String = statement match {
    case TPTP.FOF.Logical(formula) => prettyLeanFOFFormula(formula)
  }

  def prettyLeanFOFFormula(formula: TPTP.FOF.Formula): String = formula match {
    case TPTP.FOF.AtomicFormula(f, args) => prettyLeanApp(f, args.map(prettyLeanFOFTerm))
    case TPTP.FOF.QuantifiedFormula(q, variables, body) =>
      s"(${prettyLeanFOFQuantifier(q)} ${variables.map(prettyLeanUntypedVariable).mkString(" ")}, ${prettyLeanFOFFormula(body)})"
    case TPTP.FOF.UnaryFormula(connective, body) => s"${prettyLeanFOFUnary(connective)} ${prettyLeanFOFFormula(body)}"
    case TPTP.FOF.BinaryFormula(connective, left, right) =>
      s"(${prettyLeanFOFFormula(left)} ${prettyLeanFOFBinary(connective)} ${prettyLeanFOFFormula(right)})"
    case TPTP.FOF.Equality(left, right) => s"(${prettyLeanFOFTerm(left)} = ${prettyLeanFOFTerm(right)})"
    case TPTP.FOF.Inequality(left, right) => s"(${prettyLeanFOFTerm(left)} ≠ ${prettyLeanFOFTerm(right)})"
  }

  private def prettyLeanCNFStatement(statement: TPTP.CNF.Statement): String = statement match {
    case TPTP.CNF.Logical(formula) => prettyLeanCNFClause(formula)
  }

  private def prettyLeanCNFClause(clause: TPTP.CNF.Formula): String = clause.map(prettyLeanCNFLiteral).mkString(" ∨ ")

  private def prettyLeanCNFLiteral(literal: TPTP.CNF.Literal): String = literal match {
    case TPTP.CNF.PositiveAtomic(formula) => prettyLeanCNFAtomicFormula(formula)
    case TPTP.CNF.NegativeAtomic(formula) => s"¬ ${prettyLeanCNFAtomicFormula(formula)}"
    case TPTP.CNF.Equality(left, right) => s"(${prettyLeanCNFTerm(left)} = ${prettyLeanCNFTerm(right)})"
    case TPTP.CNF.Inequality(left, right) => s"(${prettyLeanCNFTerm(left)} ≠ ${prettyLeanCNFTerm(right)})"
  }

  private def prettyLeanCNFAtomicFormula(formula: TPTP.CNF.AtomicFormula): String =
    prettyLeanApp(formula.f, formula.args.map(prettyLeanCNFTerm))

  private def prettyLeanFOFTerm(term: TPTP.FOF.Term): String = term match {
    case TPTP.FOF.AtomicTerm(f, args) => prettyLeanApp(f, args.map(prettyLeanFOFTerm))
    case TPTP.FOF.Variable(name) => if(variablesAsTemplates) "_" else leanIdent(name)
    case TPTP.FOF.DistinctObject(name) => s"\"${name.drop(1).dropRight(1)}\""
    case TPTP.FOF.NumberTerm(value) => value.pretty
  }

  private def prettyLeanCNFTerm(term: TPTP.CNF.Term): String = term match {
    case TPTP.CNF.AtomicTerm(f, args) => prettyLeanApp(f, args.map(prettyLeanCNFTerm))
    case TPTP.CNF.Variable(name) => if(variablesAsTemplates) "_" else leanIdent(name)
    case TPTP.CNF.DistinctObject(name) => s"\"${name.drop(1).dropRight(1)}\""
  }

  private def prettyLeanUntypedVariable(name: String): String = if(variablesAsTemplates) s"_" else s"(${leanIdent(name)} : ι)"

  private def prettyLeanFOFQuantifier(quantifier: TPTP.FOF.Quantifier): String = quantifier match {
    case TPTP.FOF.! => "∀"
    case TPTP.FOF.? => "∃"
    case _ => throw new IllegalArgumentException(s"Unsupported quantifier in FOF: $quantifier")
  }

  private def prettyLeanFOFUnary(connective: TPTP.FOF.UnaryConnective): String = connective match {
    case TPTP.FOF.~ => "¬"
  }

  private def prettyLeanFOFBinary(connective: TPTP.FOF.BinaryConnective): String = connective match {
    case TPTP.FOF.<=> => "↔"
    case TPTP.FOF.Impl => "→"
    case TPTP.FOF.<= => "←"
    case TPTP.FOF.| => "∨"
    case TPTP.FOF.& => "∧"
    case _ => throw new IllegalArgumentException(s"Unsupported binary connective in FOF: $connective")
  }


  private def prettyLeanApp(name: String, args: Seq[String]): String = {
    val fun = leanIdent(name)
    if (args.isEmpty) fun else s"($fun ${args.mkString(" ")})"
  }

  private def leanIdent(name: String): String = {
    "«_" + name + "»"
  }

}