import leo.datastructures.TPTP
object ProofRewriter {

  // Returns (updatedDag, newObligations)
  def rewriteProofAxiomIfNotEqToInputProb(
      dag: ProofDag.Dag,
      nodeName: String,
      problemFormulas: Seq[TPTP.AnnotatedFormula]
  ): (ProofDag.Dag, Seq[TPTPProblemGenerator.Inference]) = {
    val node = dag.nodes(nodeName)
    val problemFormulaName = AnnotationInformationHelpers.fileParentInformation(
      node.additionalInfo
    ) match {
      case Some(FileInformation(_, name)) => name
      case _                              =>
        throw new ProofErrorException(
          s"Expected FileInformation for axiom node $nodeName in DAG"
        )
    }

    val problemFormula = problemFormulas.find(_.name == problemFormulaName)
    val isSyntacticallyEqual =
      problemFormula.exists(formulasAreSyntacticallyEqual(node.formula, _))
    if (isSyntacticallyEqual) {
      (dag, Seq.empty)
    } else {
      Logger.println("Syntactic mismatch found for axiom node: " + nodeName)
      val replacementNodeName = nodeName + "_inputTransf"
      val replacementFormula =
        problemFormula match {
          case Some(TPTP.CNFAnnotated(name, role, form, annotations)) =>
            TPTP.FOFAnnotated(
              replacementNodeName,
              role,
              TPTPProblemGenerator.cnfStatementToFOF(form),
              annotations
            )
          case Some(TPTP.FOFAnnotated(name, role, form, annotations)) =>
            TPTP.FOFAnnotated(replacementNodeName, role, form, annotations)
          case what => {
            Logger.println(what)
            throw new ProofUnsureException(
              s"Expected CNF or FOF formula for problem formula $problemFormulaName"
            )
          }
        }

      val replacementNode = ProofDag.Node(
        replacementNodeName,
        "axiom",
        replacementFormula,
        node.additionalInfo
      )

      val newObligation = TPTPProblemGenerator.Inference(
        "rev_obligation_axiom_" + node.name,
        Seq(node.formula),
        replacementFormula
      )
      Logger.println(
        s"Added additional proof obligation for axiom node $nodeName: ${replacementFormula.name}"
      )

      var updatedNodes = dag.nodes
      val rewrittenNode = updatedNodes(node.name).copy(
        role = "plain",
        additionalInfo = InferenceInformation(
          "thm",
          "thm",
          Seq(NamedParentInformation(replacementNodeName))
        )
      )
      updatedNodes = updatedNodes.updated(node.name, rewrittenNode)
      updatedNodes = updatedNodes + (replacementNodeName -> replacementNode)

      val updatedDag = ProofDag.Dag(updatedNodes)

      (updatedDag, Seq(newObligation))
    }
  }

  // Returns (updatedDag, newObligations)
  def rewriteNegatedConjectureIfNotEqToSyntacticNeg(
      dag: ProofDag.Dag,
      nodeName: String,
      problemFormulas: Seq[TPTP.AnnotatedFormula]
  ): (ProofDag.Dag, Seq[TPTPProblemGenerator.Inference]) = {
    val node = dag.nodes(nodeName)
    val parentNames = node.parents

    if (parentNames.size != 1) {
      throw new ProofErrorException(
        s"Expected exactly one parent for negated conjecture node $nodeName in DAG, found ${parentNames.size}"
      )
    }

    val parentName = parentNames.head
    val parentFormula = dag.nodes(parentName).formula
    val negatedParentFormula = negateFormula(parentFormula)

    val isSyntacticNegation = node.formula match {
      case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(actualFormula), _) =>
        negatedParentFormula == actualFormula
      case _ => false
    }

    if (isSyntacticNegation) {
      (dag, Seq.empty)
    } else {
      val newObligation = TPTPProblemGenerator.Inference(
        "rev_obligation_neg_" + node.name,
        Seq(node.formula),
        TPTP.FOFAnnotated(
          "rev_obligation_neg_" + node.name,
          "conjecture",
          TPTP.FOF.Logical(negatedParentFormula),
          None
        )
      )

      Logger.println(
        "Syntactic mismatch found for negated conjecture node " + nodeName
      )
      val rewrittenParentFormula = parentFormula

      val replacementNodeName = nodeName + "_inputTrans"
      val replacementNode = ProofDag.Node(
        replacementNodeName,
        "negated_conjecture",
        TPTP.FOFAnnotated(
          replacementNodeName,
          "negated_conjecture",
          TPTP.FOF.Logical(negatedParentFormula),
          None
        ),
        InferenceInformation(
          "cth",
          "negated_conjecture",
          Seq(NamedParentInformation(parentName))
        )
      )

      var updatedNodes = dag.nodes
      val rewrittenNode = updatedNodes(node.name).copy(
        role = "plain",
        formula = node.formula,
        additionalInfo = InferenceInformation(
          "thm",
          "thm",
          Seq(NamedParentInformation(replacementNodeName))
        )
      )
      updatedNodes = updatedNodes.updated(node.name, rewrittenNode)
      updatedNodes = updatedNodes.updated(
        parentName,
        updatedNodes(parentName).copy(formula = rewrittenParentFormula)
      )
      updatedNodes = updatedNodes + (replacementNodeName -> replacementNode)

      (ProofDag.Dag(updatedNodes), Seq(newObligation))
    }
  }

  def addInferencesIfSyntacticMismatch(
      dag: ProofDag.Dag,
      problemFormulas: Seq[TPTP.AnnotatedFormula],
      allowSyntacticMismatchOfAxioms: Boolean
  ): (ProofDag.Dag, Seq[TPTPProblemGenerator.Inference]) = {
    var currentDag = dag
    var obligations = Seq.empty[TPTPProblemGenerator.Inference]
    if (allowSyntacticMismatchOfAxioms) {
      for (nodeName <- currentDag.axioms) {
        val (updatedDag, newObs) = rewriteProofAxiomIfNotEqToInputProb(
          currentDag,
          nodeName,
          problemFormulas
        )
        currentDag = updatedDag
        obligations = obligations ++ newObs
      }
    }

    for (nodeName <- currentDag.countersatisfiable) {
      val (updatedDag, newObs) = rewriteNegatedConjectureIfNotEqToSyntacticNeg(
        currentDag,
        nodeName,
        problemFormulas
      )
      currentDag = updatedDag
      obligations = obligations ++ newObs
    }

    (currentDag, obligations)
  }

  // Reuse functions from Main (copied locally references)
  private def negateFormula(
      formula: TPTP.AnnotatedFormula
  ): TPTP.FOF.Formula = {
    formula match {
      case TPTP.FOFAnnotated(_, _, statement, _) =>
        val innerFormula = statement match {
          case TPTP.FOF.Logical(formula) => formula
          case _                         =>
            throw new ProofUnsureException(
              s"Expected logical FOF statement for parent formula ${formula.name}"
            )
        }
        TPTP.FOF.UnaryFormula(TPTP.FOF.~, innerFormula)
      case _ =>
        throw new ProofUnsureException(
          s"Expected logical FOF statement for parent formula ${formula.name}"
        )
    }
  }

  private def formulasAreSyntacticallyEqual(
      left: TPTP.AnnotatedFormula,
      right: TPTP.AnnotatedFormula
  ): Boolean = {
    (left, right) match {
      case (
            TPTP.FOFAnnotated(_, _, leftForm, _),
            TPTP.FOFAnnotated(_, _, rightForm, _)
          ) =>
        leftForm == rightForm
      case (
            TPTP.CNFAnnotated(_, _, leftForm, _),
            TPTP.CNFAnnotated(_, _, rightForm, _)
          ) =>
        leftForm == rightForm
      case _ => false
    }
  }
}
