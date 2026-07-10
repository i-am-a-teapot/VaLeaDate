import leo.datastructures.TPTP

object BasicChecks {

  val AXIOM = "axiom"
  val CONJECTURE = "conjecture"
  val INFERENCE = "inference"
  val NEGATED_CONJECTURE = "negated_conjecture"

  def checkAcyclicity(dag: ProofDag.Dag): Unit = {
    if (!dag.isAcyclic) {
      throw new ProofErrorException("Proof DAG is not acyclic")
    }
  }

  def checkAllSourcesAreAxiomsOrConjecturesOrPlainWithThm(
      dag: ProofDag.Dag,
      assumeTheorems: Boolean
  ): Unit = {
    var hasConjecture = !dag.conjectures.isEmpty
    dag.sources.foreach(source =>
      dag.nodes.get(source) match {
        case Some(node) =>
          val roleIsAllowed = node.role == AXIOM ||
            node.role == "plain" && AnnotationInformationHelpers.isThm(
              node.additionalInfo,
              assumeTheorems
            ) ||
            (hasConjecture && node.role == CONJECTURE) ||
            (!hasConjecture && node.role == NEGATED_CONJECTURE)
          if (!roleIsAllowed) {
            throw new ProofUnsureException(
              s"Source node $source has unexpected role '${node.role}'"
            )
          }
        case None =>
          throw new ProofErrorException(
            s"Source node $source not found in DAG nodes"
          )
      }
    )
  }

  def checkAllNegatedConjecturesAreCTH(dag: ProofDag.Dag): Unit = {
    dag.negatedConjectures.foreach(node => {
      val dagNode = dag.nodes.getOrElse(
        node,
        throw new ProofErrorException(
          s"Negated conjecture node $node not found in DAG nodes"
        )
      )
      if (!AnnotationInformationHelpers.isCth(dagNode.additionalInfo)) {
        throw new ProofErrorException(
          s"Negated conjecture node $node is missing CTH status"
        )
      }
    })
  }

  def checkAllNegatedConjectureHaveConjectureParent(dag: ProofDag.Dag): Unit = {
    dag.negatedConjectures.foreach(node => {
      val dagNode = dag.nodes.getOrElse(
        node,
        throw new ProofErrorException(
          s"Negated conjecture node $node not found in DAG nodes"
        )
      )
      val hasConjectureParent = dagNode.parents.exists(parent => {
        val parentNode = dag.nodes.getOrElse(
          parent,
          throw new ProofErrorException(
            s"Parent node $parent of negated conjecture $node not found in DAG nodes"
          )
        )
        parentNode.role == CONJECTURE
      })
      if (!hasConjectureParent) {
        throw new ProofErrorException(
          s"Negated conjecture node $node does not have a conjecture parent"
        )
      }
    })
  }

  def checkCTHStatusOnlyOnNegatedConjectures(dag: ProofDag.Dag): Unit = {
    dag.nodes.values
      .filter(node => node.role != NEGATED_CONJECTURE)
      .foreach(node =>
        if (AnnotationInformationHelpers.isCth(node.additionalInfo)) {
          throw new ProofErrorException(
            s"Node ${node.name} with role '${node.role}' has CTH status but is not a negated conjecture"
          )
        }
      )
  }

  def checkAllEdgesReferToExistingNodes(dag: ProofDag.Dag): Unit = {
    val nodeNames = dag.nodes.keySet
    dag.edges.foreach { case ProofDag.Edge(from, to) =>
      if (!nodeNames.contains(from)) {
        throw new ProofErrorException(
          s"Edge references missing source node '$from'"
        )
      }
      if (!nodeNames.contains(to)) {
        throw new ProofErrorException(
          s"Edge references missing target node '$to'"
        )
      }
    }
  }

  def checkFalseConnectedSinks(dag: ProofDag.Dag): Unit = {
    val connectedSinks =
      dag.sinks.filter(sink => dag.edges.exists(edge => edge.to == sink))

    if (!dag.falseSinks.isEmpty) {
      val connectedFalseSinks =
        dag.falseSinks.filter(sink => dag.edges.exists(edge => edge.to == sink))
      if (connectedFalseSinks.size == 0 && dag.nodes.size > 1) {
        throw new ProofErrorException(
          s"No connected sinks are false, but there are false sinks in the DAG: ${dag.falseSinks.mkString(", ")}"
        )
      }
    } else {
      throw new ProofErrorException(
        s"No false sinks found in the proof DAG"
      )
    }
  }

  def checkFalseRootReachesAxioms(dag: ProofDag.Dag): Unit = {
    if (!dag.oneConnectedFalseRoot.isDefined) {
      throw new ProofErrorException(
        s"No false sink reaches any axiom in the proof DAG"
      )
    }
  }

  def checkConjectureIsNotUsedAsPremise(dag: ProofDag.Dag): Unit = {
    val conjectures =
      dag.nodes.values.filter(_.role == CONJECTURE).map(_.name).toSet
    val childrenOfConjectures: Set[String] = dag.edges.collect {
      case ProofDag.Edge(from, to) if (conjectures.contains(from)) => to
    }.toSet

    childrenOfConjectures.foreach(child =>
      dag.nodes.get(child) match {
        case Some(node) =>
          if (node.role != NEGATED_CONJECTURE) {
            throw new ProofErrorException(
              s"Conjecture is used as premise for node $child with role '${node.role}'"
            )
          }
        case None =>
          throw new ProofErrorException(
            s"Conjecture child $child not found in DAG nodes"
          )
      }
    )
  }

  def checkInputProblemIsSameAsProof(
      dag: ProofDag.Dag,
      problemFormulas: Seq[TPTP.AnnotatedFormula],
      treatNegatedConjectureAsAxiom: Boolean = false
  ): Unit = {
    val nodesToCheck = dag.axioms ++ dag.conjectures
    for (equivsToCheck <- nodesToCheck) {
      val problemFormulaNameOpt =
        AnnotationInformationHelpers.fileParentInformation(
          dag.nodes(equivsToCheck).additionalInfo
        )
      if (problemFormulaNameOpt.isEmpty) {
        throw new ProofErrorException(
          s"Node $equivsToCheck does not have a file parent annotation"
        )
      }
      val problemFormulaName = problemFormulaNameOpt.get.formulaName
      val problemFormula =
        problemFormulas.find(_.name == problemFormulaName) match {
          case Some(formula) => formula
          case None          =>
            throw new ProofErrorException(
              s"Problem formula with name $problemFormulaName not found in input problem"
            )
        }
      val fofProblemFormula = problemFormula match {
        case TPTP.CNFAnnotated(_, _, form, _) =>
          TPTPProblemGenerator
            .cnfStatementToFOF(form)
            .asInstanceOf[TPTP.FOF.Logical]
            .formula
        case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(form), _) => form
        case _                                                  =>
          throw new ProofUnsureException(
            s"Expected CNF or FOF formula for problem formula $problemFormulaName"
          )
      }
      val nodeFromProof = dag.nodes(equivsToCheck)
      val fofNodeFormula = nodeFromProof.formula match {
        case TPTP.CNFAnnotated(_, _, form, _) =>
          TPTPProblemGenerator
            .cnfStatementToFOF(form)
            .asInstanceOf[TPTP.FOF.Logical]
            .formula
        case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(form), _) => form
        case _                                                  =>
          throw new ProofUnsureException(
            s"Expected CNF or FOF formula for axiom node $equivsToCheck in DAG"
          )
      }

      if (problemFormula.role != nodeFromProof.role) {
        if (
          treatNegatedConjectureAsAxiom && problemFormula.role == NEGATED_CONJECTURE && nodeFromProof.role == AXIOM
        ) {
          Logger.println(
            s"Treating negated conjecture ${nodeFromProof.name} as axiom"
          )
        } else {
          throw new ProofErrorException(
            s"Role mismatch for axiom $equivsToCheck: problem formula role is ${problemFormula.role}, but DAG node role is ${nodeFromProof.role}"
          )
        }
      }

      Logger.println(
        s"Checking alpha-equivalence for axiom $equivsToCheck: ${fofNodeFormula.pretty} vs ${fofProblemFormula.pretty}"
      )
      if (
        !AlphaEquivalenceChecker.checkAlphaEquivalence(
          fofNodeFormula,
          fofProblemFormula
        )
      ) {
        throw new ProofErrorException(
          s"Formula mismatch for node $equivsToCheck: proof formula '${fofNodeFormula.pretty}' is not alpha-equivalent to input formula '${fofProblemFormula.pretty}'"
        )
      }
    }
  }


  def checkESAIsSupported(dag: ProofDag.Dag): Unit = {
    dag.nodes.values
      .filter(node => AnnotationInformationHelpers.isEsa(node.additionalInfo))
      .foreach(node => {
        if (
          !AnnotationInformationHelpers
            .containsRuleStep("skolemize", node.additionalInfo)
        ) {
          throw new ProofUnsureException(
            s"ESA step ${node.name} does have a esa inference which is not supported step."
          )
        }
      })
  }

  def checkEachNodeHasValidStatus(dag: ProofDag.Dag, assumeThm : Boolean): Unit = {
    dag.nodes.values.foreach(node => {
      val status = AnnotationInformationHelpers.getStatuses(node.additionalInfo)
      if (node.role != AXIOM && node.role != CONJECTURE) {
        if (status.isEmpty && !assumeThm) {
          throw new ProofErrorException(
            s"Node ${node.name} does not have a status annotation"
          )
        }
        var validSet = Set("esa", "cth", "thm")
        if (assumeThm) {
          validSet += ""
        }
        status.foreach(s => {
          if (!validSet.contains(s.toLowerCase)) {
            throw new ProofErrorException(
              s"Node ${node.name} has an invalid status annotation: $s"
            )
          }
        })
      }
    })
  }

  def checkSkolemizationStepBasics(dag: ProofDag.Dag): Unit = {
    dag.nodes.values
      .filter(node =>
        AnnotationInformationHelpers
          .containsRuleStep("skolemize", node.additionalInfo)
      )
      .foreach(node => {
        val details = AnnotationInformationHelpers
          .getSkolemizationInformation(node.additionalInfo)
        val status = AnnotationInformationHelpers.isEsa(node.additionalInfo)
        if (!status) {
          throw new ProofErrorException(
            s"Skolemization step ${node.name} does not have status esa"
          )
        }
        if (details.newSymbols.length != 1) {
          throw new ProofErrorException(
            s"Skolemization step ${node.name} does not have exactly one new symbol"
          )
        }

        var referencedVariablesFromParents: Set[String] = Set.empty
        var referencedVariablesInSkolemSymbols: Set[String] = Set.empty
        for ((variable, function, args) <- details.skolemDefinitions) {
          referencedVariablesFromParents += variable
          referencedVariablesInSkolemSymbols ++= args

        }
        val newSymbolsReferenced = details.newSymbols.forall { sym =>
          details.skolemDefinitions.exists { case (variable, function, args) =>
            function == sym
          }
        }
        var fofFormula = node.formula match {
          case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(form), _) => form
          case TPTP.CNFAnnotated(_, _, form, _)                   =>
            TPTPProblemGenerator
              .cnfStatementToFOF(form)
              .asInstanceOf[TPTP.FOF.Logical]
              .formula
          case _ =>
            throw new ProofErrorException(
              s"Expected CNF or FOF formula for skolemization step ${node.name}"
            )
        }

        if (!details.newSymbols.toSet.subsetOf(fofFormula.symbols)) {
          throw new ProofErrorException(
            s"Not all new symbols in skolemization step ${node.name} are present in the formula: ${fofFormula.pretty}"
          )
        }

        var fofParent = node.parents.headOption match {
          case Some(parentName) =>
            dag.nodes.get(parentName) match {
              case Some(parentNode) =>
                parentNode.formula match {
                  case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(form), _) =>
                    form
                  case TPTP.CNFAnnotated(_, _, form, _) =>
                    TPTPProblemGenerator
                      .cnfStatementToFOF(form)
                      .asInstanceOf[TPTP.FOF.Logical]
                      .formula
                  case _ =>
                    throw new ProofErrorException(
                      s"Expected CNF or FOF formula for parent of skolemization step ${node.name}"
                    )
                }
              case None =>
                throw new ProofErrorException(
                  s"Parent node ${parentName} of skolemization step ${node.name} not found in DAG"
                )
            }
          case None =>
            throw new ProofErrorException(
              s"Skolemization step ${node.name} does not have a parent"
            )
        }

        if (!AnnotatedFormulaHelpers.checkFormulaIsInNNF(fofParent)) {
          fofParent = AnnotatedFormulaHelpers.transformFormulaToNNF(fofParent)
        }
        if (!AnnotatedFormulaHelpers.checkFormulaIsInNNF(fofFormula)) {
          fofFormula = AnnotatedFormulaHelpers.transformFormulaToNNF(fofFormula)
        }
        val exQuantVariablesParent = AnnotatedFormulaHelpers
          .collectQuantifiedFormulaVariables(fofParent, TPTP.FOF.?)
        val allQuantVariablesSkol = AnnotatedFormulaHelpers
          .collectQuantifiedFormulaVariables(fofFormula, TPTP.FOF.!)
        Logger.println(
          s"${newSymbolsReferenced} && ${referencedVariablesInSkolemSymbols.subsetOf(allQuantVariablesSkol)} && ${referencedVariablesFromParents.subsetOf(exQuantVariablesParent)}"
        )
        Logger.println(
          "exQuantifiedVariables: " + allQuantVariablesSkol.mkString(", ")
        )
        Logger.println(
          "referencedVariablesInSkolemSymbols: " + referencedVariablesInSkolemSymbols
            .mkString(", ")
        )
        Logger.println(
          "referencedVariablesFromParents: " + referencedVariablesFromParents
            .mkString(", ")
        )
        Logger.println(
          "forallQuantifiedVariablesInParent: " + exQuantVariablesParent
            .mkString(", ")
        )
        if (!newSymbolsReferenced) {
          throw new ProofErrorException(
            s"Skolemization step ${node.name} declares new symbols that are not defined in skolem definitions"
          )
        }
        if (
          !referencedVariablesInSkolemSymbols.subsetOf(allQuantVariablesSkol)
        ) {
          throw new ProofErrorException(
            s"Skolemization step ${node.name} references variables in skolem symbols that are not universally quantified in the step formula"
          )
        }
        if (!referencedVariablesFromParents.subsetOf(exQuantVariablesParent)) {
          throw new ProofErrorException(
            s"Skolemization step ${node.name} references source variables that are not existentially quantified in the parent formula"
          )
        }
      })
  }

  def checkIfEveryAxiomHasFileParentAnnotation(dag : ProofDag.Dag, problemFormulas: Seq[TPTP.AnnotatedFormula]): Unit = {
    val problemFormulaNames = problemFormulas.map(_.name).toSet
    dag.axioms.foreach(axiom => {
      val node = dag.nodes(axiom)
      val fileParentInfoOpt =
        AnnotationInformationHelpers.fileParentInformation(node.additionalInfo)
      if (fileParentInfoOpt.isEmpty) {
        throw new ProofErrorException(
          s"Axiom node $axiom does not have a file parent annotation"
        )
      }
      val fileParentInfo = fileParentInfoOpt.get
      if (!problemFormulaNames.contains(fileParentInfo.formulaName)) {
        throw new ProofErrorException(
          s"Axiom node $axiom has a file parent annotation with formula name '${fileParentInfo.formulaName}' that does not exist in the input problem"
        )
      }
    })
  }
  

  def performAllBasicChecks(
      dag: ProofDag.Dag,
      problemFormulas: Seq[TPTP.AnnotatedFormula],
      allowAxiomMismatch: Boolean,
      assumeTheorems: Boolean,
      treatNegatedConjectureAsAxiom: Boolean
  ): Unit = {
    Logger.println("Checking all edges refer to existing nodes...")
    checkAllEdgesReferToExistingNodes(dag)
    Logger.println("Checking acyclicity of the proof DAG...")
    checkAcyclicity(dag)
    Logger.println("Checking all sources are axioms or conjectures...")
    checkAllSourcesAreAxiomsOrConjecturesOrPlainWithThm(dag, assumeTheorems)
    Logger.println("Checking all negated conjectures are CTH...")
    checkAllNegatedConjecturesAreCTH(dag)
    Logger.println("Checking negated conjectures have conjecture parents...")
    checkAllNegatedConjectureHaveConjectureParent(dag)
    Logger.println("Checking CTH status only on negated conjectures...")
    checkCTHStatusOnlyOnNegatedConjectures(dag)
    Logger.println("Checking connected sinks have false...")
    checkFalseConnectedSinks(dag)
    Logger.println("Checking conjecture is not used as premise...")
    checkConjectureIsNotUsedAsPremise(dag)
    Logger.println("Checking if every axiom has a file parent annotation...")
    checkIfEveryAxiomHasFileParentAnnotation(dag, problemFormulas)
    Logger.println("Smoke testing skolemization steps ...")
    checkSkolemizationStepBasics(dag)
    Logger.println("Checking if every ESA step is supported...")
    checkESAIsSupported(dag)
    Logger.println("Checking if false root reaches axioms...")
    checkFalseRootReachesAxioms(dag)
    Logger.println("Checking if each node has valid status...")
    checkEachNodeHasValidStatus(dag, assumeTheorems)

    Logger.println("Checking alpha-equivalence of problem formulas...")
    if (!allowAxiomMismatch) {
      checkInputProblemIsSameAsProof(
        dag,
        problemFormulas,
        treatNegatedConjectureAsAxiom
      )
    }
  }

}
