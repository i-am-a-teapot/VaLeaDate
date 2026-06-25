import leo.datastructures.TPTP
 
object BasicChecks {

    val AXIOM = "axiom"
    val CONJECTURE = "conjecture"
    val INFERENCE = "inference"
    val NEGATED_CONJECTURE = "negated_conjecture"

    def checkAcyclicity(dag: ProofDag.Dag): Boolean = {
        dag.isAcyclic
    }

    def checkAllSourcesAreAxiomsOrConjectures(dag: ProofDag.Dag): Boolean = {
        var hasConjecture = !dag.conjectures.isEmpty
        dag.sources.forall(source => dag.nodes.get(source) match {
            case Some(node) => node.role == AXIOM || (hasConjecture && node.role == CONJECTURE) || (!hasConjecture && node.role == NEGATED_CONJECTURE)
            case None => throw new IllegalArgumentException(s"Source node $source not found in DAG nodes")
        })
    }

    def checkAllNegatedConjecturesAreCTH(dag: ProofDag.Dag): Boolean = {
        dag.negatedConjectures.forall(node => AnnotationInformationHelpers.isCth(dag.nodes(node).additionalInfo))
    }

    def checkAllNegatedConjectureHaveConjectureParent(dag: ProofDag.Dag): Boolean = {
        dag.negatedConjectures.forall(node => dag.nodes(node).parents.exists(parent => 
            dag.nodes(parent).role == CONJECTURE)
        )
    }


    def checkAllInferencesHaveParents(dag: ProofDag.Dag): Boolean = {
        var hasConjecture = !dag.conjectures.isEmpty
        dag.nodes.values.filter(node => node.role != AXIOM && node.role != CONJECTURE).forall(
            node => if(!hasConjecture && node.role == NEGATED_CONJECTURE) true else node.parents.nonEmpty
        )
    }

    def checkAllEdgesReferToExistingNodes(dag: ProofDag.Dag): Boolean = {
        val nodeNames = dag.nodes.keySet
        dag.edges.forall { case ProofDag.Edge(from, to) => nodeNames.contains(from) && nodeNames.contains(to) }
    }

    def checkAllConnectedSinksAreFalse(dag: ProofDag.Dag): Boolean = {
        val connectedSinks = dag.sinks.filter(sink => dag.edges.exists(edge => edge.to == sink))
        Logger.println(s"Connected sinks: ${connectedSinks.mkString(", ")}")
        connectedSinks.forall(connectedSink => {
        val connectedSinkNode = dag.nodes.get(connectedSink) 
  
        connectedSinkNode match {
            case Some(form) => form.formula match {
                
                case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(value), _) =>
                    value match {
                        case TPTP.FOF.AtomicFormula("$false", _) => true
                        case _ => Logger.println(s"Connected sink $connectedSink is not false: ${value.pretty}"); false
                    } 
                case TPTP.CNFAnnotated(_, _, TPTP.CNF.Logical(value), _) => 
                    if(value.isEmpty) {
                        true
                    } else if(value.size == 1) {
                        value.head match {
                            case TPTP.CNF.PositiveAtomic(TPTP.CNF.AtomicFormula("$false", _)) => true
                            case TPTP.CNF.NegativeAtomic(TPTP.CNF.AtomicFormula("$true", _)) => true
                            case _ => Logger.println(s"Connected sink $connectedSink is not false: ${value.head.pretty}"); false
                        }
                    } else {
                        false
                    }
                case _ => Logger.println(s"Unexpected formula type for connected sink $connectedSink"); false
            }
            case _ => Logger.println(s"Connected sink $connectedSink not found in DAG nodes"); false
        }}
        )
        
    }

    def checkConjectureIsNotUsedAsPremise(dag: ProofDag.Dag): Boolean = {
        val conjectures = dag.nodes.values.filter(_.role == CONJECTURE).map(_.name).toSet
        val childrenOfConjectures : Set[String] = dag.edges.collect { 
            case ProofDag.Edge(from, to) if(conjectures.contains(from)) => to
            }.toSet
            
        childrenOfConjectures.forall(child => 
            dag.nodes.get(child) match {
                case Some(node) => node.role == NEGATED_CONJECTURE
                case None => throw new IllegalArgumentException(s"Conjecture child ${child} not found in DAG nodes")
            }
        )
    }

    def checkInputProblemIsSameAsProof(dag: ProofDag.Dag, problemFormulas: Seq[TPTP.AnnotatedFormula]): Boolean = {
        val nodesToCheck = dag.axioms ++ dag.conjectures
        for(equivsToCheck <- nodesToCheck){ 
            val problemFormulaNameOpt = AnnotationInformationHelpers.fileParentInformation(dag.nodes(equivsToCheck).additionalInfo)
            if(problemFormulaNameOpt.isEmpty) {
                throw new ProofErrorException(s"Node $equivsToCheck does not have a file parent annotation")
            }
            val problemFormulaName = problemFormulaNameOpt.get.formulaName
            val problemFormula = problemFormulas.find(_.name == problemFormulaName) match {
              case Some(formula) => formula
              case None => throw new ProofErrorException(s"Problem formula with name $problemFormulaName not found in input problem")
            }
            val fofProblemFormula = problemFormula match {
                case TPTP.CNFAnnotated(_, _, form, _) => TPTPProblemGenerator.cnfStatementToFOF(form).asInstanceOf[TPTP.FOF.Logical].formula
                case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(form), _) => form
                case _ => throw new IllegalArgumentException(s"Expected CNF or FOF formula for problem formula $problemFormulaName")
            }
            val nodeFromProof = dag.nodes(equivsToCheck)
            val fofNodeFormula = nodeFromProof.formula match {
                case TPTP.CNFAnnotated(_, _, form, _) => TPTPProblemGenerator.cnfStatementToFOF(form).asInstanceOf[TPTP.FOF.Logical].formula
                case TPTP.FOFAnnotated(_, _, TPTP.FOF.Logical(form), _) => form
                case _ => throw new IllegalArgumentException(s"Expected CNF or FOF formula for axiom node $equivsToCheck in DAG")
            }

            if(problemFormula.role != nodeFromProof.role) {
                throw new ProofErrorException(s"Role mismatch for axiom $equivsToCheck: problem formula role is ${problemFormula.role}, but DAG node role is ${nodeFromProof.role}")
            }

            Logger.println(s"Checking alpha-equivalence for axiom $equivsToCheck: ${fofNodeFormula.pretty} vs ${fofProblemFormula.pretty}")
            if(!AlphaEquivalenceChecker.checkAlphaEquivalence(fofNodeFormula, fofProblemFormula)) {
                return false
            }
        }
        true
    }

    def performAllBasicChecks(dag: ProofDag.Dag, problemFormulas: Seq[TPTP.AnnotatedFormula], allowAxiomMismatch: Boolean): Unit = {
        //Throw an exception if any of the checks fail, otherwise print success message
        if (!checkAcyclicity(dag)) {
            throw new ProofErrorException("Proof DAG is not acyclic")
        }
        if(!checkAllSourcesAreAxiomsOrConjectures(dag)) {
            throw new ProofErrorException("Not all sources in the proof DAG are axioms, conjectures, or negated conjectures")
        }

        if(!checkAllNegatedConjecturesAreCTH(dag)) {
            throw new ProofErrorException("Not all negated conjectures in the proof DAG are CTH")
        }

        if(!checkAllNegatedConjectureHaveConjectureParent(dag)) {
            throw new ProofErrorException("Not all negated conjectures in the proof DAG have a conjecture parent")
        }

        if(!checkAllInferencesHaveParents(dag)) {
            throw new ProofErrorException("Not all inferences in the proof DAG have parents") 
        }
        if(!checkAllEdgesReferToExistingNodes(dag)) {
            throw new ProofErrorException("Not all edges in the proof DAG refer to existing nodes")
        }
        if(!checkAllConnectedSinksAreFalse(dag)) {
            throw new ProofErrorException("Not all connected sinks in the proof DAG are false")
        }   
        if(!checkConjectureIsNotUsedAsPremise(dag)) {
            throw new ProofErrorException("Conjecture is used as a premise in the proof DAG")
        }
        Logger.println("Checking input problem formulas...")
        if(!allowAxiomMismatch) {
            if(!checkInputProblemIsSameAsProof(dag, problemFormulas)) {
                throw new ProofErrorException("Input problem formulas do not match the formulas in the proof DAG")
            }
        }
    }
    
}
