import leo.datastructures.TPTP
import leo.datastructures.TPTP.MetaFunctionData

object ProofDag {

  // abstract class AdditionalInformation;
  // final case class InferenceInformation(status: String, rule: String, parents: Seq[String]) extends AdditionalInformation
  // final case class FileInformation(name: String, role: String) extends AdditionalInformation

  final case class Node(
      name: String,
      role: String,
      formula: TPTP.AnnotatedFormula,
      additionalInfo: AnnotationInformation
  ) {
    def parents: Seq[String] =
      AnnotationInformationHelpers.namedParents(additionalInfo).distinct.sorted
  }

  final case class Edge(from: String, to: String)

  final case class Dag(nodes: Map[String, Node]) {
    lazy val edges: Seq[Edge] =
      nodes.values.toSeq.sortBy(_.name).flatMap { node =>
        node.parents.map(parent => Edge(parent, node.name))
      }

    lazy val sources: Seq[String] = {
      val indeg =
        scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
      nodes.keys.foreach(k => indeg(k) = 0)
      edges.foreach(e => indeg(e.to) = indeg(e.to) + 1)
      nodes.keys.toSeq.filter(k => indeg(k) == 0).sorted
    }

    lazy val sinks: Seq[String] = {
      val outdeg =
        scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
      nodes.keys.foreach(k => outdeg(k) = 0)
      edges.foreach(e => outdeg(e.from) = outdeg(e.from) + 1)
      nodes.keys.toSeq.filter(k => outdeg(k) == 0).sorted
    }

    lazy val topologicalSort: List[String] = {
      import scala.collection.mutable
      val adj = mutable.Map.empty[String, mutable.ArrayBuffer[String]]
      nodes.keys.foreach(k => adj(k) = mutable.ArrayBuffer.empty[String])
      edges.foreach { e =>
        adj.getOrElseUpdate(e.from, mutable.ArrayBuffer.empty) += e.to
      }

      val indeg = mutable.Map.empty[String, Int]
      nodes.keys.foreach(k => indeg(k) = 0)
      edges.foreach(e => indeg(e.to) = indeg.getOrElse(e.to, 0) + 1)

      val queue = mutable.Queue.empty[String]
      indeg.foreach { case (k, v) => if (v == 0) queue.enqueue(k) }

      val sort = mutable.ArrayBuffer.empty[String]

      var count = 0
      while (queue.nonEmpty) {
        val n = queue.dequeue()
        sort += n
        for (child <- adj.getOrElse(n, mutable.ArrayBuffer.empty)) {
          indeg(child) = indeg(child) - 1
          if (indeg(child) == 0) queue.enqueue(child)
        }
      }
      sort.toList
    }

    lazy val conjectures: Seq[String] =
      nodes.values.toSeq.filter(_.role == "conjecture").map(_.name).sorted
    lazy val negatedConjectures: Seq[String] = nodes.values.toSeq
      .filter(_.role == "negated_conjecture")
      .map(_.name)
      .sorted
    lazy val countersatisfiable: Seq[String] = nodes.values.toSeq
      .filter(node => AnnotationInformationHelpers.isCth(node.additionalInfo))
      .map(_.name)
      .sorted
    lazy val axioms: Seq[String] =
      nodes.values.toSeq.filter(_.role == "axiom").map(_.name).sorted

    def isAcyclic: Boolean = {
      // Find out by topological sorting
      import scala.collection.mutable
      val adj = mutable.Map.empty[String, mutable.ArrayBuffer[String]]
      nodes.keys.foreach(k => adj(k) = mutable.ArrayBuffer.empty[String])
      edges.foreach { e =>
        adj.getOrElseUpdate(e.from, mutable.ArrayBuffer.empty) += e.to
      }

      val indeg = mutable.Map.empty[String, Int]
      nodes.keys.foreach(k => indeg(k) = 0)
      edges.foreach(e => indeg(e.to) = indeg.getOrElse(e.to, 0) + 1)

      val queue = mutable.Queue.empty[String]
      indeg.foreach { case (k, v) => if (v == 0) queue.enqueue(k) }

      var count = 0
      while (queue.nonEmpty) {
        val n = queue.dequeue()
        count += 1
        for (child <- adj.getOrElse(n, mutable.ArrayBuffer.empty)) {
          indeg(child) = indeg(child) - 1
          if (indeg(child) == 0) queue.enqueue(child)
        }
      }
      count == nodes.size
    }

    def toDot: String = {
      val nodeLines = nodes.values.toSeq.sortBy(_.name).map { node =>
        val base = s"${escape(node.name)}\\n${escape(node.role)}"
        val statusPart = s"\\n[${escape(node.additionalInfo.toString())}]"
        val formulaPart =
          s"\\n${escape(truncate(LeanPrettyPrinter.prettyLeanSyntax(node.formula), 140))}"
        val label = base + statusPart + formulaPart
        val style = "filled"
        val fill = "lightgoldenrod1"
        s"  ${dotId(node.name)} [label=\"$label\", shape=box, style=\"$style\", fillcolor=\"$fill\"];"
      }

      val edgeLines = edges.map { edge =>
        s"  ${dotId(edge.from)} -> ${dotId(edge.to)};"
      }

      (Seq(
        "digraph ProofDag {",
        "  rankdir=BT;",
        "  node [fontname=\"Helvetica\"];",
        "  edge [arrowsize=0.7];"
      ) ++
        nodeLines ++
        edgeLines ++
        Seq("}")).mkString("\n")
    }
  }

  def fromProof(
      steps: Seq[TPTP.AnnotatedFormula],
      assumeThm: Boolean,
      negatedConjectureAsAxiom: Boolean
  ): Dag = {
    val nodes = scala.collection.mutable.LinkedHashMap.empty[String, Node]

    steps.foreach { step =>
      val nameOpt = stepName(step)
      if (nameOpt.isEmpty)
        throw new ProofUnsureException(
          s"Error: could not retrieve name for step $step"
        )
      val name = AnnotatedFormulaHelpers.sanitizeName(nameOpt.get)

      Logger.println(s"Processing step $name", Logger.VERBOSITY_MEDIUM)

      val annotations =
        AnnotationInformationHelpers.getInformationFromAnnotation(
          step.annotations
        )
      Logger.println(
        s"Step $name has additional information: $annotations",
        Logger.VERBOSITY_MEDIUM
      )
      val roleOpt = stepRole(step)
      if (roleOpt.isEmpty) {
        throw new ProofErrorException(
          s"Error: could not retrieve role for step $name"
        )
      }
      var role = roleOpt.getOrElse(
        throw new ProofUnsureException(s"role not found for step $name")
      )
      if (negatedConjectureAsAxiom && role == "negated_conjecture") {
        if (
          AnnotationInformationHelpers
            .fileParentInformation(annotations.get)
            .isDefined
        ) {
          role = "axiom"
        }
      }
      if (annotations.isDefined) {
        // check if there is a status
        val status = AnnotationInformationHelpers.getStatuses(annotations.get)
        val fileInfo =
          AnnotationInformationHelpers.fileParentInformation(annotations.get)
        if (!assumeThm && status.isEmpty && fileInfo.isEmpty) {
          throw new ProofErrorException(
            s"Error: no status found for step $name and assumeThm is false"
          )
        }

        // If we've already recorded this node as an actual proof step, that's a duplicate
        if (nodes.contains(name)) {
          throw new ProofErrorException(
            s"Duplicate node name encountered: $name"
          )
        }
        nodes.update(
          name,
          Node(name, role, formula = step, additionalInfo = annotations.get)
        )
      } else {
        // If we've already recorded this node as an actual proof step, that's a duplicate
        if (nodes.contains(name)) {
          throw new ProofErrorException(
            s"Duplicate node name encountered: $name"
          )
        }
        nodes.update(
          name,
          Node(
            name,
            role,
            formula = step,
            additionalInfo = EmptyAnnotationInformation()
          )
        )
      }
    }
    Dag(nodes.toMap)
  }

  private def stepName(step: TPTP.AnnotatedFormula): Option[String] =
    step match {
      case TPTP.THFAnnotated(name, _, _, _) => Some(name)
      case TPTP.TFFAnnotated(name, _, _, _) => Some(name)
      case TPTP.FOFAnnotated(name, _, _, _) => Some(name)
      case TPTP.TCFAnnotated(name, _, _, _) => Some(name)
      case TPTP.CNFAnnotated(name, _, _, _) => Some(name)
      case TPTP.TPIAnnotated(name, _, _, _) => Some(name)
    }

  private def stepRole(step: TPTP.AnnotatedFormula): Option[String] =
    step match {
      case TPTP.THFAnnotated(_, role, _, _) => Some(role)
      case TPTP.TFFAnnotated(_, role, _, _) => Some(role)
      case TPTP.FOFAnnotated(_, role, _, _) => Some(role)
      case TPTP.TCFAnnotated(_, role, _, _) => Some(role)
      case TPTP.CNFAnnotated(_, role, _, _) => Some(role)
      case TPTP.TPIAnnotated(_, role, _, _) => Some(role)
    }

  private def truncate(s: String, n: Int): String = {
    val cleaned = s.replaceAll("[\r\n]+", " ").replaceAll("\\s+", " ").trim
    if (cleaned.length <= n) cleaned else cleaned.take(n - 3) + "..."
  }

  private def dotId(name: String): String =
    name.map {
      case c if c.isLetterOrDigit || c == '_' => c
      case _                                  => '_'
    }

  private def escape(value: String): String =
    value.replace("\\", "\\\\").replace("\"", "\\\"")

}
