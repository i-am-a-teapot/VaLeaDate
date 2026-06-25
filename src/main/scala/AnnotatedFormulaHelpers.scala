import leo.datastructures.TPTP
 

object AnnotatedFormulaHelpers {
  def gatherKeywordsInTerm(gt: TPTP.GeneralTerm, keywords: Set[String]): Seq[TPTP.MetaFunctionData] = {
    def rec(term: TPTP.GeneralTerm): Seq[TPTP.MetaFunctionData] = {
      val fromList = term.list.toSeq.flatten.flatMap(rec)
      val fromData = term.data.toSeq.flatMap {
        case m@TPTP.MetaFunctionData(f, _) if keywords.contains(f) => Seq(m)
        case TPTP.MetaFunctionData(_, args) => args.flatMap(rec)
        case _ => Seq.empty
      }
      fromData ++ fromList
    }
    rec(gt)
  }

  def findSingleInference(annotation: TPTP.Annotations, keywords: Set[String] = Set("inference")): Option[TPTP.MetaFunctionData] = {
    val all = annotation match {
      case Some((gt, _)) => gatherKeywordsInTerm(gt, keywords)
      case None => Seq.empty
    }
    if (all.size > 1) throw new IllegalArgumentException(s"Multiple sections from $keywords found in annotation")
    all.headOption
  }

  def parentNamesFromTerm(term: TPTP.GeneralTerm): Seq[String] = {
    if (term.list.isDefined) Seq.empty
    else {
      term.data match {
        case Seq(TPTP.MetaFunctionData("inference", args)) if args.size >= 3 =>
          args(2).list.toSeq.flatten.flatMap(parentNamesFromTerm)
        case Seq(TPTP.MetaFunctionData(name, Seq())) => Seq(name)
        case Seq(TPTP.NumberData(number)) => Seq(number.pretty)
        case _ => Seq.empty
      }
    }
  }

  def sanitizeName(name: String): String =
    name.replace("(", "").replace(")", "").replace(" ", "_").replace("'", "").replace(",", "")
}
