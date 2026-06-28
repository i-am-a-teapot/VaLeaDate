import leo.datastructures.TPTP
import TPTP.MetaFunctionData

abstract class AnnotationInformation{
  override def toString(): String
}

abstract class AdditionalInformation {
  override def toString(): String
}

final case class SkolemizationInformation(newSymbols: Seq[String], skolemDefinitions: Seq[(String, String, Seq[String])]) extends AdditionalInformation {
  override def toString(): String = {
    val newSymbolsStr = newSymbols.mkString(", ")
    val skolemDefsStr = skolemDefinitions.map { case (v, f, args) =>
      s"($v, $f, [${args.mkString(", ")}])"
    }.mkString(", ")
    s"SkolemizationInformation(newSymbols=[$newSymbolsStr], skolemDefinitions=[$skolemDefsStr])"
  }
}

abstract class ParentInformation

final case class NamedParentInformation(name: String) extends ParentInformation
final case class UnnamedInformation(inferenceInformation: InferenceInformation) extends ParentInformation

final case class EmptyAnnotationInformation() extends AnnotationInformation {
  override def toString(): String = "EmptyAnnotationInformation"
}
final case class InferenceInformation(status: String, rule: String, parents: Seq[ParentInformation], additionalInfo: Seq[AdditionalInformation] = Seq.empty) extends AnnotationInformation {
  override def toString(): String = {
    val parentsStr = parents.map {
      case NamedParentInformation(name) => name
      case UnnamedInformation(n) => s"UnnamedInformation($n)"
    }.mkString(", ")
    s"InferenceInformation(status=$status, rule=$rule, parents=[$parentsStr], additionalInfo=[${additionalInfo.mkString(", ")}])"
  }
}
final case class FileInformation(fileName: String, formulaName: String) extends AnnotationInformation {
  override def toString(): String = s"FileInformation(fName=$fileName, name=$formulaName)"
}

final object AnnotationInformationHelpers {

  def namedParents(annotationInformation: AnnotationInformation): Seq[String] = {
    var names = annotationInformation match {
      case InferenceInformation(_, _, parents, _) =>
        parents.flatMap {
          case NamedParentInformation(name) => Seq(name)
          case UnnamedInformation(inf) => namedParents(inf)
          case _ => Seq.empty
        }
      case _ => Seq.empty
    }
    return names.distinct.sorted
  }

  def isThm(annotationInformation: AnnotationInformation, assumeThm: Boolean): Boolean = {
    val statuses = getStatuses(annotationInformation)
    if(assumeThm){
      if(statuses.isEmpty){
        return true
      }
      return statuses.forall(stat => stat == "thm" || stat == "unknown")
    }
    return statuses.forall(_ == "thm")
  }

  def isCth(annotationInformation: AnnotationInformation): Boolean = {
    getStatuses(annotationInformation).contains("cth")
  }

  def isEsa(annotationInformation: AnnotationInformation): Boolean = {
    getStatuses(annotationInformation).contains("esa")
  }

  def getStatuses(annotationInformation: AnnotationInformation): Seq[String] = {
    annotationInformation match {
      case InferenceInformation(status, _, parents, _) => {
        val parentStatuses = parents.flatMap {
          case NamedParentInformation(_) => Seq.empty
          case UnnamedInformation(inf) => getStatuses(inf)
        }
        status +: parentStatuses
      }
      case _ => Seq.empty
    }
  }

  def containsRuleStep(ruleName :String, annotationInformation: AnnotationInformation): Boolean = {
    annotationInformation match {
      case InferenceInformation(_, rule, parents, _) => {
        rule == ruleName || parents.exists {
          case NamedParentInformation(_) => false
          case UnnamedInformation(inf) => containsRuleStep(ruleName, inf)
        }
      }
      case _ => false
    }
  }

  def getRuleStep(ruleName :String, annotationInformation: AnnotationInformation): Seq[InferenceInformation] = {
    annotationInformation match {
      case inf@InferenceInformation(_, rule, parents, _) => {
        val fromParents = parents.flatMap {
          case NamedParentInformation(_) => Seq.empty
          case UnnamedInformation(inf) => getRuleStep(ruleName, inf)
        }
        if(rule == ruleName) {
          return Seq(inf) ++ fromParents
        } else {
          return fromParents
        }
      }
      case _ => Seq.empty
    }
  }

  def fileParentInformation(annotationInformation: AnnotationInformation): Option[FileInformation] = {
    annotationInformation match {
      case info@FileInformation(fileName, formulaName) => Some(info)
      case _ => None
    }
  }

  def getSkolemizationInformation(annotationInformation: AnnotationInformation): SkolemizationInformation = {
    val skolemizeAnnotations = getRuleStep("skolemize", annotationInformation);
    if(skolemizeAnnotations.length != 1){
        throw new ProofErrorException(s"Expected exactly one skolemization step, found ${skolemizeAnnotations.length}")
    }
    val skolemizationDetails = skolemizeAnnotations.head.additionalInfo
    if(skolemizationDetails.length != 1){
        throw new ProofErrorException(s"Expected exactly one skolemization details, found ${skolemizationDetails.length}")
    }
    val details = skolemizationDetails.head match {
        case info@SkolemizationInformation(newSymbols, skolemDefinitions) => info
        case _ => throw new ProofErrorException(s"Expected skolemization details, found ${skolemizationDetails.head.getClass}")
    }
    return details
  }

  private def inferencesFromTerm(term: TPTP.GeneralTerm): Option[InferenceInformation] = {
    if (term.list.isDefined) None
    else {
      term.data match {
        case Seq(TPTP.MetaFunctionData("inference", args)) if args.size >= 3 =>{
          val statusArg = args(1)
          val status = gatherKeywordsInTerm(statusArg, Set("status")).flatMap { metaFunctionData =>
            inferenceStatus(metaFunctionData)
          }.headOption.getOrElse("unknown")
          val rule = args(0).data.toSeq.flatMap {
            case MetaFunctionData(f, args) => {
              Some(f)
            }
            case _ => None
          }.headOption.getOrElse("unknown")
          var details = Seq.empty[AdditionalInformation]
          if(rule == "skolemize"){
            Logger.println(s"Extracting skolemization details for term: ${term.pretty}", Logger.VERBOSITY_MEDIUM)
            details = details :+ extractSkolemizationDetails(statusArg)
          }
          Logger.println(s"Status for inference: $status", Logger.VERBOSITY_MEDIUM)
          Logger.println(s"Rule for inference: $rule", Logger.VERBOSITY_MEDIUM)
          var inferenceInformationFromArgs = None

          var parentInformation = Seq.empty[ParentInformation]
          for(i <- 2 until args.size) {
            val argument = args(i)
            //try and get parent inferences
            if(argument.list.isDefined){
              for(arg <- argument.list.get) {
                val parentInference = inferencesFromTerm(arg)
                if(parentInference.nonEmpty) {
                  parentInformation = parentInformation ++ Seq(UnnamedInformation(parentInference.get))
                } else {
                  val name = arg.data match {
                    case Seq(TPTP.MetaFunctionData(name, Seq())) => Some(name)
                    case _ => None
                  }
                  if(name.nonEmpty) {
                    parentInformation = parentInformation :+ NamedParentInformation(name.get)
                  }
                }
              }
            }
          }
          Some(InferenceInformation(status, rule, parentInformation, details))
        }
        case Seq(TPTP.MetaFunctionData(name, Seq())) => None
        case _ => None
      }
    }
  }

  def getInformationFromAnnotation(annotation: TPTP.Annotations) : Option[AnnotationInformation] = {
    Logger.println("Inferences list")

    if(annotation.isEmpty) {
      Logger.println("No annotation found")
      return None
    } 

    //try get file information
    val fileInfo = findInferenceList(annotation, Set("file")).headOption
    if(fileInfo.isDefined) {
      val fileNameOpt = fileName(fileInfo)
      val fileInputFormulaNameOpt = fileInputFormulaName(fileInfo)
      if(fileNameOpt.isDefined && fileInputFormulaNameOpt.isDefined) {
        Logger.println(s"File name: ${fileNameOpt.get}",Logger.VERBOSITY_MEDIUM)
        Logger.println(s"File input formula name: ${fileInputFormulaNameOpt.get}",Logger.VERBOSITY_MEDIUM)
        return Some(FileInformation(fileNameOpt.get, fileInputFormulaNameOpt.get))
      }
    }
    //try get inference information
    val firstGeneralTerm = annotation.get._1
    return inferencesFromTerm(firstGeneralTerm)
  }

  def extractSkolemizationDetails(inputStep: TPTP.GeneralTerm): SkolemizationInformation = {
    val skolemizeTerms = gatherKeywordsInTerm(inputStep, Set("skolemize"))
    val newSymbols = gatherKeywordsInTerm(inputStep, Set("new_symbols"))
    val newSymbolTerm = newSymbols.headOption.getOrElse {
      throw new ProofErrorException(s"No new_symbols term found in skolemization details: ${inputStep.pretty}")
    }
    val newSymbolInformation = newSymbolTerm match {
      case MetaFunctionData(f, args) => {
        if(f != "new_symbols") {
          throw new IllegalArgumentException(s"Expected new_symbols term, found $f")
        }
        if(args.size < 2) {
          throw new IllegalArgumentException(s"Expected new_symbols term to have at least 2 arguments, found ${args.size}")
        }
        val typeOfNewSymobls = args(0).data match {
          case Seq(TPTP.MetaFunctionData(typeName, Seq())) => typeName
          case _ => throw new IllegalArgumentException(s"Expected new_symbols term to have a type as the first argument, found ${args(0).pretty}")
        }
        Logger.println(s"Type of new symbols term: ${typeOfNewSymobls}", Logger.VERBOSITY_MEDIUM)
        val newSymbols = args(1).list match {
          case Some(list) => list.flatMap(term => term.data match {
            case Seq(TPTP.MetaFunctionData(name, Seq())) => Some(name)
            case _ => None
          })
          case None => throw new IllegalArgumentException(s"Expected new_symbols term to have a list as the second argument, found ${args(1).pretty}")
        }
        Logger.println(s"New symbols term: ${newSymbols}", Logger.VERBOSITY_MEDIUM)
        newSymbols
      }
      case _ => throw new IllegalArgumentException(s"Expected new_symbols term to be a MetaFunctionData, found ${newSymbolTerm.pretty}")
    }

    val skolemizeTerm = skolemizeTerms.headOption.getOrElse {
      throw new ProofErrorException(s"No skolemize term found in skolemization details: ${inputStep.pretty}")
    }
    val skolemArityInformation = skolemizeTerm match {
      case MetaFunctionData(f, args) => {
        if(f != "skolemize") {
          throw new IllegalArgumentException(s"Expected skolemize term, found $f")
        }
        if(args.size < 2) {
          throw new IllegalArgumentException(s"Expected skolemize term to have at least 2 arguments, found ${args.size}")
        }
        val variableToSkolemize = args(0).data match {
          case Seq(TPTP.MetaVariable(v)) => v
          case _ => throw new IllegalArgumentException(s"Expected skolemize term to have a variable as the first argument, found ${args(0).pretty}")
        }
        Logger.println(s"Variable to skolemize: ${variableToSkolemize}", Logger.VERBOSITY_MEDIUM)
        val skolemFunctionSize = args(1).data match {
          case Seq(TPTP.MetaFunctionData(f, fArgs)) => {
            val argNames = fArgs.flatMap { arg =>
              arg.data match {
                case Seq(TPTP.MetaVariable(v)) => Some(v)
                case Seq(TPTP.MetaFunctionData(name, Seq())) => Some(name)
                case _ => None
              }
            }
            (f, argNames)
          }
          case _ => throw new IllegalArgumentException(s"Expected skolemize term to have a function call as the second argument, found ${args(1).pretty}")
        }
        Logger.println(s"Skolem function: ${skolemFunctionSize._1}(${skolemFunctionSize._2.mkString(", ")})", Logger.VERBOSITY_MEDIUM)
        Seq((variableToSkolemize, skolemFunctionSize._1, skolemFunctionSize._2))
      }
      case _ => throw new IllegalArgumentException(s"Expected skolemize term to be a MetaFunctionData, found ${skolemizeTerm.pretty}")
    }

    SkolemizationInformation(newSymbolInformation, skolemArityInformation)

  }


  def findInferenceList(annotation: TPTP.Annotations, keywords: Set[String] = Set("inference")): Seq[TPTP.MetaFunctionData] = {
    val all = annotation match {
      case Some((gt, _)) => gatherKeywordsInTerm(gt, keywords)
      case None => Seq.empty
    }
    if (all.size > 1) throw new IllegalArgumentException(s"Multiple sections from $keywords found in annotation")
    all
  }

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

  private def inferenceStatus(inference: TPTP.MetaFunctionData): Option[String] = {
    inference match {
      case TPTP.MetaFunctionData("status", Seq(statusTerm)) =>
        statusTerm.data match {
          case Seq(TPTP.MetaFunctionData(name, Seq())) => Some(name)
          case _ => None
        }
        case _ => None
    } 
  }

  private def inferenceRule(inference: Option[TPTP.MetaFunctionData]): Option[String] = {
    inference match {
      case Some(TPTP.MetaFunctionData(_, inferenceData)) if inferenceData.size >= 1 => {
        inferenceData(0).data.toSeq.flatMap { data =>
          data match {
            case MetaFunctionData(f, args) => {
              Some(f)
            }
            case _ => None
          }
        }.headOption
      }
      case _ => None
    }
  }

  private def fileName(fileInfo: Option[TPTP.MetaFunctionData]): Option[String] = {
    fileInfo match {
      case Some(TPTP.MetaFunctionData("file", args)) if args.nonEmpty =>
        args(0).data.toSeq.collectFirst {
          case MetaFunctionData(rawName, Seq()) => stripOuterQuotes(rawName)
        }
      case _ => None
    }
  }

  private def fileInputFormulaName(fileInfo: Option[TPTP.MetaFunctionData]): Option[String] = {
    fileInfo match {
      case Some(TPTP.MetaFunctionData("file", args)) if args.size >= 2 =>
        args(1).data.toSeq.collectFirst {
          case MetaFunctionData(role, Seq()) => stripOuterQuotes(role)
        }
      case _ => None
    }
  }

  private def stripOuterQuotes(value: String): String = {
    if ((value.startsWith("'") && value.endsWith("'")) || (value.startsWith("\"") && value.endsWith("\""))) {
      value.substring(1, value.length - 1)
    } else value
  }

 

  private def truncate(s: String, n: Int): String = {
    val cleaned = s.replaceAll("[\r\n]+", " ").replaceAll("\\s+", " ").trim
    if (cleaned.length <= n) cleaned else cleaned.take(n - 3) + "..."
  }
}