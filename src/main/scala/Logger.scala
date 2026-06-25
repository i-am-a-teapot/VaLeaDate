object Logger {
  private var verbosity: Int = 0

  //define verbosity levels
  val VERBOSITY_NONE: Int = 0
  val VERBOSITY_LOW: Int = 1
  val VERBOSITY_MEDIUM: Int = 2
  val VERBOSITY_HIGH: Int = 3
  
  def setVerbose(verbose: Int): Unit = {
    verbosity = verbose
  }

  def isVerbose: Boolean = verbosity != 0

  def println(msg: String): Unit = {
    if (verbosity != 0) scala.Predef.println(msg)
  }

  def println(msg: Any): Unit = {
    if (verbosity != 0) scala.Predef.println(msg)
  }

  def println(): Unit = {
    if (verbosity != 0) scala.Predef.println()
  }

  def print(msg: String): Unit = {
    if (verbosity != 0) scala.Predef.print(msg)
  }

  def print(msg: Any): Unit = {
    if (verbosity != 0) scala.Predef.print(msg)
  }

  def println(msg: String, verbosity: Int): Unit = {
    if (this.verbosity >= verbosity) scala.Predef.println(msg)
  }

  def print(msg: Any, verbosity: Int): Unit = {
    if (this.verbosity >= verbosity) scala.Predef.print(msg)
  }
}
