package core.rdf

/**
  * Created by espen on 13.05.17.
  */
object Triples {
  val IRRELEVANT = "IRRELEVANT"
  abstract class ResultStatement(val subject : String, val property: String, val objectValue: String)
  case class OrdinaryStatement(override val subject : String, override val property: String, override val objectValue: String) extends ResultStatement(subject, property, objectValue)
  case class SharedPropertyStatement(override val subject : String, override val property: String, override val objectValue: String) extends ResultStatement(subject, property, objectValue)
  case class VMStatement(override val subject : String, override val property: String, override val objectValue: String, replacement : (String, String), vmCount: (String, Int)) extends ResultStatement(subject, property, objectValue)
  case class TMStatement(override val subject : String, override val property: String, override val objectValue: String, domainTypes : List[String], rangeTypes: List[String]) extends ResultStatement(subject, property, objectValue)
  case class TMStatementReplaceBothTypes(override val subject : String, override val property: String, override val objectValue: String) extends ResultStatement(subject, property, objectValue)
  case class TMStatementResult(override val subject : String, override val property: String, override val objectValue: String, entityEntityTypeOldSubject : (String, String, String), entityEntityTypeOldObject :(String, String, String)) extends ResultStatement(subject, property, objectValue)
}
