package core.rdf

/**
  * Created by espen on 13.05.17.
  */
object Triples {
  abstract class ResultStatement(val subject : String, val property: String, val objectValue: String)
  case class OrdinaryStatement(override val subject : String, override val property: String, override val objectValue: String) extends ResultStatement(subject, property, objectValue)
  case class VMStatement(override val subject : String, override val property: String, override val objectValue: String, replacement : (String, String), vmCount: (String, Int)) extends ResultStatement(subject, property, objectValue)
  case class TMStatement(override val subject : String, override val property: String, override val objectValue: String, entityTypeOrgEntityReplacements : List[(String, String, String)]) extends ResultStatement(subject, property, objectValue)
}
