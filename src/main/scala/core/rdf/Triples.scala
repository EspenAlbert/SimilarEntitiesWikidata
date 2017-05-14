package core.rdf

/**
  * Created by espen on 13.05.17.
  */
object Triples {
  abstract class ResultTriple(val subject : String,   val property: String,  val objectValue: String)
  case class OrdinaryTriple(override val subject : String,   override val property: String,  override val objectValue: String) extends ResultTriple(subject, property, objectValue)
  case class VMTriple( override val subject : String,  override val property: String,  override val objectValue: String, replacement : (String, String), vmCount: (String, Int)) extends ResultTriple(subject, property, objectValue)
  case class TMTriple( override val subject : String,  override val property: String,  override val objectValue: String, entityTypeOrgEntityReplacements : List[(String, String, String)]) extends ResultTriple(subject, property, objectValue)
}
