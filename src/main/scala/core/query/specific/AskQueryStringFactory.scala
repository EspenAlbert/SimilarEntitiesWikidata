package core.query.specific

import core.globals.SimilarPropertyOntology

/**
  * Created by espen on 09.05.17.
  */
object AskQueryStringFactory {
  def oneOfTypesUsedInRange(comparableTypes: List[String], property: String) : String =
    s"""
       |ask
       |where {
       |?s <${SimilarPropertyOntology.propertyDistributionNode}> ?pdN .
       |?pdN <${SimilarPropertyOntology.distributionForProperty}> <$property> .
       |?pdN <${SimilarPropertyOntology.rangeCount}> ?rC .
       |filter(?s in (<${comparableTypes.mkString(">,<")}>))
       |}
     """.stripMargin


  def oneOfTypesUsedInDomain(comparableTypes: List[String], property: String) : String =
    s"""
       |ask
       |where {
       |?s <${SimilarPropertyOntology.propertyDistributionNode}> ?pdN .
       |?pdN <${SimilarPropertyOntology.distributionForProperty}> <$property> .
       |?pdN <${SimilarPropertyOntology.domainCount}> ?dC .
       |filter(?s in (<${comparableTypes.mkString(">,<")}>))
       |}
     """.stripMargin

}
