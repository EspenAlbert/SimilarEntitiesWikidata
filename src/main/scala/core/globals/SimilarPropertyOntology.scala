package core.globals

/**
  * Created by Espen on 04.11.2016.
  */
object SimilarPropertyOntology extends Enumeration{


  val maxCountForProperties = 17871093
  val maxPropertyNumber = 3333


//  val a = org.apache.jena.vocabulary.RDFS.
  type SimilarPropertyOntology = Value
  val standardRdfTypeDBpedia = Value("http://www.w3.org/2004/02/skos/core#Concept")
  val wikiPageWikiLink = Value("http://dbpedia.org/ontology/wikiPageWikiLink")
  val owlSameAs = Value("http://www.w3.org/2002/07/owl#sameAs")
  val w = Value("http://www.wikidata.org/entity/")
  val rdfType = Value("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  val rdfsSubclassOf = Value("http://www.w3.org/2000/01/rdf-schema#subClassOf")
  val rdfsLabel = Value("http://www.w3.org/2000/01/rdf-schema#label")
  val spoBaseStrategy = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#Strategy")
  val spo = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#")
  val spoCount = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#count")
  val domainCount = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#domainCount")
  val rangeCount = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#rangeCount")
  val sharableDomain: SimilarPropertyOntology = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#sharableDomain")
  val sharableRange: SimilarPropertyOntology = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#sharableRange")
  val sameTypePossible: SimilarPropertyOntology = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#sameTypePossible")
  val valueMatchClass = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatch")
  val maxMinStats = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#MaxMinStats")
  val valueMatchProperty = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchProperty")
  val valueMatchValue = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchValue")
  val valueMatchCount = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#valueMatchCount")
  val valueMatchSubjectStrategy = Value(spo + "ValueMatchSubjectStrategy")
  val valueMatchObjectStrategy = Value(spo + "ValueMatchObjectStrategy")
  val alternativeLinkStrategy = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#AlternativeLinkStrategy")
  val directLinkStrategy = Value(spo + "DirectLinkStrategy")
  val dateTimeStrategy = Value(spo + "TimeProperty")
  val searchDirectedL1Strategy: SimilarPropertyOntology = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#SearchDirectedL1Strategy")
  val searchDirectedL2Strategy: SimilarPropertyOntology = Value(spo + "SearchDirectedL2Strategy")
  val searchUndirectedL1Strategy: SimilarPropertyOntology = Value(spo + "SearchUndirectedL1Strategy")
  val searchUndirectedL2Strategy: SimilarPropertyOntology = Value(spo + "SearchUndirectedL2Strategy")
  val propertyMatchStrategy: SimilarPropertyOntology = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#PropertyMatchStrategy")
  val basePropertyClassId: SimilarPropertyOntology = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#Property")
  val aggregatorStrategy = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#AggregatorStrategy")
  val expandNodeStrategy = Value("http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#expandNodeStrategy")
  val datatypeBoolean = Value("http://www.w3.org/2001/XMLSchema#boolean")
  val datatypeDouble = Value("http://www.w3.org/2001/XMLSchema#double")
  val timeoutElement = Value("TIMEOUT")
  val isDescriptive: SimilarPropertyOntology = Value(spo + "isDescriptiveProperty")
  val isDomainType: SimilarPropertyOntology = Value(spo + "isDomainType")
  val isRangeType : SimilarPropertyOntology = Value(spo + "isRangeType")


  implicit def getString(similarPropertyOntology: SimilarPropertyOntology) : String = {
    return similarPropertyOntology.toString
  }



}
