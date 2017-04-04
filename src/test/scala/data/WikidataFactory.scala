package data

import core.globals.{KnowledgeGraph, MyDatasets, SimilarPropertyOntology}
import core.rdf.GraphRDF
import core.strategies.PropertyMatchStrategy

/**
  * Created by espen on 16.03.17.
  */
object WikidataFactory {


  val countMaleGender = 2645899

  val domainCountGender = 1405976
  val rangeCountOccupation = 4792

  implicit val knowledgeGraph = KnowledgeGraph.wikidata
  val w = "http://www.wikidata.org/entity/"
  val directLinkProperty = w + "P1393"
  val itemProperty = w + "P1026"
  val valueMatchObject = w + "P1072"
  val timeOfDiscoveryProperty = w + "P575"
  val quantityProp = w + "P1971"
  val itemProperties = List(directLinkProperty, valueMatchObject, itemProperty)

  val externalIdProperties : List[String]= List(w + "P2099", w + "P2736", w + "P597", w + "P2642", w + "P3192")
  val stringProperties = flattenList(List(w + "P645", externalIdProperties)).asInstanceOf[List[String]]
  val urlProperties = List(w + "P2488")
  val ordinaryProperties : List[String] = flattenList(List(urlProperties, stringProperties, itemProperties, timeOfDiscoveryProperty, quantityProp)).asInstanceOf[List[String]]


  val qualifierProperties = List(w + "P39P1734q", w + "P18P580q")
  val qualifierGeoProperties = List(w + "P20P625qla", w + "P20P625qlo", w + "P885P625qla", w + "P885P625qlo")
  val geoProperties = List(w + "P625lo", w + "P625la", w + "P1333la", w + "P1333lo")

  val allProperties : List[String] = flattenList(List(
    ordinaryProperties,
    qualifierProperties,
    qualifierGeoProperties,
    geoProperties)).asInstanceOf[List[String]]

  def flattenList(ls : List[Any]): List[Any] = {
    ls.flatMap{
      case i: List[_] => flattenList(i)
      case e => List(e)
    }
  }
  val obama = w + "Q76"
  val human = w + "Q5"
  val ringoStarr = new {
    val id = w + "Q2632"
    val rdfTypes = List(human)
    val dateOfBProp = w + "P569"
    val dateOfBValue = "\"1940-07-07\"^^xsd:date"
    val spouseProp =w + "P26"
    val spouseValues = List(w + "Q233993", w + "Q2124432")
    val memberOfProp = w + "P463"
    val memberOfValue = w + "Q1299"
    val performerProp = w + "P175"
    val performerSubject1 = w + "Q21682709"
    val lifestyleProp = w + "P1576"
    val lifestyleValue = w + "Q18338317"
    val musicBrainzIdProp = w + "P434"
    val musicBrainzIdValue = "300c4c73-33ac-4255-9d57-4e32627f5e13"
    val occupationProp = w + "P106"
    val occupationValues = List(w + "Q33999", w + "Q386854", w +  "Q36834", w + "Q2405480", w + "Q488205", w + "Q10800557")
    val genderProp = w + "P21"
    val genderValue = w + "Q6581097"
    val propertyMatchStrategyLifeStyle = PropertyMatchStrategy(lifestyleProp, true, rdfTypes, 200)
    val countryOfCitizenShipValue = w + "Q145"
    val countryOfCitizenShipProperty = w + "P27"
    val performedByMostCommonTypes = List(w + "Q2188189", w + "Q482994", w + "Q2031291")
    val performerSubjectsMostPopularObjects = List(
      (w + "P407", w + "Q1860", 28),
      (w + "P31", w + "Q482994", 19),
      (w + "P136", w + "Q11399", 11),
      (w + "P264", w + "Q1273666", 4)
    )
    val otherPerformersWithMoreThan2RockMusicPerformances = List(w + "Q11036", w + "Q212533", w + "Q1052727", w + "Q2382329")
  }


  val johnLennon = w + "Q1203"
  val paulMcCartney = w + "Q2599"
  val georgeHarrison = w + "Q2643"
  val peteBest = w + "Q207335"
  val stuartSutcliffe = w + "Q204218" //Error in the new wikidata I guess, he should not be a member of the beatles
  val theBeatles = new {
    val members = List(ringoStarr.id, johnLennon, paulMcCartney, georgeHarrison, peteBest)
    val id = w + "Q1299"
  }
  val placeOfBirthProp = w + "P19"

  val genre = w + "P136"
  val rockMusicGenre = w + "Q11399"

  def obamaSubjectStatements: List[(String, String, String)] = {
    val p1o = List(w + "Q380782", w + "Q1379733")
    val p = w + "P410" //Military Rank
    val p2 = w + "P106"
    val p2o = List(w + "Q82955"," " + w + "Q40348"," " + w + "Q15958642")
    return p1o.map(o => (obama, p, o)) ++ p2o.map(o => (obama, p2, o))
  }
  def obamaObjectStatements: List[(String, String, String)] = {
    val p1 = w + "P180" //depiction of, painting of, motif, portrait of, landscape of, represents
    val p1s = List(w + "Q2915674"," " + w + "Q4858105"," " + w + "Q5842038")
    val p2 = w + "P26"//spouse
    val p2s = w + "Q13133"
    return List((p2s, p2, obama)) ++ p1s.map(s => (s, p1, obama))
  }
  def obamaStatements: List[(String, String, String)] = {
    return obamaSubjectStatements ++ obamaObjectStatements
  }

  def kristiansandAndTypes(): (GraphRDF, List[String]) = {
    val kristiansand = new GraphRDF(SimilarPropertyOntology.w + "Q2415")
    val type1 = w + "Q515"
    val type2 = w + "Q755707"
    val expectedTypes = List(type1, type2)
    return (kristiansand, expectedTypes )
  }
}
