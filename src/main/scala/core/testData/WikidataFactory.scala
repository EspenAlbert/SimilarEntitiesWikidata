package core.testData

import core.globals.{KnowledgeGraphs, SimilarPropertyOntology}
import core.rdf.GraphRDF

/**
  * Created by espen on 16.03.17.
  */
object WikidataFactory {


  val countMaleGender = 2646113

  val domainCountGender = 1405976
  val rangeCountOccupation = 4792
  val domainCountTypes = 19709395

  implicit val knowledgeGraph = KnowledgeGraphs.wikidata
  val w = "http://www.wikidata.org/entity/"
  val hierarchyDepthHuman = 3
  val hierarchyDepthBand = 3
  val hierarchyDepthRockBand = 4
  val entityType = w + "Q35120"
  val typeProperty = w + "P31"
  val professionType = w + "Q28640"
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
  val headOfGovernment = w + "P6"
  val davidCameron = w + "Q192"
  val obama = w + "Q76"
  val human = w + "Q5"
  val band = w + "Q215380"
  val randomBands = List(w+"Q658182")
  val musicalDuo = w + "Q19184926"
  val jazzBand = w + "Q2596245"
  val musicalEnsemble = w + "Q2088357"
  val countryProp = w + "P17"
  val domainTypesLifestyleProp = List(human, "http://www.wikidata.org/entity/Q515", "http://www.wikidata.org/entity/Q3464126", "http://www.wikidata.org/entity/Q515", "http://www.wikidata.org/entity/Q839954", "http://www.wikidata.org/entity/Q486972", "http://www.wikidata.org/entity/Q15649510", "http://www.wikidata.org/entity/Q137535", "http://www.wikidata.org/entity/Q13406463", "http://www.wikidata.org/entity/Q6498826", "http://www.wikidata.org/entity/Q15125752", "http://www.wikidata.org/entity/Q182603", "http://www.wikidata.org/entity/Q4167410", "http://www.wikidata.org/entity/Q44613", "http://www.wikidata.org/entity/Q35509", "http://www.wikidata.org/entity/Q571", "http://www.wikidata.org/entity/Q532", "http://www.wikidata.org/entity/Q216353", "http://www.wikidata.org/entity/Q11424", "http://www.wikidata.org/entity/Q159979", "http://www.wikidata.org/entity/Q2985549", "http://www.wikidata.org/entity/Q3375719", "http://www.wikidata.org/entity/Q188784", "http://www.wikidata.org/entity/Q15632617", "http://www.wikidata.org/entity/Q3658341", "http://www.wikidata.org/entity/Q15773317", "http://www.wikidata.org/entity/Q15773347", "http://www.wikidata.org/entity/Q7918273", "http://www.wikidata.org/entity/Q3320743", "http://www.wikidata.org/entity/Q1855011", "http://www.wikidata.org/entity/Q1715155", "http://www.wikidata.org/entity/Q4931504", "http://www.wikidata.org/entity/Q721207", "http://www.wikidata.org/entity/Q374666", "http://www.wikidata.org/entity/Q16998564", "http://www.wikidata.org/entity/Q8046437", "http://www.wikidata.org/entity/Q3445893", "http://www.wikidata.org/entity/Q2369882", "http://www.wikidata.org/entity/Q27301864", "http://www.wikidata.org/entity/Q2642184")
  val rangeTypesLifestyleProp = List("http://www.wikidata.org/entity/Q189533", "http://www.wikidata.org/entity/Q4875688")
  val ringoStarr = new {
    val performerProp = w + "P175"
    val id = w + "Q2632"
    val rdfTypes = List(human)
    val dateOfBProp = w + "P569"
    val dateOfBValue = "\"1940-07-07\"^^xsd:date"
    val spouseProp =w + "P26"
    val spouseValues = List(w + "Q233993", w + "Q2124432")
    val memberOfProp = w + "P463"
    val memberOfValue = w + "Q1299"
    val performerSubject1 = w + "Q21682709"
    val lifestyleProp = w + "P1576"
    val lifestyleValue = w + "Q18338317"
    val musicBrainzIdProp = w + "P434"
    val musicBrainzIdValue = "300c4c73-33ac-4255-9d57-4e32627f5e13"
    val occupationProp = w + "P106"
    val occupationValues = List(w + "Q33999", w + "Q386854", w +  "Q36834", w + "Q2405480", w + "Q488205", w + "Q10800557")
    val genderProp = w + "P21"
    val genderValue = w + "Q6581097"
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
    val occupationPropsMostPopularObjects = List(
      (w + "P31", w + "Q28640", 5),
      (w + "P1424", w + "Q18821574", 2),
      (w + "P279", w + "Q33999", 2)
    )
    val recordLabelProp = w + "P264"
    val recordLabelValues = List(w + "Q213710")
  }


  val johnLennon = w + "Q1203"
  val paulMcCartney = w + "Q2599"
  val georgeHarrison = w + "Q2643"
  val peteBest = w + "Q207335"
  val stuartSutcliffe = w + "Q204218" //Error in the new wikidata I guess, he should not be a member of the beatles
  val rockBand = w + "Q5741069"
  val theBeatles = new {
    val members = List(ringoStarr.id, johnLennon, paulMcCartney, georgeHarrison, peteBest)
    val id = w + "Q1299"
    val rdfTypes = List(rockBand)
  }
  val placeOfBirthProp = w + "P19"

  val genre = w + "P136"
  val rockMusicGenre = w + "Q11399"

  val topicMainCategoryProp = w + "P910"
  val rockBandPropertyDistribution = new {
    val performerDistibution = (ringoStarr.performerProp, 0.47439613526570046,4,0.47439613526570046, 978)
    val topicMainCategoryPropDistribution = (topicMainCategoryProp,0.47439613526570046 ,347, 0.1212, 0)
    val memberOfDistribution = (ringoStarr.memberOfProp,0d, 0,0.17439613526570047, 361 )
  }


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
