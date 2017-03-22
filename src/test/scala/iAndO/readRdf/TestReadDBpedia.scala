package iAndO.readRdf

import core.globals.{KnowledgeGraph, MyDatasets}
import core.globals.KnowledgeGraph.KnowledgeGraph
import core.query.specific.{AskQuery, QueryFactory, UpdateQueryFactory}
import org.scalatest.FunSuite
import tags.{ActiveSlowTag, ActiveTag}

/**
  * Created by espen on 20.03.17.
  */
class TestReadDBpedia extends FunSuite{
  test("Wikidata filter") {
    val statementShouldFind = "<http://dbpedia.org/resource/Wales> <http://www.w3.org/2002/07/owl#sameAs> <http://www.wikidata.org/entity/Q25> ."
    val statementShouldNotFind = "<http://dbpedia.org/resource/Wales> <http://www.w3.org/2002/07/owl#sameAs> <http://wikidata.dbpedia.org/resource/Q25> ."
    val statementShouldNotFind2 = "<http://dbpedia.org/resource/Wales> <http://www.w3.org/2002/07/owl#sameAs> <http://af.dbpedia.org/resource/Wallis> ."
    assert(ReadDBpedia.wikidataFilter.findFirstIn(statementShouldFind).isDefined)
    assert(ReadDBpedia.wikidataFilter.findFirstIn(statementShouldNotFind).isEmpty)
    assert(ReadDBpedia.wikidataFilter.findFirstIn(statementShouldNotFind2).isEmpty)
  }
  test("Datasets are existing", ActiveTag) {
    implicit val knowledgeGraph = KnowledgeGraph.dbPedia
    //SKOS Categories
    val s1 = "<http://dbpedia.org/resource/Category:World_War_II> <http://www.w3.org/2004/02/skos/core#broader> <http://dbpedia.org/resource/Category:The_World_Wars> ."
    val s2 = "<http://dbpedia.org/resource/Category:Programming_languages> <http://www.w3.org/2004/02/skos/core#related> <http://dbpedia.org/resource/Category:Programming_language_topics> ."
    testStatementsExist(List(s1,s2))

    //Topical concepts
    val tConceptsStatements = "<http://dbpedia.org/resource/Category:Futurama> <http://purl.org/dc/terms/subject> <http://dbpedia.org/resource/Futurama> .\n<http://dbpedia.org/resource/Futurama> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2004/02/skos/core#Concept> .\n<http://dbpedia.org/resource/Category:Programming_languages> <http://purl.org/dc/terms/subject> <http://dbpedia.org/resource/Programming_language> .\n<http://dbpedia.org/resource/Programming_language> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2004/02/skos/core#Concept> .\n<http://dbpedia.org/resource/Category:Anime> <http://purl.org/dc/terms/subject> <http://dbpedia.org/resource/Anime> .".split("\n")
    testStatementsExist(tConceptsStatements)


    //instance_types_en
    val instanceTypesStatements = "<http://dbpedia.org/resource/Achilles> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Thing> .\n<http://dbpedia.org/resource/An_American_in_Paris> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Thing> .\n<http://dbpedia.org/resource/Actrius> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://dbpedia.org/ontology/Film> .\n<http://dbpedia.org/resource/Animalia_(book)> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://dbpedia.org/ontology/Book> .\n<http://dbpedia.org/resource/Agricultural_science> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Thing> .\n<http://dbpedia.org/resource/Alain_Connes> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://dbpedia.org/ontology/Scientist> .\n<http://dbpedia.org/resource/Allan_Dwan> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://dbpedia.org/ontology/Person> .\n<http://dbpedia.org/resource/Allan_Dwan__1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://dbpedia.org/ontology/PersonFunction> .".split("\n")
    testStatementsExist(instanceTypesStatements)

    //article_categories
    val articleCategoriesStatements = "<http://dbpedia.org/resource/Albedo> <http://purl.org/dc/terms/subject> <http://dbpedia.org/resource/Category:Climate_forcing> .\n<http://dbpedia.org/resource/Albedo> <http://purl.org/dc/terms/subject> <http://dbpedia.org/resource/Category:Climatology> .\n<http://dbpedia.org/resource/Albedo> <http://purl.org/dc/terms/subject> <http://dbpedia.org/resource/Category:Electromagnetic_radiation> .\n<http://dbpedia.org/resource/Albedo> <http://purl.org/dc/terms/subject> <http://dbpedia.org/resource/Category:Radiometry> .\n<http://dbpedia.org/resource/Albedo> <http://purl.org/dc/terms/subject> <http://dbpedia.org/resource/Category:Scattering,_absorption_and_radiative_transfer_(optics)> .\n<http://dbpedia.org/resource/Albedo> <http://purl.org/dc/terms/subject> <http://dbpedia.org/resource/Category:Radiation> .".split("\n")
    testStatementsExist(articleCategoriesStatements)
    //page links
    val pageLinksStatements = "<http://dbpedia.org/resource/AccessibleComputing> <http://dbpedia.org/ontology/wikiPageWikiLink> <http://dbpedia.org/resource/Computer_accessibility> .\n<http://dbpedia.org/resource/AfghanistanGeography> <http://dbpedia.org/ontology/wikiPageWikiLink> <http://dbpedia.org/resource/Geography_of_Afghanistan> .\n<http://dbpedia.org/resource/AfghanistanPeople> <http://dbpedia.org/ontology/wikiPageWikiLink> <http://dbpedia.org/resource/Demographics_of_Afghanistan> .\n<http://dbpedia.org/resource/AfghanistanCommunications> <http://dbpedia.org/ontology/wikiPageWikiLink> <http://dbpedia.org/resource/Communications_in_Afghanistan> .\n<http://dbpedia.org/resource/AfghanistanTransportations> <http://dbpedia.org/ontology/wikiPageWikiLink> <http://dbpedia.org/resource/Transport_in_Afghanistan> .\n<http://dbpedia.org/resource/AfghanistanTransnationalIssues> <http://dbpedia.org/ontology/wikiPageWikiLink> <http://dbpedia.org/resource/Foreign_relations_of_Afghanistan> .\n<http://dbpedia.org/resource/AfghanistanMilitary> <http://dbpedia.org/ontology/wikiPageWikiLink> <http://dbpedia.org/resource/Afghan_Armed_Forces> .\n<http://dbpedia.org/resource/AfghanistanHistory> <http://dbpedia.org/ontology/wikiPageWikiLink> <http://dbpedia.org/resource/History_of_Afghanistan> .".split("\n")
    testStatementsExist(pageLinksStatements)

    //Mappingbased Literals
    val mappingBasedLiterals = "<http://dbpedia.org/resource/Actrius> <http://xmlns.com/foaf/0.1/name> \"Actresses\"@en .\n<http://dbpedia.org/resource/Actrius> <http://dbpedia.org/ontology/runtime> \"6000.0\"^^<http://www.w3.org/2001/XMLSchema#double> .\n<http://dbpedia.org/resource/Animalia_(book)> <http://xmlns.com/foaf/0.1/name> \"Animalia\"@en .\n<http://dbpedia.org/resource/Animalia_(book)> <http://dbpedia.org/ontology/numberOfPages> \"32\"^^<http://www.w3.org/2001/XMLSchema#positiveInteger> .\n<http://dbpedia.org/resource/Animalia_(book)> <http://dbpedia.org/ontology/isbn> \"0-810-91868-4\".\n<http://dbpedia.org/resource/Alain_Connes> <http://xmlns.com/foaf/0.1/name> \"Alain Connes\"@en .\n<http://dbpedia.org/resource/Alain_Connes> <http://dbpedia.org/ontology/birthDate> \"1947-04-01\"^^<http://www.w3.org/2001/XMLSchema#date> .\n<http://dbpedia.org/resource/Allan_Dwan> <http://xmlns.com/foaf/0.1/name> \"Allan Dwan\"@en .\n<http://dbpedia.org/resource/Allan_Dwan> <http://dbpedia.org/ontology/birthDate> \"1885-04-03\"^^<http://www.w3.org/2001/XMLSchema#date> .\n<http://dbpedia.org/resource/Allan_Dwan> <http://dbpedia.org/ontology/birthYear> \"1885\"^^<http://www.w3.org/2001/XMLSchema#gYear> .\n<http://dbpedia.org/resource/Allan_Dwan> <http://dbpedia.org/ontology/deathDate> \"1981-12-28\"^^<http://www.w3.org/2001/XMLSchema#date> .".split("\n")
    testStatementsExist(mappingBasedLiterals)

    //Mappingbased objects http://downloads.dbpedia.org/preview.php?file=2016-04_sl_core-i18n_sl_en_sl_mappingbased_objects_en.ttl.bz2
    val mappingBasedObjects = "<http://dbpedia.org/resource/Actrius> <http://dbpedia.org/ontology/director> <http://dbpedia.org/resource/Ventura_Pons> .\n<http://dbpedia.org/resource/Actrius> <http://dbpedia.org/ontology/producer> <http://dbpedia.org/resource/Ventura_Pons> .\n<http://dbpedia.org/resource/Actrius> <http://dbpedia.org/ontology/writer> <http://dbpedia.org/resource/Josep_Maria_Benet_i_Jornet> .\n<http://dbpedia.org/resource/Actrius> <http://dbpedia.org/ontology/distributor> <http://dbpedia.org/resource/Walt_Disney_Studios_Motion_Pictures> .\n<http://dbpedia.org/resource/Actrius> <http://dbpedia.org/ontology/country> <http://dbpedia.org/resource/Spain> .\n<http://dbpedia.org/resource/Actrius> <http://dbpedia.org/ontology/language> <http://dbpedia.org/resource/Catalan_language> .\n<http://dbpedia.org/resource/Actrius> <http://dbpedia.org/ontology/writer> <http://dbpedia.org/resource/Ventura_Pons> .\n<http://dbpedia.org/resource/Animalia_(book)> <http://dbpedia.org/ontology/author> <http://dbpedia.org/resource/Graeme_Base> .\n<http://dbpedia.org/resource/Animalia_(book)> <http://dbpedia.org/ontology/illustrator> <http://dbpedia.org/resource/Graeme_Base> .\n<http://dbpedia.org/resource/Animalia_(book)> <http://dbpedia.org/ontology/literaryGenre> <http://dbpedia.org/resource/Picture_book> .\n<http://dbpedia.org/resource/Animalia_(book)> <http://dbpedia.org/ontology/publisher> <http://dbpedia.org/resource/Harcourt_(publisher)> .\n<http://dbpedia.org/resource/Alain_Connes> <http://dbpedia.org/ontology/doctoralAdvisor> <http://dbpedia.org/resource/Jacques_Dixmier> .\n<http://dbpedia.org/resource/Alain_Connes> <http://dbpedia.org/ontology/doctoralStudent> <http://dbpedia.org/resource/Georges_Skandalis> .\n<http://dbpedia.org/resource/Alain_Connes> <http://dbpedia.org/ontology/birthPlace> <http://dbpedia.org/resource/Draguignan> .".split("\n")
    testStatementsExist(mappingBasedObjects)

  }

  private def testStatementsExist(statements: Iterable[String])(implicit knowledgeGraph: KnowledgeGraph) = {
    assert(statements.size > 1)
    statements.foreach(s => assert(AskQuery.ask(() => s)))
  }
  test("Only valid properties should be present", ActiveSlowTag) {
    val validProperties = List("http://dbpedia.org/ontology/wikiPageWikiLink", "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", "http://purl.org/dc/terms/subject", "http://www.w3.org/2004/02/skos/core#Concept", "http://www.w3.org/2004/02/skos/core#related>","http://www.w3.org/2004/02/skos/core#prefLabel","http://www.w3.org/2004/02/skos/core#broader")
    implicit val knowledgeGraph = KnowledgeGraph.dbPedia
    val distinctProperties = QueryFactory.findAllDistinctProperties
    println(distinctProperties.length)
    distinctProperties.foreach( prop => if(!validProperties.contains(prop))UpdateQueryFactory.cleanDatasetWhere(MyDatasets.DBpediaDS, s"?s <$prop> ?o"))
    val distinctPropertiesAfter = QueryFactory.findAllDistinctProperties
    assert(distinctPropertiesAfter.length == validProperties.length)

  }
}
