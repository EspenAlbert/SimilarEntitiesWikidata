package core.globals

/**
  * Created by Espen on 04.11.2016.
  */
object ResultsSimilarArtistsGlobals extends Enumeration{


  type ResultsSimilarArtistGlobals = Value
  val base = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#")
  val similar =  Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#similar")
  val statementCount=  Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#statementCount")
  val foundEntity=  Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#foundEntity")
  val ranking=  Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#ranking")
  val simScore=  Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#simScore")
  val runType = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#runType")
  val execTime = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#execTime")
  val foundEntitiesCount = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#foundEntitiesCount")
  val recalled= Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#recalled")
  val notRecalled= Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#notRecalled")
  val qEntity= Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#qEntity")
  val qEntityResult= Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#qEntityResult")
  val hadTimeout = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#hadTimeout")
  val recall = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#recall")
  val precision = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#precision")
  val avgExecTime = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#avgExecTime")
  val avgFoundEntities = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#avgFoundEntities")
  val percentTimeout = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#percentTimeout")
  val entityRelation = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#entity")
  val featureName = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#featureName")
  val featureWeight = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#featureWeight")
  val featureFound = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#featureFound")
  val featurePath= Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#featurePath")
  val expectedSimilar= Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#expectedSimilar")
  val relevantRelationshipsCount= Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#relevantRelationshipsCount")
  val totalRelationshipsCount= Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#totalRelationshipsCount")
  val runNamePrefix = Value("http://www.espenalbert.com/rdf/resultsSimilarArtists#runNamePrefix/")

  implicit def getStringFromOptionsForResultQueryVariable(similarPropertyOntology: ResultsSimilarArtistGlobals) : String = {
    return similarPropertyOntology.toString
  }
  def getRunURIFromName(runName: String) : String= {
    if(runName.startsWith("http")) runName else {
      val runURI = s"""${ResultsSimilarArtistsGlobals.runNamePrefix}$runName"""
      runURI
    }
  }


}
