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

  implicit def getStringFromOptionsForResultQueryVariable(similarPropertyOntology: ResultsSimilarArtistGlobals) : String = {
    return similarPropertyOntology.toString
  }


}
