package core.strategies

import core.globals.SimilarPropertyOntology

/**
  * Created by espen on 30.03.17.
  */
object StrategyNameFactory {
  def getNameFromStrategyURI(uri : String) : String = {
    uri match {
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchSubjectStrategy" => ValueMatchStrategy.name
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#ValueMatchObjectStrategy" => ValueMatchStrategy.name
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#DirectLinkStrategy" => DirectLinkStrategy.name
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#PropertyMatchStrategy" => PropertyMatchStrategy.name
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#TimeProperty" => DateComparisonStrategy.name
      case "http://www.espenalbert.com/rdf/wikidata/similarPropertyOntology#AlternativeLinkStrategy" => "NOT USED.."
      case a if a==SimilarPropertyOntology.searchDirectedL1Strategy.toString => SearchDirectedL1Strategy.name
      case a if a==SimilarPropertyOntology.searchDirectedL2Strategy.toString => SearchDirectedL2Strategy.name
      case a if a==SimilarPropertyOntology.searchUndirectedL1Strategy.toString => SearchUndirectedL1Strategy.name
      case a if a==SimilarPropertyOntology.searchUndirectedL2Strategy.toString => SearchUndirectedL2Strategy.name
    }
  }

}
