package ownOntologyPopularizer

/**
  * Created by Espen on 01.11.2016.
  */
//{'wikibase-item': 860, 'commonsMedia': 38, 'math': 2, 'wikibase-property': 11, 'quantity': 324, 'globe-coordinate': 8, 'string': 180, 'external-id': 1290, 'monolingualtext': 30, 'url': 34, 'time': 37}

object MapPropertyDatatypeToClass {

  val datatypeToClass = Map("wikibase-item" -> "ItemProperty", "commonsMedia" -> "CollectionMediaProperty", "math" -> "MathProperty",
  "wikibase-property" -> "OtherProperty", "quantity" -> "QuantityProperty", "globe-coordinate" -> "Globe-CoordinateProperty", "string" -> "StringProperty",
  "external-id" -> "External-idProperty", "monolingualtext" -> "Monolingual-textProperty", "url" -> "URLProperty", "time" -> "TimeProperty")
}
