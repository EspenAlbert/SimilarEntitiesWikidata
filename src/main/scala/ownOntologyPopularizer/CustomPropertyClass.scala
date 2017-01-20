package ownOntologyPopularizer

import globals.SimilarPropertyOntology

/**
  * Created by Espen on 01.11.2016.
  */
//{'wikibase-item': 860, 'commonsMedia': 38, 'math': 2, 'wikibase-property': 11, 'quantity': 324, 'globe-coordinate': 8, 'string': 180, 'external-id': 1290, 'monolingualtext': 30, 'url': 34, 'time': 37}

object CustomPropertyClass extends Enumeration{

  type CustomPropertyClass = Value
  private def getFullName(value : String) : String = {
    return SimilarPropertyOntology.spo.toString + "#" + value
  }
  val baseProperty = Value(getFullName("Property"))
  val itemProperty = Value(getFullName("ItemProperty"))
  val collectionMediaProperty = Value(getFullName("CollectionMediaProperty"))
  val mathProperty = Value(getFullName("MathProperty"))
  val otherProperty = Value(getFullName("OtherProperty"))
  val quantityProperty = Value(getFullName("QuantityProperty"))
  val coordinateProperty = Value(getFullName("CoordinateProperty"))
  val stringProperty = Value(getFullName("StringProperty"))
  val external_idProperty = Value(getFullName("External-idProperty"))
  val monolingual_TextProperty = Value(getFullName("Monolingual-TextProperty"))
  val urlProperty = Value(getFullName("URLProperty"))
  val timeProperty = Value(getFullName("TimeProperty"))

  val datatypeToClass = Map("wikibase-item" -> itemProperty, "commonsMedia" -> "CollectionMediaProperty", "math" -> "MathProperty",
  "wikibase-property" -> "OtherProperty", "quantity" -> "QuantityProperty", "globe-coordinate" -> "Globe-CoordinateProperty", "string" -> "StringProperty",
  "external-id" -> "External-idProperty", "monolingualtext" -> "Monolingual-TextProperty", "url" -> "URLProperty", "time" -> "TimeProperty")


}
