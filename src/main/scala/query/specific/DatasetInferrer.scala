package query.specific

import globals.{MyDatasets, SimilarPropertyOntology}

/**
  * Created by Espen on 15.11.2016.
  */
object DatasetInferrer {
  def overrideDataset(SimilarProperties: String) : Unit= {
    overrideDataset = SimilarProperties
  }
  var overrideDataset = ""
  val patternForValueMatchDataset = ("" + SimilarPropertyOntology.valueMatchClass + "|" +SimilarPropertyOntology.valueMatchCount + "|" + SimilarPropertyOntology.valueMatchValue + "").r
  val patternForSimilarPropOntology = (SimilarPropertyOntology.spo + "").r
  def getDataset(query : String) : String = {
    if(overrideDataset != "") {
      val oldValue = overrideDataset
      overrideDataset = ""
      return oldValue
    }
    patternForValueMatchDataset.findFirstIn(query) match {
      case Some(s) => return MyDatasets.ValueMatch
      case None => patternForSimilarPropOntology.findFirstMatchIn(query) match {
        case Some(s) => return MyDatasets.SimilarProperties
        case None => return MyDatasets.DsBig//CHANGE FOR YOUR DB

      }

    }
  }
}
