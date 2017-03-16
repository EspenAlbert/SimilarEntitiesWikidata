package similarityFinder.implicitTest

import core.query.specific.DatasetInferrer

/**
  * Created by espen on 10.03.17.
  */
object Querier {

  def queryMe(query : String)(implicit dataset: String) = {
    println("Dataset: "+ dataset)
  }
  def queryInferDataset(query: String)(implicit dataset: String) ={
    println(DatasetInferrer.getDatasetWithImplicit(query))
  }

}
