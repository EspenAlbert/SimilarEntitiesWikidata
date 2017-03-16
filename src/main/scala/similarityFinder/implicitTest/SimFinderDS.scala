package similarityFinder.implicitTest

import core.globals.MyDatasets

/**
  * Created by espen on 10.03.17.
  */
class SimFinderDS(dataset: String) {
  implicit val activeDataset = dataset
  Querier.queryMe("The query")

  def worksForFunctionToo(): Unit = {
    Querier.queryMe("the query")
  }
  def worksForCallingAnotherFunctionFromFunction(query: String): Unit = {
    Querier.queryInferDataset(query)
  }

}
object SimFinderDS {
  implicit var dataset = MyDatasets.ValueMatch
  def filterGeoPropertyTypes(properties: List[String])(implicit dataset: String) : List[String] = {
    return List(dataset)
  }
  def filterGeoPropertyTypes2(properties: List[String]) : List[String] = {
    return List(dataset)
  }

}