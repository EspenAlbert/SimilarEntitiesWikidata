package strategies

import rdf.GraphRDF

/**
  * Created by Espen on 09.11.2016.
  */
object StrategyGenerator {

  def generateStrategies(entity: GraphRDF) : List[Strategy] = {
    throw new NotImplementedError()
  }
  def generateInBNotInAStrategies(otherEntities : List[String]) : List[Strategy] = {
    throw new NotImplementedError()
  }

}
