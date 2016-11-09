package main

import rdf.GraphRDF

/**
  * Created by Espen on 09.11.2016.
  */
object SimilarityFinder {

  def findTopKSimilarTo(entity : String, topK : Int) : Unit = {
    val entityGraph = new GraphRDF(entity)
  }

}
