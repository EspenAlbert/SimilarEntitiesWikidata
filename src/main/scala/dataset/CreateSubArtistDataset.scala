package dataset

import breeze.linalg.sum
import graphProducer.GraphDataProducer

import scala.collection.mutable

/**
  * Created by espen on 03.02.17.
  */
object CreateSubArtistDataset {
  def produceDataset() : Map[String, List[String]] = {
    val entitiesMoreThanXStatements = GraphDataProducer.getSetOfEntitiesWithMoreThanXStatements(100)
    val artistiDatasetFull = ArtistDatasetReader.getDatasetFromFile()
    val filteredDataset = mutable.Map[String, List[String]]()
    for((key, values) <- artistiDatasetFull) {
      if(entitiesMoreThanXStatements.contains(key)) {
        filteredDataset += (key -> values.filter(entitiesMoreThanXStatements.contains(_)))
      }
    }
    println(filteredDataset.size)
    println(filteredDataset.values.foldRight(0){(entry, previous) => entry.length + previous})
    return filteredDataset.toMap
  }

}
