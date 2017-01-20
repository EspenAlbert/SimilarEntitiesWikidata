package baseline

import breeze.linalg.{DenseVector, sum}
import globals.SimilarPropertyOntology
import rdf.{GraphRDF, WikidataPropertyHelper}

import scala.collection.mutable.ListBuffer

/**
  * Created by Espen on 09.11.2016.
  */
object VectorRepresentationCreator {

  def createVectorRepresentation(entity : String): DenseVector[Double] = {
    val entityGraph = new GraphRDF(entity)
    val subjectProperties = entityGraph.getProperties(s = true)
    val propertyIdfMap = PropertyIdfCalculator.getMapFromFile()
    val maxPropertyNumber = SimilarPropertyOntology.maxPropertyNumber
    val representation = DenseVector.zeros[Double](maxPropertyNumber * 2)
    val invalidProperties = Set("http://www.wikidata.org/entity/P31", "http://www.wikidata.org/entity/P513")
    for(prop <- subjectProperties.filterNot(invalidProperties.contains(_))) {
      val propertyIdOption = WikidataPropertyHelper.getId(prop)
      propertyIdOption match {
        case Some(a) => representation(a) += propertyIdfMap(prop)._1
        case None => Unit
      }
    }
    val objectProperties = entityGraph.getProperties(o=true)
    for(prop <- objectProperties.filterNot(_ == "http://www.wikidata.org/entity/P31")) {
      val propertyIdOption = WikidataPropertyHelper.getId(prop)
      propertyIdOption match {
        case Some(a) => representation(a * 2) += propertyIdfMap(prop)._2
        case None => Unit
      }
    }

    return representation
  }

  def calculateLength(a: DenseVector[Double]) : Double = {
    return sum(a :* a)
  }


  def calculateCosine(vectorRepresentationMain: DenseVector[Double], vectorRepresentationOther: DenseVector[Double]): Double = {
    return (sum(vectorRepresentationMain :* vectorRepresentationOther)) / (calculateLength(vectorRepresentationMain) + calculateLength(vectorRepresentationOther))
  }

  def compareVectorRepresentationForEntities(entity: String, otherEntities: String*) : List[Double] = {
    val vectorRepresentationMain = createVectorRepresentation(entity)
    val listBuffer = ListBuffer[Double]()
    for(other <- otherEntities) {
      val vectorRepresentationOther = createVectorRepresentation(other)
//      print(entity + "\tSimilarity to: " + other + "\t = " + calculateCosine(vectorRepresentationMain, vectorRepresentationOther) + "\n")
      val cosine = calculateCosine(vectorRepresentationMain, vectorRepresentationOther)
      listBuffer.append(cosine)
    }
//    println("----------------COMPARISON COMPLETE -----------------")
    return listBuffer.toList
  }

}
