package baseline

import breeze.linalg.{DenseVector, sum}
import globals.Namespace
import rdf.{GraphRDF, WikidataPropertyHelper}

/**
  * Created by Espen on 09.11.2016.
  */
object VectorRepresentationCreator {

  def createVectorRepresentation(entity : String): DenseVector[Double] = {
    val entityGraph = new GraphRDF(entity)
    val subjectProperties = entityGraph.getProperties(s = true)
    val propertyIdfMap = PropertyIdfCalculator.getMapFromFile()
    val maxPropertyNumber = Namespace.maxCountForProperties.toString.toInt
    val representation = DenseVector.zeros[Double](maxPropertyNumber * 2)
    for(prop <- subjectProperties) {
      val propertyIdOption = WikidataPropertyHelper.getId(prop)
      propertyIdOption match {
        case Some(a) => representation(a) += propertyIdfMap(prop)
        case None => Unit
      }
    }
    val objectProperties = entityGraph.getProperties(o=true)
    for(prop <- subjectProperties) {
      val propertyIdOption = WikidataPropertyHelper.getId(prop)
      propertyIdOption match {
        case Some(a) => representation(a * 2) += propertyIdfMap(prop)
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

  def compareVectorRepresentationForEntities(entity: String, otherEntities: String*) : Unit = {
    val vectorRepresentationMain = createVectorRepresentation(entity)
    for(other <- otherEntities) {
      val vectorRepresentationOther = createVectorRepresentation(other)
      print(entity + "\tSimilarity to: " + other + "\t = " + calculateCosine(vectorRepresentationMain, vectorRepresentationOther) + "\n")
    }
    println("----------------COMPARISON COMPLETE -----------------")
  }

}
