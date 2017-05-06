package core.query.specific

import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.MyDatasets
import core.query.QueryServerScala
import core.query.variables.JenaQueryVars._
import org.apache.jena.query.QuerySolution

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
  * Created by espen on 03.05.17.
  */
object QueryFactoryJena {
  def allPropertyDistributionsLocally(entityType: String)(implicit knowledgeGraph: KnowledgeGraph) : immutable.Iterable[(String, Try[Int], Try[Int], Double)] = {
    val queryString = QueryStringFactory.propertyDistributions(entityType)
    val countDomains = LiteralIntOptionVar("cD")
    val countRanges = LiteralIntOptionVar("cR")
    val importanceRatios = LiteralDoubleVar("iR")
    val properties = URIVar("p")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, countDomains, countRanges, importanceRatios, properties)
    val rCountDomains = countDomains.results.toVector
    val rCountRanges = countRanges.results.toVector
    val rImportanceRatios = importanceRatios.results.toVector
    val rProperties = properties.results.toVector
    val expectedDistributions = rCountDomains.size
    assert(expectedDistributions == rCountRanges.size)
    assert(expectedDistributions == rImportanceRatios.size)
    assert(expectedDistributions == rProperties.size)
    return Range(0, expectedDistributions).map(index =>
      (rProperties(index), rCountDomains(index), rCountRanges(index), rImportanceRatios(index)))
  }

  def propertyDistributionLocally(entityType: String, property: String)(implicit knowledgeGraph: KnowledgeGraph) : Try[(Option[Int], Option[Int], Double)] = {
    val queryString = QueryStringFactory.propertyDistribution(entityType, property)
    val countDomain = LiteralIntVar("cD")
    val countRange = LiteralIntVar("cR")
    val importanceRatio = LiteralDoubleVar("iR")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, countDomain, countRange, importanceRatio)
    return Try((countDomain.results.headOption, countRange.results.headOption, importanceRatio.results.head))
  }
  def typePropertyCountLocal(entityType: String, property: String, isDomain: Boolean)(implicit knowledgeGraph: KnowledgeGraph) : Int = {
    val queryString = QueryStringFactory.typePropertyCountLocal(entityType, property, isDomain)
    val count = LiteralIntVar("c")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, count)
    count.results.head
  }

  def countEntitiesOfTypeForProperty(typeEntity: String, isSubject: Boolean, property: String)(implicit knowledgeGraph: KnowledgeGraph) : Int = {
    val queryString = QueryStringFactory.countEntitiesOfTypeForProperty(typeEntity, isSubject, property)
    val count = LiteralIntVar("c")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, count)
    count.results.head
  }

  def propertiesWhereRangeHasType(typeEntity: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString = QueryStringFactory.propertiesWhereRangeHasType(typeEntity)
    val properties = URIVar("p")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, properties)
    properties.results.toList
  }

  def propertiesWhereDomainHasType(typeEntity: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString = QueryStringFactory.propertiesWhereDomainHasType(typeEntity)
    val properties = URIVar("p")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, properties)
    properties.results.toList
  }

  def findAllPossibleTypes()(implicit knowledgeGraph: KnowledgeGraph): List[String] = {
    val queryString = QueryStringFactory.possibleTypes
    val possibleTypes = URIVar("pT")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, possibleTypes)
    possibleTypes.results.toList
  }

  def hierachyLevel(entityType: String)(implicit knowledgeGraph: KnowledgeGraph) : Option[Int] = {
    val queryString = QueryStringFactory.hiearchyLevel(entityType)
    val level = LiteralIntVar("hL")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, level)
    assert(level.results.size < 2, s"Multiple hierarchy levels exists for : $entityType")
    level.results.headOption
  }

  def childrenOf(parent: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString = QueryStringFactory.childrenOf(parent)
    val children = URIVar("c")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, children)
    children.results.toList
  }
  def childrenOfAndChildrenIsType(parent: String)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString = QueryStringFactory.childrenOfAndChildrenIsType(parent)
    val children = URIVar("c")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, children)
    children.results.toList
  }

  def findSubjectsAndProperties(objectValue: String)(implicit knowledgeGraph: KnowledgeGraph): (List[String], List[String]) = {
    val queryString = QueryStringFactory.subjectsAndProperties(objectValue)
    val properties = URIVar("p")
    val subjects = URIVar("s")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, subjects, properties)
    (subjects.results.toList, properties.results.toList)
  }


  def entityTypes(entity : String)(implicit knowledgeGraph: KnowledgeGraph): List[String] = {
    val queryString = QueryStringFactory.allTypesForEntity(entity)
    val types = URIVar("t")
    QueryServerScala.query(MyDatasets.dsWikidata, queryString, types)
    return types.results.toList
  }
  def distinctPropertiesWhereEntityIsObject(entity: String)(implicit knowledgeGraph: KnowledgeGraph): List[String] = {
    val qString = QueryStringFactory.distinctPropertiesWhereObject(entity)
    val properties = URIVar("p")
    QueryServerScala.query(MyDatasets.dsWikidata, qString, properties)
    return properties.results.toList
  }
  def parentsTo(entity : String)(implicit knowledgeGraph: KnowledgeGraph) : Iterable[String] = {
    val queryString = QueryStringFactory.parentsTo(entity)
    val parents = URIVar("p")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, parents)
    return parents.results

  }
  def parentsToParentIsType(entity : String)(implicit knowledgeGraph: KnowledgeGraph) : Iterable[String] = {
    val queryString = QueryStringFactory.parentsToParentIsType(entity)
    val parents = URIVar("p")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, parents)
    return parents.results

  }
  def parentToEntityXStepsAway(entity : String, steps : Int)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString = QueryStringFactory.parentToEntityXStepsAway(entity, steps)
    val parents = URIVar("o")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, parents)
    return parents.results.toList

  }

}
