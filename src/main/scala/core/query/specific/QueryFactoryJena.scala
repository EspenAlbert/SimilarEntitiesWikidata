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
  def highValueMatchesForEntity(entity: String)(implicit knowledgeGraph: KnowledgeGraph): List[String] = {
    val queryString = QueryStringFactory.highValueMatchesForEntity(entity)
    val properties = URIVar("p")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, properties)
    properties.results.toList
  }

  def propertiesAndCountsForType(entityType: String, isSubject: Boolean, thresholdCount: Int)(implicit knowledgeGraph: KnowledgeGraph): List[(String, Int)] = {
    val queryString = QueryStringFactory.propertiesAndCountsForType(entityType, thresholdCount, isSubject)
    val properties = URIVar("p")
    val counts = LiteralIntVar("c")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, properties, counts)
    return properties.results.zip(counts.results).toList
  }

  def objectsWithPropertyAndSubject(property: String, subject: String)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= (s: String) => s): List[String] = {
    val queryString = adjustQuery(QueryStringFactory.objectsWithPropertyAndSubject(property, subject))
    val objects = UnknownStringVar("o")//TODO: Test with different datatypes: Dates, ints, doubles, uris, etc.
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, objects)
    return objects.results.toList
  }

  def subjectsWithPropertyAndValue(property: String, objectValue: String)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= (s: String) => s): List[String] = {
    val queryString = adjustQuery(QueryStringFactory.subjectsWithPropertyAndValue(property, objectValue))
    val subjects = URIVar("s")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, subjects)
    return subjects.results.toList
  }

  def datasetSize(dataset: String) : Int = {
    val queryString = QueryStringFactory.countTriples()
    val count = LiteralIntVar("c")
    QueryServerScala.query(dataset, queryString, count)
    return count.results.head
  }


  def typesWithMoreThanThresholdEntities(thresholdForStoringPropertyDistributionsLocally: Int)(implicit knowledgeGraph: KnowledgeGraph): List[(String, Int)] = {
    val queryString = QueryStringFactory.typesWithGreaterThanCount(thresholdForStoringPropertyDistributionsLocally)
    val types = URIVar("t")
    val counts = LiteralIntVar("c")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, types, counts)
    return types.results.zip(counts.results).toList
  }

  def domainAndRangeTypeWithCountsForProperty(property: String) (implicit knowledgeGraph: KnowledgeGraph): Iterable[(String, Option[Int], Option[Int])] = {
    val queryString = QueryStringFactory.domainAndRangeTypeWithCountsForProperty(property)
    val types = URIVar("eT")
    val countDomains = LiteralIntOptionVar("cD")
    val countRanges = LiteralIntOptionVar("cR")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, types, countDomains, countRanges)
    val rTypes = types.results.toVector
    val rCountDomains = countDomains.results.toVector
    val rCountRanges = countRanges.results.toVector
    val expectedDistributions = rTypes.size
    assert(expectedDistributions == rCountDomains.size)
    assert(expectedDistributions == rCountRanges.size)
    return Range(0, expectedDistributions).map(index =>
      (rTypes(index), rCountDomains(index).toOption,rCountRanges(index).toOption))
  }


  def propertiesForWhereEntityIsSubject(id: String)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= (s: String) => s): Iterable[String] = {
    val queryString = adjustQuery(QueryStringFactory.propertiesForWhereEntityIsSubject(id))
    val properties = URIVar("p")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, properties)
    return properties.results
  }

  def comparableTypesPropertyDistribution(domainProperties: Iterable[String], rangeProperties: Iterable[String])(implicit knowledgeGraph: KnowledgeGraph) : Iterable[(String, Int)] = {
    val queryString = QueryStringFactory.comparableTypesPropertyDistribution(domainProperties, rangeProperties)
    val types = URIVar("t")
    val counts = LiteralIntVar("c")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, types, counts)
    return types.results.zip(counts.results)
  }

  def allPropertyDistributionsLocally(entityType: String)(implicit knowledgeGraph: KnowledgeGraph) : immutable.Iterable[(String, Try[Double], Try[Int], Try[Double], Try[Int])] = {
    val queryString = QueryStringFactory.propertyDistributions(entityType)
    val countDomains = LiteralIntOptionVar("cD")
    val countRanges = LiteralIntOptionVar("cR")
    val typePropertyRatioDomain = LiteralDoubleOptionVar("tprD")
    val typePropertyRatioRange = LiteralDoubleOptionVar("tprR")
    val properties = URIVar("p")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, countDomains, countRanges, typePropertyRatioDomain,typePropertyRatioRange, properties)
    val rCountDomains = countDomains.results.toVector
    val rCountRanges = countRanges.results.toVector
    val rTypePropertyRatioDomain = typePropertyRatioDomain.results.toVector
    val rTypePropertyRatioRange = typePropertyRatioRange.results.toVector
    val rProperties = properties.results.toVector
    val expectedDistributions = rCountDomains.size
    assert(expectedDistributions == rCountRanges.size)
    assert(expectedDistributions == rTypePropertyRatioDomain.size)
    assert(expectedDistributions == rTypePropertyRatioRange.size)
    assert(expectedDistributions == rProperties.size)
    return Range(0, expectedDistributions).map(index =>
      (rProperties(index), rTypePropertyRatioDomain(index), rCountDomains(index),rTypePropertyRatioRange(index), rCountRanges(index)))
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
  def distinctPropertiesWhereEntityIsObject(entity: String)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= (s: String) => s): List[String] = {
    val qString = adjustQuery(QueryStringFactory.distinctPropertiesWhereObject(entity))
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
