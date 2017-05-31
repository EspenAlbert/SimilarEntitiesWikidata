package core.query.specific

import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.{MyDatasets, SimilarPropertyOntology}
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
  def objectsWithSubjectOfEntityTypeForProperty(property: String, domainType: String, isCommonType: Boolean)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= noChangeToQueryMethod): List[(String, String)] = {
    val queryString = adjustQuery(QueryStringFactory.objectsWithSubjectOfEntityTypeForProperty(property, domainType, isCommonType))
    val subjects = URIVar("s")
    val objects = URIVar("o")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, subjects, objects)
    return (subjects.results zip objects.results).toList
  }

  def subjectsWithObjectsOfEntityTypeForProperty(property: String, rangeType: String, isCommonType: Boolean)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= noChangeToQueryMethod): List[(String, String)] = {
    val queryString = adjustQuery(QueryStringFactory.subjectsWithObjectsOfEntityTypeForProperty(property, rangeType, isCommonType))
    val subjects = URIVar("s")
    val objects = URIVar("o")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, subjects, objects)
    return (subjects.results zip objects.results).toList
  }

  def performCountQuery(queryString: String)(implicit knowledgeGraph: KnowledgeGraph): Int = {
    val count = LiteralIntVar("c")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, count)
    return count.results.head
  }


  def findOrderedCountForTypes(isDomainProperties: Iterable[String], isRangeProperties: Iterable[String], numberOfComparableTypes : Int=10)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString = QueryStringFactory.findOrderedCountsForTypes(isDomainProperties, isRangeProperties, numberOfComparableTypes)
    val topTypes = URIVar("s")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, topTypes)
    topTypes.results.toList
  }
  def findOrderedSumOfRatioForTypes(isDomainProperties: Iterable[String], isRangeProperties: Iterable[String], numberOfComparableTypes : Int=10)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString = QueryStringFactory.findOrderedSumOfRatioForTypes(isDomainProperties, isRangeProperties, numberOfComparableTypes)
    val topTypes = URIVar("s")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, topTypes)
    topTypes.results.toList
  }

  def entitiesOfProperty(property: String, isSubject: Boolean)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= noChangeToQueryMethod) : List[String]= {
    val queryString = adjustQuery(QueryStringFactory.entitiesOfProperty(property, isSubject))
    val entities = URIVar("e")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, entities)
    entities.results.toList
  }

  def subjectsOfEntityTypeForProperty(property: String, entityType: String, commonType: Boolean = false)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= noChangeToQueryMethod): List[String] = {
    val queryString = adjustQuery(QueryStringFactory.subjectsOfPropertyWithEntityType(property, entityType, commonType))
    val subjects = URIVar("s")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, subjects)
    subjects.results.toList
  }

  private val noChangeToQueryMethod = (s: String) => s

  def objectsOfEntityTypeForProperty(property: String, entityType: String, commonType: Boolean = false)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= noChangeToQueryMethod): List[String] = {
    val queryString = adjustQuery(QueryStringFactory.objectsOfPropertyWithEntityType(property, entityType, commonType))
    val objects = URIVar("o")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, objects)
    objects.results.toList
  }

  def entityTypesForValues(entity: String, property: String, isSubject: Boolean)(implicit knowledgeGraph: KnowledgeGraph): List[String] = {
    val queryString = QueryStringFactory.entityTypesForValuesOf(entity, property, isSubject)
    val entityTypes = URIVar("t")
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, entityTypes)
    entityTypes.results.toList
  }

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

  def objectsWithPropertyAndSubject(property: String, subject: String)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= noChangeToQueryMethod): List[String] = {
    val queryString = adjustQuery(QueryStringFactory.objectsWithPropertyAndSubject(property, subject))
    val objects = UnknownStringVar("o")//TODO: Test with different datatypes: Dates, ints, doubles, uris, etc.
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, objects)
    return objects.results.toList
  }

  def subjectsWithPropertyAndValue(property: String, objectValue: String)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= noChangeToQueryMethod): List[String] = {
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


  def propertiesForWhereEntityIsSubject(id: String, distinct : Boolean = true)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= noChangeToQueryMethod): Iterable[String] = {
    val queryString = adjustQuery(QueryStringFactory.propertiesForWhereEntityIsSubject(id, distinct))
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


  def propertyDistributionLocally(entityType: String, property: String)(implicit knowledgeGraph: KnowledgeGraph) : (Try[Int], Try[Int], Try[Double], Try[Double]) = {
    val queryString = QueryStringFactory.propertyDistribution(entityType, property)
    val countDomain = LiteralIntOptionVar("cD")
    val countRange = LiteralIntOptionVar("cR")
    val typePropertyRatioDomain = LiteralDoubleOptionVar("tprD")
    val typePropertyRatioRange = LiteralDoubleOptionVar("tprR")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, countDomain, countRange, typePropertyRatioDomain, typePropertyRatioRange)
    return (countDomain.results.head, countRange.results.head, typePropertyRatioDomain.results.head, typePropertyRatioRange.results.head)
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
    val queryString = QueryStringFactory.childrenOfAndChildrenOrParentIsType(parent)
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
    QueryServerScala.query(DatasetInferrer.getDataset(queryString), queryString, types)
    return types.results.toList
  }
  def distinctPropertiesWhereEntityIsObject(entity: String, distinct: Boolean = true)(implicit knowledgeGraph: KnowledgeGraph, adjustQuery: String => String= noChangeToQueryMethod): List[String] = {
    val qString = adjustQuery(QueryStringFactory.distinctPropertiesWhereObject(entity, distinct))
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
  def childrenOfEntityXStepsAway(entity: String, steps: Int)(implicit knowledgeGraph: KnowledgeGraph) : List[String] = {
    val queryString = QueryStringFactory.childrenOfEntityXStepsAway(entity, steps)
    val children = URIVar("o")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, children)
    return children.results.toList

  }
  def numberOfTypesWithPropertyDistributionLocally(implicit knowledgeGraph: KnowledgeGraph) : Int = {
    val queryString = QueryStringFactory.numberOfTypesWithPropertyDistributionLocally()
    val count = LiteralIntVar("c")
    val dataset = DatasetInferrer.getDataset(queryString)
    QueryServerScala.query(dataset, queryString, count)
    return count.results.head

  }

}
