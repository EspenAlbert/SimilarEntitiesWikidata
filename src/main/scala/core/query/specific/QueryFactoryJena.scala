package core.query.specific

import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.globals.MyDatasets
import core.query.QueryServerScala
import org.apache.jena.query.QuerySolution

import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
  * Created by espen on 03.05.17.
  */
object QueryFactoryJena {
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

  abstract class QueryVar(varName:String) {
    def addResult(qs : QuerySolution)
  }
  case class UnknownStringVar(varName:String) extends QueryVar(varName:String) {
    var results = ListBuffer[String]()
    override def addResult(qs : QuerySolution): Unit ={
      val v = qs.get(varName)
      if(v.isURIResource) results.append(qs.getResource(varName).getURI)
      if(v.isLiteral) results.append(qs.getLiteral(varName).getString)
    }
  }
  case class URIVar(varName:String) extends QueryVar(varName:String) {
    var results = ListBuffer[String]()
    override def addResult(qs : QuerySolution): Unit =results.append(qs.getResource(varName).getURI)
  }
  case class LiteralStringVar(varName:String) extends QueryVar(varName:String) {
    var results = ListBuffer[String]()
    override def addResult(qs : QuerySolution): Unit =results.append(qs.getLiteral(varName).getString)
  }
  case class LiteralIntVar(varName:String) extends QueryVar(varName:String){//TODO: Check error during concurrency
    var results= ListBuffer[Int]()
    override def addResult(qs : QuerySolution): Unit ={
      results.append(qs.getLiteral(varName).getInt)
    }
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

}
