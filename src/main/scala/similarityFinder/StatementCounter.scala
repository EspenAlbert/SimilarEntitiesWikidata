package similarityFinder

import core.globals.KnowledgeGraph.KnowledgeGraph
import core.globals.{KnowledgeGraph, MyDatasets}
import core.query.Query
import core.query.specific.{QueryFactory, UpdateQueryFactory}
import core.rdf.GraphRDF

/**
  * Created by espen on 03.04.17.
  */
object StatementCounter {

  def countEntityStatements(implicit knowledgeGraph: KnowledgeGraph): Unit = {
    val entitiesQueryString =
      s"""
         |select distinct ?o
         |where {
         |  ?s ?p ?o
         |  filter(isUri(?o))
         |  }
       """.stripMargin
    val entitiesQuery = new Query(()=> entitiesQueryString, MyDatasets.resultsSimilarArtists)
    entitiesQuery.execute()
    val entities : List[String] = entitiesQuery.getResults("o")
    val correctDsEntities = entities.filter(_.startsWith(KnowledgeGraph.getDatasetEntityPrefix(knowledgeGraph)))
    println(s"Correct entities size: ${correctDsEntities.size}")
    correctDsEntities.foreach(e =>{
      val statementCount = new GraphRDF(e).getStatementCountWithoutTypeStatements
      UpdateQueryFactory.addStatementCount(e, statementCount)
    })
  }

}
