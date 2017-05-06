package core.query.variables

import org.apache.jena.query.QuerySolution

import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
  * Created by espen on 06.05.17.
  */
object JenaQueryVars {
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
  case class LiteralIntOptionVar(varName:String) extends QueryVar(varName:String){//TODO: Check error during concurrency
  var results= ListBuffer[Try[Int]]()
    override def addResult(qs : QuerySolution): Unit ={
      results.append(Try(qs.getLiteral(varName).getInt))
    }
  }
  case class LiteralDoubleVar(varName:String) extends QueryVar(varName:String){
  var results= ListBuffer[Double]()
    override def addResult(qs : QuerySolution): Unit ={
      results.append(qs.getLiteral(varName).getDouble)
    }
  }

}
