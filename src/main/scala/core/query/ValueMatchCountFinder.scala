package core.query

import core.globals.KnowledgeGraphs.KnowledgeGraph
import core.query.specific.{QueryFactory, UpdateQueryFactory}

import scala.util.{Failure, Success}

/**
  * Created by espen on 04.05.17.
  */
object ValueMatchCountFinder {
  var thresholdCountStoreValueMatchCount = 1000


  def valueIsAPotentialValueMatchFindCount(value: String, property: String, valueIsSubject: Boolean, onlyGreaterThanCounts : Boolean = true)(implicit knowledgeGraph: KnowledgeGraph): Option[Int] = {
    if(!value.startsWith("http")) return None
    QueryFactory.getValueMatchFromExistingDb(value, property) match {
      case Some(s) =>return Some(s)
      case None => {

        val countFromDs = if (!valueIsSubject) QueryFactory.findCountForPropertyWithValue(property, value) else
          QueryFactory.findCountForPropertyWithSubject(property, value)
        countFromDs match {
          case Success(count) => {
            if(!onlyGreaterThanCounts || count > thresholdCountStoreValueMatchCount)UpdateQueryFactory.updateValueCount(property, value, count) //Stores the value so we don't have to do the full count again..
            return Some(count)
          }
          case Failure(m) => {
            println(s"Unable to find count for: $property with value: $value isSubject: $valueIsSubject\n Failure message: $m")
            return None
          }
        }
      }
    }
  }

}
