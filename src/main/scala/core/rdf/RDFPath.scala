package core.rdf


case class RDFPath(startEntity: String, endEntity: String, properties : List[String], isSubjectList : List[Boolean], middleEntities: List[String]) {


  require(properties.length == isSubjectList.length, s"failed to create path, properties and isSubjectList must be of same length!: ${properties} ${isSubjectList} ${middleEntities}")
  require(isSubjectList.length == middleEntities.length+1, s"failed to create path, middleEntities should be shorter than properties: ${properties} ${isSubjectList} ${middleEntities}")

  val path = toString

  override def toString: String = {
    RDFPath.createPath(this)
  }
}

object RDFPath {
  def length(path: RDFPath) : Int = {
    if(path == null) return 1
    else {
      path.properties.length
    }
  }

  def createPath(path: RDFPath): String = {
    val RDFPath(startE, endE, props, isSubjs, middleEs) = path
    val pathLength = path.properties.size
    Range(0, pathLength).map(_ match {
      case 0 if pathLength == 1 => createPathLength1(startE, props.head, isSubjs.head, endE)
      case 0 if pathLength > 1 => createPathLength1(path.startEntity, path.properties.head, path.isSubjectList.head, path.middleEntities.head)
      case a if pathLength == a+1 => s"${isSubjs(a)},${props(a)},$endE"
      case a if pathLength > a+1 => s"${isSubjs(a)},${props(a)},${middleEs(a)}"
    })
      .mkString(",")
  }

  def createPathLength2(qEntity: String, isSubject: Boolean, property: String, middleNode: String, middleNodeIsSubj: Boolean, property2: String, foundEntity: String): String = {
    createPathLength1(qEntity, property, isSubject, middleNode) + s",$middleNodeIsSubj,$property2,$foundEntity"
  }

  def createPathLength1(qEntity : String, property : String, isSubject : Boolean, foundEntity : String): String = {
    s"$qEntity,$isSubject,$property,$foundEntity"
  }
  def extend(path: RDFPath, subject: String, property: String, objectValue: String): RDFPath = {
    if(path == null) return new RDFPath(subject, objectValue, property::Nil, true::Nil, Nil)
    else {
      //Move end entity to middle entities add new entity
      val oldEndEntity = path.endEntity
      val isSubject = oldEndEntity == subject
      val endEntity = if (isSubject) objectValue else subject
      new RDFPath(path.startEntity, endEntity, path.properties ++ List(property), path.isSubjectList ++ List(isSubject), path.middleEntities ++ List(oldEndEntity))
    }

  }
}