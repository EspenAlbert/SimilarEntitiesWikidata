package core.globals

/**
  * Created by espen on 20.02.17.
  */

abstract class PropertyType
case class QuantityPropertyType() extends PropertyType
case class DateTimePropertyType() extends PropertyType
case class StringPropertyType() extends PropertyType
case class UrlPropertyType() extends PropertyType
case class ItemPropertyType() extends PropertyType
case class GlobeCoordinatePropertyType() extends PropertyType

object PropertyType {
    implicit def stringFromPropertyType(pType: PropertyType): String = {
      return pType.toString
    }

    implicit def stringToPropType(pTypeString: String): PropertyType = {
      pTypeString match {
        case x if (x == QuantityPropertyType().toString) => return QuantityPropertyType()
        case x if (x == DateTimePropertyType().toString) => return DateTimePropertyType()
        case x if (x == StringPropertyType().toString) => return StringPropertyType()
        case x if (x == UrlPropertyType().toString) => return UrlPropertyType()
        case x if (x == ItemPropertyType().toString) => return ItemPropertyType()
        case x if (x == GlobeCoordinatePropertyType().toString) => return GlobeCoordinatePropertyType()
      }
    }
  }