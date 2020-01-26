package de.tarn_vedra

import io.lemonlabs.uri.Uri
import io.lemonlabs.uri.Url
import scala.util.Try
import scala.util.Success
import scala.util.Failure

sealed trait ParameterType

object ParameterType {
  case object Number extends ParameterType
  case object Date extends ParameterType
  case object String extends ParameterType
  case object Token extends ParameterType
  case object Reference extends ParameterType
  case object Composite extends ParameterType
  case object Quantity extends ParameterType
  case object Uri extends ParameterType
  case object Special extends ParameterType

  val all = Set[ParameterType](Number, Date, String, Token, Reference, Composite, Quantity, Uri, Special)
}

sealed class Modifier(val identifier: String, supportedParameterTypes: Set[ParameterType]) {
  def isSupportedBy(parameterType: ParameterType): Boolean = {
    this.supportedParameterTypes.contains(parameterType)
  }
}

object Modifier {
  // case class Missing(shouldBeMissing: Boolean) extends Modifier("missing") // TODO
  case object Exact extends Modifier("exact", supportedParameterTypes = Set(ParameterType.String))
  case object Contains extends Modifier("contains", supportedParameterTypes = Set(ParameterType.String))
  case object Text extends Modifier("text", supportedParameterTypes = Set(ParameterType.Token))
  case object In extends Modifier("in", supportedParameterTypes = Set(ParameterType.Token))
  case object Below extends Modifier("below", supportedParameterTypes = Set(ParameterType.Reference, ParameterType.Token, ParameterType.Uri))
  case object Above extends Modifier("above", supportedParameterTypes = Set(ParameterType.Reference, ParameterType.Token, ParameterType.Uri))
  case object NotIn extends Modifier("not-in", supportedParameterTypes = Set(ParameterType.Token))
  case object Identifier extends Modifier("identifier", supportedParameterTypes = Set(ParameterType.Reference))
  // case class ReferenceType(val resourceType: String) extends Modifier(resourceType) // TODO

  val all = Set[Modifier](Exact, Contains, Text, In, Below, Above, NotIn, Identifier)

  def apply(modifier: String): Option[Modifier] = all.find(_.identifier == modifier)
}

sealed class Prefix(val identifier: String, val supportedParameterTypes: Set[ParameterType]) {
  def supportsParameterType(pt: ParameterType) = this.supportedParameterTypes.contains(pt)
}

object Prefix {
  private val orderedParameterTypes = Set[ParameterType](ParameterType.Number, ParameterType.Date, ParameterType.Quantity)

  case object Equal extends Prefix("eq", supportedParameterTypes = ParameterType.all)
  case object NotEqual extends Prefix("ne", supportedParameterTypes = orderedParameterTypes)
  case object GreaterThan extends Prefix("gt", supportedParameterTypes = orderedParameterTypes)
  case object LessThan extends Prefix("lt", supportedParameterTypes = orderedParameterTypes)
  case object GreaterOrEqual extends Prefix("ge", supportedParameterTypes = orderedParameterTypes)
  case object LessOrEqual extends Prefix("le", supportedParameterTypes = orderedParameterTypes)
  case object StartsAfter extends Prefix("sa", supportedParameterTypes = orderedParameterTypes)
  case object EndsBefore extends Prefix("eb", supportedParameterTypes = orderedParameterTypes)
  case object Approximately extends Prefix("ap", supportedParameterTypes = orderedParameterTypes)

  val all = Set[Prefix](Equal, NotEqual, GreaterThan, LessThan, GreaterOrEqual, LessOrEqual, StartsAfter, EndsBefore, Approximately) 

  def apply(modifier: String): Option[Prefix] = all.find(_.identifier == modifier)
}

case class Parameter(
  rawValue: String, 
  name: String,
  parameterType: ParameterType, 
  modifier: Option[Modifier],
  prefix: Prefix,
  value: String
)

object Parameter {
  def parse(parameterType: ParameterType, rawValue: String, lhs: String, rhs: String): Try[Parameter] = {
    val (modifier: Option[Modifier], parameterName: String) = lhs.split(":") match {
      case Array(x) => (None, x)
      case Array(x, modifier) => {
        Modifier(modifier) match {
          case None => return Failure(new RuntimeException(s"Invalid modifier $modifier"))
          case Some(modifier) if modifier.isSupportedBy(parameterType) => (Some(modifier), x)
          case _ => return Failure(new RuntimeException(s"Modifier $modifier is not supported by ParameterType $parameterType"))
        }
        
      }
      case _ => return Failure(new RuntimeException(s"Invalid parameter or modifier: ${lhs}"))
    }

    val (prefix, value) = rhs.size match {
      case n if n > 2 => {
        Prefix(rhs.take(2)) match {
          case None => (None, rhs)
          case Some(p) if p.supportsParameterType(parameterType) => (p, rhs.drop(2))
          case Some(prefix) => return Failure(new RuntimeException(s"Invalid prefix $prefix for ParameterType $parameterType"))
        }
        (Prefix(rhs.take(2)), rhs.drop(2))
      }
      case _ => (None, rhs)
    }

    Success(
      Parameter(
        rawValue = rawValue,
        name = parameterName,
        parameterType = ParameterType.String,
        modifier = modifier,
        prefix = prefix.getOrElse(Prefix.Equal),
        value = value
      )
    )
  }

  def parse(parameterType: ParameterType, rawValue: String): Try[Parameter] = {
    rawValue.split("=") match {
      case Array(lhs, rhs) => parse(parameterType, rawValue, lhs, rhs)
    }
  }
}
