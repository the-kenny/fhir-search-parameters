package de.tarn_vedra

import io.lemonlabs.uri.Uri
import io.lemonlabs.uri.Url
import scala.util.Try
import scala.util.Success
import scala.util.Failure

sealed class ParameterType(val supportedModifiers: Set[Modifier] = Set())

object ParameterType {
  case object Number extends ParameterType // Search parameter SHALL be a number (a whole number, or a decimal).
  case object Date extends ParameterType // Search parameter is on a date/time. The date format is the standard XML format, though other formats may be supported.
  case object String extends ParameterType(supportedModifiers = Set(Modifier.Exact, Modifier.Contains))
  case object Token extends ParameterType(supportedModifiers = Set(Modifier.Text, Modifier.In, Modifier.Below, Modifier.Above, Modifier.NotIn))
  case object Reference extends ParameterType(supportedModifiers = Set(Modifier.Identifier, Modifier.Above, Modifier.Below)) // A reference to another resource (Reference or canonical).
  case object Composite extends ParameterType // A composite search parameter that combines a search on two values together.
  case object Quantity extends ParameterType // A search parameter that searches on a quantity.
  case object Uri extends ParameterType(supportedModifiers = Set(Modifier.Below, Modifier.Above)) // A search parameter that searches on a URI (RFC 3986).
  case object Special extends ParameterType // Special logic applies to this parameter per the description of the search parameter.

  val all = Set[ParameterType](Number, Date, String, Token, Reference, Composite, Quantity, Uri, Special)
}

sealed class Modifier(val identifier: String) {
  def isSupportedBy(parameterType: ParameterType): Boolean = {
    parameterType.supportedModifiers.contains(this)
  }
}

object Modifier {
  // case class Missing(shouldBeMissing: Boolean) extends Modifier("missing") // TODO
  case object Exact extends Modifier("exact")
  case object Contains extends Modifier("contains")
  case object Text extends Modifier("text")
  case object In extends Modifier("in")
  case object Below extends Modifier("below")
  case object Above extends Modifier("above")
  case object NotIn extends Modifier("not-in")
  case object Identifier extends Modifier("identifier")
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
