package de.tarn_vedra.fhir.search

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.{util => ju}
import scala.math.ScalaNumber
import java.net.URI
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import java.time.LocalDate
import java.util.regex.Pattern

final case class UnknownModifierException(val modifierString: String) 
  extends RuntimeException(s"Unknown modifier $modifierString")
final case class UnsupportedModifierException(val parameterType: ParameterType, val modifier: Modifier) 
  extends RuntimeException(s"Unsupported modifier $modifier on ParameterType $parameterType")
final case class UnsupportedPrefixException(val parameterType: ParameterType, val prefix: Prefix) 
  extends RuntimeException(s"Unsupported prefix $prefix on ParameterType $parameterType")

  
object ValueType {
  type String = java.lang.String
  type Number = ScalaNumber
  type Date = String
  type URI = java.net.URI
  case class Composite(values: Map[String, (Prefix, String)])
  case class Token(system: Option[String], code: Option[String])
  case class Quantity(amount: Number, unit: String)
  
  sealed trait Reference
  case class UriReference(val uri: URI)
  case class IdReference(val id: String, val system: Option[String])
}
  
sealed trait ParameterType {
  type ValueType <: Any

  def parse(value: String): Try[ValueType]
}

object ParameterType {
  case object Number extends ParameterType {
    override type ValueType = ValueType.Number 
    def parse(value: String): Try[ValueType] = Try(BigDecimal.apply(value))
  }
  
  case object Date extends ParameterType {
    override type ValueType = ValueType.Date 
    def parse(value: String): Try[ValueType] = Success(value)
  }
  
  case object String extends ParameterType {
    override type ValueType = ValueType.String 
    def parse(value: String): Try[ValueType] = Success(value)
  }
  
  case object Token extends ParameterType {
    override type ValueType = ValueType.Token 
    def parse(value: String): Try[ValueType] = ???
  }
  
  case object Reference extends ParameterType {
    override type ValueType = ValueType.Reference 
    def parse(value: String): Try[ValueType] = ???
  }
  
  case object Composite extends ParameterType {
    override type ValueType = ValueType.Composite 
    def parse(value: String): Try[ValueType] = ???
  }
  
  case object Quantity  extends ParameterType {
    override type ValueType = ValueType.Quantity 
    def parse(value: String): Try[ValueType] = ???
  }
  
  case object Uri extends ParameterType {
    override type ValueType = ValueType.URI 
    def parse(value: String): Try[ValueType] = ???
  }
  
  case object Special extends ParameterType {
    override type ValueType = ValueType.String 
    def parse(value: String): Try[ValueType] = ???
  }

  val all = Set[ParameterType](Number, Date, String, Token, Reference, Composite, Quantity, Uri, Special)
}

sealed class Modifier(val identifier: String, supportedParameterTypes: Set[ParameterType]) {
  def isSupportedBy(parameterType: ParameterType): Boolean = {
    this.supportedParameterTypes.contains(parameterType)
  }
}

object Modifier {
  // case class Missing(shouldBeMissing: Boolean) extends Modifier("missing") // TODO
  case object Exact      extends Modifier("exact",      supportedParameterTypes = Set(ParameterType.String))
  case object Contains   extends Modifier("contains",   supportedParameterTypes = Set(ParameterType.String))
  case object Text       extends Modifier("text",       supportedParameterTypes = Set(ParameterType.Token))
  case object In         extends Modifier("in",         supportedParameterTypes = Set(ParameterType.Token))
  case object Below      extends Modifier("below",      supportedParameterTypes = Set(ParameterType.Reference, ParameterType.Token, ParameterType.Uri))
  case object Above      extends Modifier("above",      supportedParameterTypes = Set(ParameterType.Reference, ParameterType.Token, ParameterType.Uri))
  case object NotIn      extends Modifier("not-in",     supportedParameterTypes = Set(ParameterType.Token))
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

  case object Equal          extends Prefix("eq", supportedParameterTypes = ParameterType.all)
  case object NotEqual       extends Prefix("ne", supportedParameterTypes = orderedParameterTypes)
  case object GreaterThan    extends Prefix("gt", supportedParameterTypes = orderedParameterTypes)
  case object LessThan       extends Prefix("lt", supportedParameterTypes = orderedParameterTypes)
  case object GreaterOrEqual extends Prefix("ge", supportedParameterTypes = orderedParameterTypes)
  case object LessOrEqual    extends Prefix("le", supportedParameterTypes = orderedParameterTypes)
  case object StartsAfter    extends Prefix("sa", supportedParameterTypes = orderedParameterTypes)
  case object EndsBefore     extends Prefix("eb", supportedParameterTypes = orderedParameterTypes)
  case object Approximately  extends Prefix("ap", supportedParameterTypes = orderedParameterTypes)

  val all = Set[Prefix](Equal, NotEqual, GreaterThan, LessThan, GreaterOrEqual, LessOrEqual, StartsAfter, EndsBefore, Approximately) 

  def apply(modifier: String): Option[Prefix] = all.find(_.identifier == modifier)
}

final case class Parameter[T <: ParameterType](
  rawValue: String, 
  name: String,
  parameterType: ParameterType, 
  modifier: Option[Modifier],
  prefix: Prefix,
  value: String,
  typedValue: T#ValueType
)

object Parameter {
  def parse[T <: ParameterType](parameterType: T, rawValue: String, lhs: String, rhs: String): Try[Parameter[T]] = {
    val (modifier: Option[Modifier], parameterName: String) = lhs.split(":", 2) match {
      case Array(x) => (None, x)
      case Array(x, modifier) => {
        Modifier(modifier) match {
          case None => return Failure(UnknownModifierException(modifier))
          case Some(modifier) if modifier.isSupportedBy(parameterType) => (Some(modifier), x)
          case Some(modifier) => return Failure(UnsupportedModifierException(parameterType, modifier))
        }
      }
    }

    val (prefix, value) = rhs.size match {
      case n if n > 2 => {
        Prefix(rhs.take(2)) match {
          case None => (None, rhs)
          case Some(p) if p.supportsParameterType(parameterType) => (Some(p), rhs.drop(2))
          case Some(prefix) => return Failure(UnsupportedPrefixException(parameterType, prefix))
        }
      }
      case _ => (None, rhs)
    }

    val typedValue = parameterType.parse(value) match {
      case Success(value) => value
      case Failure(exception) => return Failure(exception)
    }

    Success(
      Parameter(
        rawValue = rawValue,
        name = parameterName,
        parameterType = parameterType,
        modifier = modifier,
        prefix = prefix.getOrElse(Prefix.Equal),
        value = value,
        typedValue = typedValue
      )
    )
  }

  def parse[T <: ParameterType](parameterType: T, rawValue: String): Try[Parameter[T]] = {
    rawValue.split("=") match {
      case Array(lhs, rhs) => parse(parameterType, rawValue, lhs, rhs)
    }
  }
}
