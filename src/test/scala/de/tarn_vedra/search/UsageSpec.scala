package de.tarn_vedra.fhir.search

import org.scalatest._
import scala.util.Success
import java.{util => ju}

class UsageSpec extends FlatSpec with Matchers {
  "The parse function" should "behave ergonomical" in {
    import de.tarn_vedra.fhir.search._

    val birthDate = Parameter.parse(ParameterType.Date, "birthDate=ap1995-08-25").get
    birthDate.name          should equal("birthDate")
    birthDate.value         should equal("1995-08-25")
    birthDate.typedValue    should equal(ValueType.Date("1995-08-25"))
    birthDate.parameterType should equal(ParameterType.Date)
    birthDate.prefix        should equal(Prefix.Approximately)
    birthDate.modifier      should equal(None)
    birthDate.rawValue      should equal("birthDate=ap1995-08-25")

    val name = Parameter.parse(ParameterType.String, "name:contains=Frank").get
    name.name          should equal("name")
    name.value         should equal("Frank")
    name.typedValue    should equal("Frank")
    name.parameterType should equal(ParameterType.String)
    name.prefix        should equal(Prefix.Equal)
    name.modifier      should equal(Some(Modifier.Contains))
    name.rawValue      should equal("name:contains=Frank")
  }
}
