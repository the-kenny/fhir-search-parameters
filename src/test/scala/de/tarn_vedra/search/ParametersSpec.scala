package de.tarn_vedra.fhir.search

import org.scalatest._
import scala.util.Success
import java.{util => ju}

class ParameterTypeSpec extends FlatSpec with Matchers {
  "The parse function of Date" should "parse" ignore {
    ???
  }

  "The parse function of Number" should "parse" in {
    import ParameterType.Number.parse
    import ValueType.Number
    
    // [parameter]=100	Values that equal 100, to 3 significant figures precision, so this is actually searching for values in the range [99.5 ... 100.5)
    parse("100").get shouldBe Number(BigDecimal(100), 0.5)
    parse("-100").get shouldBe Number(BigDecimal(-100), 0.5)

    parse("100.0").get shouldBe Number(BigDecimal(100), 0.05)
    parse("-100.0").get shouldBe Number(BigDecimal(-100), 0.05)

    // [parameter]=100.00	Values that equal 100, to 5 significant figures precision, so this is actually searching for values in the range [99.995 ... 100.005)
    parse("100.00").get shouldBe Number(BigDecimal(100), 0.005)
    parse("-100.00").get shouldBe Number(BigDecimal(-100), 0.005)

    // [parameter]=1e2	Values that equal 100, to 1 significant figures precision, so this is actually searching for values in the range [95 ... 105)
    parse("1e2").get shouldBe Number(BigDecimal(100), 5.0)
    parse("-1e2").get shouldBe Number(BigDecimal(-100), 5.0)

    parse("1.1e2").get shouldBe Number(BigDecimal(110), 0.5)
    parse("-1.1e2").get shouldBe Number(BigDecimal(-110), 0.5)

    parse("11931.34453245").get shouldBe Number(BigDecimal(11931.34453245), 0.000000005)
    parse("0.34453245").get shouldBe Number(BigDecimal(0.34453245), 0.000000005)
    parse("0.00000000001").get shouldBe Number(BigDecimal("0.00000000001"), 0.000000000005)
                 
    parse("asdf").isFailure shouldBe true
    parse("infinity").isFailure shouldBe true
    parse("").isFailure shouldBe true
  }

  "The parse function of String" should "parse" ignore {
    ???
  }

  "The parse function of Token" should "parse" ignore {
    ???
  }

  "The parse function of Reference" should "parse" ignore {
    ???
  }

  "The parse function of Composite" should "parse" ignore {
    ???
  }

  "The parse function of Quantity" should "parse" ignore {
    ???
  }

  "The parse function of Uri" should "parse" ignore {
    ???
  }

  "The parse function of Special" should "parse" ignore {
    ???
  }

}

class ParameterSpec extends FlatSpec with Matchers {
  "The parse function" should "handle rawValue correctly" in {
    Parameter.parse(ParameterType.String, "name=foo").get.rawValue shouldBe "name=foo"
  }

  "The parse function" should "handle parameterType correctly" in {
    Parameter.parse(ParameterType.String, "name=foo").get.parameterType shouldBe ParameterType.String
  }

  "The parse function" should "parse the parameter name" in {
    Parameter.parse(ParameterType.String, "name=foo").get.name shouldBe "name"
  }

  "The parse function" should "handle modifiers" in {
    Parameter.parse(ParameterType.String, "name=foo").get.modifier shouldBe None
    Parameter.parse(ParameterType.String, "name:contains=foo").get.modifier shouldBe Some(Modifier.Contains)
    Parameter.parse(ParameterType.String, "name:invalidModifier=foo").failed.get shouldBe a[UnknownModifierException]
  }

  "The parse function" should "reject unsupported modifiers for a given ParameterType" in {
    Parameter.parse(ParameterType.Date, "birthDate:exact=2019-01-01").failed.get shouldBe a[UnsupportedModifierException]
    Parameter.parse(ParameterType.String, "name:below=foo").failed.get shouldBe a[UnsupportedModifierException]
    Parameter.parse(ParameterType.Number, "age:contains=42").failed.get shouldBe a[UnsupportedModifierException]
  }

  "The parse function" should "handle prefixes" in {
    Parameter.parse(ParameterType.Date, "birthDate=2019-01-01").get.prefix shouldBe Prefix.Equal
    Parameter.parse(ParameterType.Date, "birthDate=xxx2019-01-01").get.prefix shouldBe Prefix.Equal
    Parameter.parse(ParameterType.Date, "birthDate=lt2019-01-01").get.prefix shouldBe Prefix.LessThan
    Parameter.parse(ParameterType.Date, "birthDate=ap2019-01-01").get.prefix shouldBe Prefix.Approximately
  }

  "The parse function" should "reject prefixes not allowed for the parameter type" in {
    Parameter.parse(ParameterType.String, "name=SomeName").get.prefix shouldBe Prefix.Equal
    Parameter.parse(ParameterType.String, "name=eqSomeName").get.prefix shouldBe Prefix.Equal
    // String parameters don't support `lt` or `ap`
    Parameter.parse(ParameterType.String, "name=ltSomeName").failed.get shouldBe a[UnsupportedPrefixException]
    Parameter.parse(ParameterType.String, "name=apSomeName").failed.get shouldBe a[UnsupportedPrefixException]
  }

  "The experimental typedValue field" should "have the correct data type" in {
    Parameter.parse(ParameterType.Number, "age=42.5").get.typedValue shouldBe a[de.tarn_vedra.fhir.search.ValueType.Number]
    Parameter.parse(ParameterType.Number, "age=42.5").get.typedValue shouldBe de.tarn_vedra.fhir.search.ValueType.Number(BigDecimal(42.5), 0.05)

    Parameter.parse(ParameterType.String, "name=SomeName").get.typedValue shouldBe a[String]
    Parameter.parse(ParameterType.String, "name=SomeName").get.typedValue shouldBe "SomeName"

    Parameter.parse(ParameterType.Date, "birthDate=1995-01-20").get.typedValue shouldBe a[String]
    Parameter.parse(ParameterType.Date, "birthDate=1995-01-20").get.typedValue shouldBe "1995-01-20"
    Parameter.parse(ParameterType.Date, "birthDate=1995-01-").get.typedValue shouldBe "1995-01-"
    Parameter.parse(ParameterType.Date, "birthDate=1995-").get.typedValue shouldBe "1995-"
  }
}
