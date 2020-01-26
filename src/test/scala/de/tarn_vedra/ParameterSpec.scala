package de.tarn_vedra

import org.scalatest._
import scala.util.Success

class ParameterSpec extends FlatSpec with Matchers {
  "The parse function" should "handle rawValue correctly" in {
    Parameter.parse(ParameterType.String, "name=foo").get.rawValue shouldBe "name=foo"
  }

  "The parse function" should "parse the parameter name" in {
    Parameter.parse(ParameterType.String, "name=foo").get.name shouldBe "name"
  }

  "The parse function" should "handle modifiers" in {
    Parameter.parse(ParameterType.String, "name=foo").get.modifier shouldBe None
    Parameter.parse(ParameterType.String, "name:contains=foo").get.modifier shouldBe Some(Modifier.Contains)
    Parameter.parse(ParameterType.String, "name:invalidModifier=foo").isFailure shouldBe true
  }

  "The parse function" should "reject unsupported modifiers for a given ParameterType" in {
    Parameter.parse(ParameterType.Date, "birthDate:exact=2019-01-01").isFailure shouldBe true
    Parameter.parse(ParameterType.String, "name:below=foo").isFailure shouldBe true
    Parameter.parse(ParameterType.Number, "age:contains=42").isFailure shouldBe true
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
    Parameter.parse(ParameterType.String, "name=ltSomeName").isFailure shouldBe true
    Parameter.parse(ParameterType.String, "name=apSomeName").isFailure shouldBe true
  }
}
