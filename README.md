# fhir-parameters

A standalone Scala implementation of [FHIR](http://hl7.org/fhir/) [search parameters](https://www.hl7.org/fhir/search.html).

## Usage

```scala
import de.tarn_vedra.fhir.search._

val birthDate = Parameter.parse(ParameterType.Date, "birthDate=ap1995-08-25").get
birthDate.name          == "birthDate"
birthDate.value         == "1995-08-25"
birthDate.typedValue    == ValueType.Date("1995-08-25")
birthDate.parameterType == ParameterType.Date
birthDate.prefix        == Prefix.Approximately
birthDate.modifier      == None
birthDate.rawValue      == "birthDate=ap1995-08-25"

val name = Parameter.parse(ParameterType.String, "name=Frank").get
name.name          == "name"
name.value         == "Frank"
name.typedValue    == "Frank"
name.parameterType == ParameterType.String
name.prefix        == Prefix.Equal
name.modifier      == Some(Modifier.Contains)
name.rawValue      == "name=Frank"
```
