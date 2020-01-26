# fhir-parameters

A standalone Scala implementation of [FHIR](http://hl7.org/fhir/) [search parameters](https://www.hl7.org/fhir/search.html).

## Usage

```scala
import de.tarn_vedra.fhir.search._

val birthDate = Parameter.parse(ParameterType.Date, "birthDate=1995-08-25").get
birthDate.name          == "birthDate"
birthDate.value         == "1995-08-25"
birthDate.parameterType == ParameterType.Date
birthDate.prefix        == Prefix.Equal
birthDate.modifier      == None
birthDate.rawValue      == "birthDate=1995-08-25"

val name = Parameter.parse(ParameterType.String, "name=apFrank").get
name.name          == "name"
name.value         == "Frank"
name.parameterType == ParameterType.String
name.prefix        == Prefix.Approximately
name.modifier      == None
name.rawValue      == "name=apFrank"
```
