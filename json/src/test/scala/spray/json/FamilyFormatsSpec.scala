// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package spray.json

import org.scalatest._
import shapeless._
import java.util.UUID

// Example domain models used in the tests. Note that the domain model
// and formatters are defined in sibling packages.
package examples {
  sealed trait SimpleTrait
  case class Foo(s: String)         extends SimpleTrait
  case class Bar()                  extends SimpleTrait
  case object Baz                   extends SimpleTrait
  case class Faz(o: Option[String]) extends SimpleTrait

  sealed trait SubTrait extends SimpleTrait
  case object Fuzz      extends SubTrait

  sealed trait Spiel
  case object Buzz extends Spiel

  case class Schpugel(v: String) // I asked my wife to make up a word
  case class Smim(v: String)     // I should stop asking my wife to make up words

  sealed trait Smash
  case class Flooma(label: String) extends Smash
  case class Blam(label: String)   extends Smash

  sealed trait Cloda
  case class Plooba(thing: String) extends Cloda // *sigh*

  object Quack
  case class Huey(duck: Quack.type, witch: Option[Quack.type])
  case class Dewey(duck: Quack.type, witch: Option[Quack.type])
  case class Louie(duck: Quack.type, witch: Option[Quack.type])
  case class Bluey(duck: Quack.type, witch: Option[Quack.type])

  case class Mickey(mouse: String, num: Int = 3)
  case class Billy(thing: Int,
                   label: String,
                   other: Int = 5,
                   mickey: Mickey = Mickey("mouse", 3))

  sealed trait Paff
  case class Piff(s: String)                                      extends Paff
  case class Mozz(n: String, m: Int = 4, lal: Lal = Paf("hello")) extends Paff

  sealed trait Lal
  case class Paf(label: String, other: String = "other") extends Lal

  // I love monkeys, you got a problem with that?
  sealed trait Primates
  sealed trait Strepsirrhini     extends Primates
  sealed trait Haplorhini        extends Primates
  sealed trait Tarsiiformes      extends Haplorhini
  case object Tarsiidae          extends Tarsiiformes
  sealed trait Simiiformes       extends Haplorhini
  sealed trait Platyrrhini       extends Simiiformes
  case object Callitrichidae     extends Platyrrhini
  case object Cebidae            extends Platyrrhini
  case object Aotidae            extends Platyrrhini
  case object Pitheciidae        extends Platyrrhini
  case object Atelidae           extends Platyrrhini
  sealed trait Catarrhini        extends Simiiformes
  sealed trait Cercopithecoidea  extends Catarrhini
  case object Cercopithecidae    extends Cercopithecoidea
  sealed trait Hominoidea        extends Catarrhini
  case object Hylobatidae        extends Hominoidea
  case class Hominidae(id: UUID) extends Hominoidea

  // recursive cat
  case class Cat(nick: String, tail: Option[Cat] = None)
}

trait LowPriorityUserFormats {
  import examples._
  ///////////////////////////////////////////////
  // non-trivial user-defined JsonFormat (not RootJsonFormat)
  //
  // This is in a lower priority implicit scope than the familyFormats
  // (caveat #2), so we have two options to force its visibility:
  //
  // 1. do the same trick as with SymbolJsonFormat
  // 2. shadow familyFormat with a non-implicit variant (see below)
  //
  // Most usecases will prefer option 1. Note that the same trick is
  // needed for JsonFormats and RootJsonFormats, there is nothing
  // special about this being JsonFormat.
  implicit val SmashFormat: JsonFormat[Smash] = new JsonFormat[Smash] {
    def read(json: JsValue): Smash = json match {
      case obj: JsObject =>
        obj.fields.head match {
          case ("flooma", JsString(label)) => Flooma(label)
          case ("blam", JsString(label))   => Blam(label)
          case _ =>
            deserializationError("expected (kind,JsString), got " + json)
        }
      case _ => deserializationError("expected JsString, got " + json)
    }
    def write(obj: Smash): JsValue = obj match {
      case Flooma(label) => JsObject("flooma" -> JsString(label))
      case Blam(label)   => JsObject("blam"   -> JsString(label))
    }
  }
}

object ExamplesFormats
    extends DefaultJsonProtocol
    with FamilyFormats
    with LowPriorityUserFormats {
  import examples._

  // WORKAROUND caveat 2
  implicit val highPrioritySymbolFormat: JsonFormat[Symbol] = symbol
  implicit val highPrioritySmashFormat: JsonFormat[Smash]   = SmashFormat

  ///////////////////////////////////////////////
  // Example of "explicit implicit" for performance
  implicit val SimpleTraitFormat: RootJsonFormat[SimpleTrait] = cachedImplicit

  ///////////////////////////////////////////////
  // user-defined hinting
  implicit val SubTraitHint: FlatCoproductHint[SubTrait] =
    new FlatCoproductHint[SubTrait]("hint")
  implicit val SpielHint: NestedCoproductHint[Spiel] =
    new NestedCoproductHint[Spiel]

  ///////////////////////////////////////////////
  // user-defined field naming rules
  implicit val ClodaHint: FlatCoproductHint[Cloda] =
    new FlatCoproductHint[Cloda]("TYPE") {
      override def fieldName(orig: String): String = orig.toUpperCase
    }
  implicit val PloobaHint: ProductHint[Plooba] = new ProductHint[Plooba] {
    override def fieldName[K <: Symbol](k: K): String = k.name.toUpperCase
  }
  ///////////////////////////////////////////////
  // user-defined /missing value rules
  implicit val HueyHint: ProductHint[Huey] = new ProductHint[Huey] {
    override def nulls = AlwaysJsNull
  }
  implicit val DeweyHint: ProductHint[Dewey] = new ProductHint[Dewey] {
    override def nulls = JsNullNotNone
  }
  implicit val LouieHint: ProductHint[Louie] = new ProductHint[Louie] {
    override def nulls = NeverJsNull
  }
  implicit val BlueyHint: ProductHint[Bluey] = new ProductHint[Bluey] {
    override def nulls = AlwaysJsNullTolerateAbsent
  }
  implicit val QuackFormat: JsonFormat[Quack.type] =
    JsonFormat.instance[Quack.type](_ => JsNull) {
      // needed something that would serialise to JsNull for testing
      case JsNull => Quack
      case other  => deserializationError(s"unexpected $other")
    }

  implicit val TestHint: ProductHint[Billy] = new ProductHint[Billy] {
    override def nulls = UseDefaultJsNull
  }
  implicit val MozzHint: ProductHint[Mozz] = new ProductHint[Mozz] {
    override def nulls = UseDefaultJsNull
  }
  implicit val PafHint: ProductHint[Paf] = new ProductHint[Paf] {
    override def nulls = UseDefaultJsNull
  }
  ///////////////////////////////////////////////
  // user-defined JsonFormat
  implicit val SchpugelFormat: JsonFormat[Schpugel] =
    JsonFormat.instance[Schpugel](s => JsString(s.v)) {
      case JsString(v) => Schpugel(v)
      case other       => deserializationError(s"unexpected $other")
    }

  ///////////////////////////////////////////////
  // user-defined RootJsonFormat
  implicit val SmimFormat: RootJsonFormat[Smim] =
    RootJsonFormat.instance[Smim](s => JsObject("smim" -> JsString(s.v))) {
      case JsObject(els) if els.contains("smim") =>
        els("smim") match {
          case JsString(v) => Smim(v)
          case other       => deserializationError(s"unexpected $other")
        }
      case other => deserializationError(s"unexpected $other")
    }
}

class FamilyFormatsSpec
    extends FlatSpec
    with Matchers
    with SprayJsonTestSupport {
  import examples._
  import ExamplesFormats._

  "FamilyFormats" should "support case objects" in {
    roundtrip(Baz, "{}")
  }

  it should "support symbols, provided by base spray-json" in {
    // any use of Symbol seems to need the hack above. Known caveat.
    roundtrip('foo, """"foo"""")
  }

  it should "support case classes" in {
    roundtrip(Foo("foo"), """{"s":"foo"}""")
    roundtrip(Bar(), "{}")
  }

  it should "support recursive case classes" in {
    roundtrip(
      Cat(
        "foo",
        Some(
          Cat(
            "bar",
            Some(Cat("baz"))
          )
        )
      ),
      """{"nick":"foo","tail":{"nick":"bar","tail":{"nick":"baz"}}}"""
    )
  }

  it should "support optional parameters on case classes" in {
    roundtrip(Faz(Some("meh")), """{"o":"meh"}""") // note uses optionFormat, not familyFormat
    roundtrip(Faz(None), "{}")                     // should be omitted, not "null"
  }

  it should "fail when missing required fields" in {
    intercept[DeserializationException] {
      """{}""".parseJson.convertTo[Foo]
    }
  }

  it should "support simple sealed families" in {
    roundtrip(Foo("foo"): SimpleTrait, """{"type":"Foo","s":"foo"}""")
    roundtrip(Bar(): SimpleTrait, """{"type":"Bar"}""")
    roundtrip(Baz: SimpleTrait, """{"type":"Baz"}""")
    roundtrip(Fuzz: SimpleTrait, """{"type":"Fuzz"}""")
  }

  it should "fail when missing required coproduct disambiguators" in {
    intercept[DeserializationException] {
      """{"s":"foo"}""".parseJson.convertTo[SimpleTrait]
    }
  }

  it should "support custom coproduct keys" in {
    roundtrip(Fuzz: SubTrait, """{"hint":"Fuzz"}""")
    roundtrip(Buzz: Spiel, """{"Buzz":{}}""")
  }

  it should "support custom coproduct field naming rules" in {
    roundtrip(Plooba("poo"): Cloda, """{"TYPE":"PLOOBA","THING":"poo"}""")
  }

  it should "support custom product field naming rules" in {
    roundtrip(Plooba("poo"), """{"THING":"poo"}""")
  }

  it should "support default value for product" in {
    """{"thing": 1, "label": "test"}""".parseJson
      .convertTo[Billy] shouldBe Billy(1, "test", 5, Mickey("mouse", 3))
  }

  it should "support default parameters on coproducts" in {
    """{"type":"Mozz","n":"wow"}""".parseJson
      .convertTo[Paff] shouldBe Mozz("wow", 4, Paf("hello"))
  }

  it should "support custom missing value rules" in {
    roundtrip(Huey(Quack, None), """{"duck":null,"witch":null}""")
    roundtrip(Dewey(Quack, None), """{"duck":null}""")
    roundtrip(Louie(Quack, None), """{}""")
    roundtrip(Bluey(Quack, None), """{"duck":null,"witch":null}""")

    val nulls = """{"duck":null,"witch":null}""".parseJson
    nulls.convertTo[Huey] shouldBe Huey(Quack, None)
    nulls.convertTo[Dewey] shouldBe Dewey(Quack, Some(Quack))
    nulls.convertTo[Louie] shouldBe Louie(Quack, None)
    nulls.convertTo[Bluey] shouldBe Bluey(Quack, None)

    val partial = """{"duck":null}""".parseJson
    intercept[DeserializationException] {
      partial.convertTo[Huey] shouldBe Huey(Quack, None)
    }
    partial.convertTo[Dewey] shouldBe Dewey(Quack, None)
    partial.convertTo[Louie] shouldBe Louie(Quack, None)
    partial.convertTo[Bluey] shouldBe Bluey(Quack, None)

    val empty = """{}""".parseJson
    intercept[DeserializationException] {
      empty.convertTo[Huey] shouldBe Huey(Quack, None)
    }
    intercept[DeserializationException] {
      empty.convertTo[Dewey] shouldBe Dewey(Quack, None)
    }
    empty.convertTo[Louie] shouldBe Louie(Quack, None)
    intercept[DeserializationException] {
      empty.convertTo[Bluey] shouldBe Bluey(Quack, None)
    }
  }

  it should "fail when missing required (null) values" in {
    val noduck  = """{"witch":null}""".parseJson
    val nowitch = """{"duck":null}""".parseJson

    intercept[DeserializationException] {
      noduck.convertTo[Huey]
    }
    intercept[DeserializationException] {
      noduck.convertTo[Dewey]
    }
    noduck.convertTo[Louie] shouldBe Louie(Quack, None)
    intercept[DeserializationException] {
      noduck.convertTo[Bluey]
    }

    intercept[DeserializationException] {
      nowitch.convertTo[Huey]
    }
    nowitch.convertTo[Dewey] shouldBe Dewey(Quack, None)
    nowitch.convertTo[Louie] shouldBe Louie(Quack, None)
    nowitch.convertTo[Bluey] shouldBe Bluey(Quack, None)
  }

  it should "prefer user customisable JsonFormats" in {
    roundtrip(Schpugel("foo"), """"foo"""")
  }

  it should "prefer user customisable RootJsonFormats" in {
    roundtrip(Smim("foo"), """{"smim":"foo"}""")
  }

  it should "prefer non-trivial user customisable JsonFormats" in {
    // uncomment the next line as an alternative to the redefinition of FloomaFormat
    // def familyFormat = ???
    roundtrip(Flooma("aha"): Smash, """{"flooma":"aha"}""") // via our JsonFormat[Smash]
  }

  it should "fail to compile when a member of the family cannot be serialised" in {
    // this is an example of when this library can be very
    // frustrating. The compiler error when an implicit cannot be
    // created is always the least specific type. Here we're missing a
    // formatter for UUIDs but the compiler warns about Primates. If
    // we narrow it down to Hominidae it also errors... but finding
    // these problems is a human driven search game.

    shapeless.test.illTyped(
      """roundtrip(Hominidae(UUID.randomUUID): Primates)""",
      ".*could not find implicit value for evidence parameter of type spray.json.JsonFormat\\[spray.json.examples.Primates\\].*"
    )

    shapeless.test.illTyped(
      """roundtrip(Hominidae(UUID.randomUUID))""",
      ".*could not find implicit value for evidence parameter of type spray.json.JsonFormat\\[spray.json.examples.Hominidae\\].*"
    )
  }

  ///////////////////////////////////////////////
  // non-trivial AST (in separate file)
  it should "support an example ADT" in {
    import ExampleAst._

    roundtrip(SpecialToken: TokenTree, """{"type":"SpecialToken"}""")

    val fieldTerm = FieldTerm("thing is ten", DatabaseField("THING"), "10")
    roundtrip(
      fieldTerm: TokenTree,
      """{"type":"FieldTerm","text":"thing is ten","field":{"column":"THING"},"value":"10"}"""
    )

    val and = AndCondition(fieldTerm, fieldTerm, "wibble")
    roundtrip(
      and: TokenTree,
      """{"type":"AndCondition","left":{"type":"FieldTerm","text":"thing is ten","field":{"column":"THING"},"value":"10"},"right":{"type":"FieldTerm","text":"thing is ten","field":{"column":"THING"},"value":"10"},"text":"wibble"}"""
    )
  }
}
