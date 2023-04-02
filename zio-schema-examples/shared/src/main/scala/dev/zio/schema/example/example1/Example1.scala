package dev.zio.schema.example.example1


import zio._
import zio.schema.codec.DecodeError
import zio.schema.{DeriveSchema, Schema, TypeId}
import zio.stream.ZPipeline

import scala.reflect.ClassTag

/**
 * Example 1 of ZIO-Schema:
 *
 * In this example we define our basic Domain Model.
 * Then we'll show how to manually construct a Schema for the given domain model
 * and how to derive one using macros.
 *
 * We'll then use the Schema to transform instances of our classes (Person) to/from JSON and Protobuf.
 */

object Domain {
  sealed trait PaymentMethod

  final case class Person(name: String, age: Int)

  final case class Customer(person: Person, paymentMethod: PaymentMethod)

  object PaymentMethod {
    final case class CreditCard(number: String, expirationMonth: Int, expirationYear: Int) extends PaymentMethod
    final case class WireTransfer(accountNumber: String, bankCode: String)                 extends PaymentMethod
  }
}

import dev.zio.schema.example.example1.Domain._

object ManualConstruction {
  import Domain.PaymentMethod._
  import zio.schema.Schema._

  val schemaPerson: Schema[Person] = Schema.CaseClass2[String, Int, Person](
    TypeId.parse("dev.zio.schema.example.example1.Domain.Person"),
    field01 =
      Schema.Field[Person, String]("name", Schema.primitive[String], get0 = _.name, set0 = (p, v) => p.copy(name = v)),
    field02 = Schema.Field[Person, Int]("age", Schema.primitive[Int], get0 = _.age, set0 = (p, v) => p.copy(age = v)),
    construct0 = (name, age) => Person(name, age)
  )

  val schemaPaymentMethodWireTransfer: Schema[WireTransfer] = Schema.CaseClass2[String, String, WireTransfer](
    TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod.WireTransfer"),
    field01 = Schema.Field[WireTransfer, String](
      "accountNumber",
      Schema.primitive[String],
      get0 = _.accountNumber,
      set0 = (p, v) => p.copy(accountNumber = v)
    ),
    field02 = Schema.Field[WireTransfer, String](
      "bankCode",
      Schema.primitive[String],
      get0 = _.bankCode,
      set0 = (p, v) => p.copy(bankCode = v)
    ),
    construct0 = (number, bankCode) => PaymentMethod.WireTransfer(number, bankCode)
  )


  // TODO do logging of the types for each parameter/field
  import com.typesafe.scalalogging._



  object ObjParamsCreditCard {

    val typeId_creditCard: TypeId = TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod.CreditCard")

    val field1_creditCard: Field[CreditCard, String] = Schema.Field[CreditCard, String](
      name0 = "number",
      schema0 = Schema.primitive[String],
      //get0: (R => A) --- (CreditCard => String)
      get0 = (r:CreditCard) => r.number,
      // set0: (R, A) => R ----- (CreditCard, String) => CreditCard
      set0 = (cc: CreditCard, s: String) => cc.copy(number = s)
    )

    val logger_field1_cc: Logger = Logger[Field[CreditCard, String]] // TODO how to return class type from the obj
    // field1_creditcard??


    //"str".asInstanceOf[field1_creditCard.getClass]
    logger_field1_cc.info(s"name0 param = ${field1_creditCard.name.toString}")
    logger_field1_cc.info(s"schema0 param = ${field1_creditCard.schema.toString}")
    logger_field1_cc.info(s"get0 param = ${field1_creditCard.get.toString()}")
    logger_field1_cc.info(s"set0 param = ${field1_creditCard.set.toString()}")
    logger_field1_cc.info(s"set0.getClass.getSimpleName = ${field1_creditCard.set.getClass.getSimpleName}")
    // TODO left off here

    val field2_creditCard: Field[CreditCard, RuntimeFlags] = Schema.Field[CreditCard, Int](
      name0 = "expirationMonth",
      Schema.primitive[Int],
      get0 = (cc: CreditCard) => cc.expirationMonth,
      set0 = (cc: CreditCard, rf: RuntimeFlags) => cc.copy(expirationMonth = rf)
    )

    val field3_creditCard: Field[CreditCard, RuntimeFlags] = Schema.Field[CreditCard, Int](
      "expirationYear",
      Schema.primitive[Int],
      get0 = (cc: CreditCard) => cc.expirationYear,
      set0 = (cc: CreditCard, rf: RuntimeFlags) => cc.copy(expirationYear = rf)
    )


    val construct0_creditCard: (String, Int, Int) => CreditCard =
      (number: String, expirationMonth: Int, expirationYear: Int) =>
        PaymentMethod.CreditCard(number, expirationMonth, expirationYear)
  }
  import ObjParamsCreditCard._

  //TODO ASSERT  properties of zioschema constrcuts - e.g. that caseclass3 has 3 fields and a construct (assert type
  // params using reflection/typetag)
  val schemaPaymentMethodCreditCard: Schema[CreditCard] = Schema.CaseClass3[String, Int, Int, CreditCard](
    id0 = typeId_creditCard,
    field01 = field1_creditCard,
    field02 = field2_creditCard,
    field03 = field3_creditCard,
    construct0 = construct0_creditCard
  )

  //-----------------------------------------------

  //TODO - looking here for way to make this composition of types more explicit using specs and proptesting (e.g.
  // assert that schema[paymentmethod] is an enum because it has the subtypes whil the other two have no subtype so
  // can just be case classes)

  val schemaPaymentMethod: Schema[PaymentMethod] =
    Schema.Enum2[PaymentMethod.CreditCard, PaymentMethod.WireTransfer, PaymentMethod](
      id = TypeId.parse("dev.zio.schema.example.example1.Domain.PaymentMethod"),
      case1 = Case[PaymentMethod, PaymentMethod.CreditCard](
        id = "CreditCard",
        schema = schemaPaymentMethodCreditCard,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.CreditCard],
        construct = pc => pc.asInstanceOf[PaymentMethod],
        isCase = _.isInstanceOf[PaymentMethod.CreditCard],
        annotations = Chunk.empty
      ),
      // TODO - meaning of R => A construct types? what do they want to construct to?
      case2 = Case[PaymentMethod, PaymentMethod.WireTransfer](
        id = "WireTransfer",
        schema = schemaPaymentMethodWireTransfer,
        unsafeDeconstruct = pm => pm.asInstanceOf[PaymentMethod.WireTransfer],
        construct = pc => pc.asInstanceOf[PaymentMethod],
        isCase = _.isInstanceOf[PaymentMethod.WireTransfer],
        annotations = Chunk.empty
      ),
      annotations = Chunk.empty
    )

  val schemaCustomer: Schema[Customer] = Schema.CaseClass2[Person, PaymentMethod, Customer](
    TypeId.parse("dev.zio.schema.example.example1.Domain.Customer"),
    field01 =
      Schema.Field[Customer, Person]("person", schemaPerson, get0 = _.person, set0 = (p, v) => p.copy(person = v)),
    field02 = Schema.Field[Customer, PaymentMethod](
      "paymentMethod",
      schemaPaymentMethod,
      get0 = _.paymentMethod,
      set0 = (p, v) => p.copy(paymentMethod = v)
    ),
    construct0 = (person, paymentMethod) => Customer(person, paymentMethod)
  )

  val schemaPersonDictionary: Schema[scala.collection.immutable.Map[String, Person]] =
    Schema.map(
      Schema.primitive[String],
      Schema[Person](schemaPerson)
    )

}

object MacroConstruction {

  implicit val schemaPerson: Schema[Person] = DeriveSchema.gen[Person]

  val schemaPaymentMethod: Schema[PaymentMethod] = DeriveSchema.gen[PaymentMethod]

  val schemaCustomer: Schema[Customer] = DeriveSchema.gen[Customer]

  val schemaPersonDictionaryFromMacro: Schema[scala.collection.immutable.Map[String, Person]] =
    DeriveSchema.gen[Map[String, Person]]

}

object JsonSample extends zio.ZIOAppDefault {
  import ManualConstruction._
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _                      <- ZIO.unit
      person                 = Person("Michelle", 32)
      personToJsonTransducer = JsonCodec.schemaBasedBinaryCodec[Person](schemaPerson).streamEncoder
      _ <- ZStream(person)                     // ZStream[Any, Nothing, Person]
           .via(personToJsonTransducer)   // ZStream[Any, Nothing, Byte]
           .via(ZPipeline.utf8Decode)     // ZStream[ANy, CharacterCodingException, ...]
           .foreach(ZIO.debug(_))                  // ZIO[Any, CharacterCodingException, Unit]
    } yield ExitCode.success
}

object ProtobufExample extends ZIOAppDefault {
  import ManualConstruction._
  import zio.schema.codec.ProtobufCodec
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _      <- ZIO.unit
      _      <- ZIO.debug("protobuf roundtrip")
      person = Person("Michelle", 32)

      personToProto: ZPipeline[Any, Nothing, Person, Byte] = ProtobufCodec.protobufCodec[Person](schemaPerson).streamEncoder
      protoToPerson: ZPipeline[Any, DecodeError, Byte, Person] = ProtobufCodec.protobufCodec[Person](schemaPerson).streamDecoder

      newPerson <- ZStream(person)                              // ZStream[ANy, Nothing, Person]
                    .via(personToProto)                     // ZStream[Any, Nothing, Byte]
                    .via(protoToPerson)                     // ZStream[Any, DecodeError, Person]
                    .runHead                                         // ZIO[ANy, DecodeError, Option[Person]]
                    .some                                            // ZIO[Any, Option[DecodeError], Person]
                    .catchAll(error => ZIO.debug(error))      // ZIO[Any, Nothing, Any]
      _ <- ZIO.debug("is old person the new person? " + (person == newPerson).toString)
      _ <- ZIO.debug("old person: " + person)
      _ <- ZIO.debug("new person: " + newPerson)
    } yield ExitCode.success
}

object CombiningExample extends ZIOAppDefault {
  import ManualConstruction._
  import zio.schema.codec.{ JsonCodec, ProtobufCodec }
  import zio.stream.ZStream

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _      <- ZIO.unit
      _      <- ZIO.debug("combining roundtrip")
      person = Person("Michelle", 32)

      personToJson: ZPipeline[Any, Nothing, Person, Byte] = JsonCodec.schemaBasedBinaryCodec[Person](schemaPerson).streamEncoder
      jsonToPerson: ZPipeline[Any, DecodeError, Byte, Person] = JsonCodec.schemaBasedBinaryCodec[Person](schemaPerson).streamDecoder

      // TODO - studythisbetter using Given/When/Then scenario (specs2, see scalaprobprogr project, figaro book to
      //  sehow to use given/when/then). GOAL: to see GIVEN: schemaPerson, WHEN call straemEncoder, THEN expect
      //  personToProto. GOAL: to see how this process is built / operated / run through by breaking it down by hand
      //  (to see all the moving parts using tests where you can pin down each component).
      personToProto: ZPipeline[Any, Nothing, Person, Byte] = ProtobufCodec.protobufCodec[Person](schemaPerson).streamEncoder
      protoToPerson: ZPipeline[Any, DecodeError, Byte, Person] = ProtobufCodec.protobufCodec[Person](schemaPerson).streamDecoder

      // TODO copy as comments the shadow types that appear in IntelliJ (to the right of each line in this block)
      newPerson <- ZStream(person)
           .tap(v => ZIO.debug("input object is: " + v))
           .via(personToJson)
           .via(jsonToPerson)
           .tap(v => ZIO.debug("object after json roundtrip: " + v))
           .via(personToProto)
           .via(protoToPerson)
           .tap(v => ZIO.debug("person after protobuf roundtrip: " + v))
           .runHead
           .some
           .catchAll(error => ZIO.debug(error))
      _ <- ZIO.debug("is old person the new person? " + (person == newPerson).toString)
      _ <- ZIO.debug("old person: " + person)
      _ <- ZIO.debug("new person: " + newPerson)
    } yield ExitCode.success
}

object DictionaryExample extends ZIOAppDefault {

  import MacroConstruction._
  import zio.schema.codec.JsonCodec
  import zio.stream.ZStream
  import scala.collection.immutable.Map
  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _ <- ZIO.unit
      person: Person = Person("Mike", 32)

      dictionary: Map[String, Person] = Map("m" -> person)

      dictionaryToJson: ZPipeline[Any, Nothing, Map[String, Person], Byte] = JsonCodec
        .schemaBasedBinaryCodec[Map[String, Person]](schemaPersonDictionaryFromMacro)
        .streamEncoder

      jsonToDictionary: ZPipeline[Any, DecodeError, Byte, Map[String, Person]] = JsonCodec
        .schemaBasedBinaryCodec[Map[String, Person]](schemaPersonDictionaryFromMacro)
        .streamDecoder

      // TODO copy as comments the shadow types that appear in IntelliJ (to the right of each line in this block)
      newPersonDictionary <- ZStream(dictionary)
                              .via(dictionaryToJson)
                              .via(jsonToDictionary)
                              .runHead
                              .some
                              .catchAll(error => ZIO.debug(error))
      _ <- ZIO.debug("is old dictionary the new dictionary? " + (dictionary == newPersonDictionary).toString)
      _ <- ZIO.debug("old dictionary: " + dictionary)
      _ <- ZIO.debug("new dictionary: " + newPersonDictionary)
    } yield ExitCode.success
}
