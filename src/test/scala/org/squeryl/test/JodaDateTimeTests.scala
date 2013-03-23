package org.squeryl.test

import org.squeryl._
import org.squeryl.framework.{SchemaTester, RunTestsInsideTransaction}
import org.joda.time.DateTime

import PrimitiveTypeMode._

object JodaDateTimeTests {
  class JodaDateTimeAsProperty extends KeyedEntity[Long] {
    val id: Long = 0
    val datetime = new DateTime()
  }

  class JodaDateTimeAsId extends KeyedEntity[DateTime] {
    var id =  new DateTime()
    lazy val foreigns = TestSchema.datetimeOneToMany.left(this)
  }

  class JodaDateTimeAsForeignKey(val foreignJodaDateTime: DateTime) extends KeyedEntity[Long] {
    val id: Long = 0
  }

  object TestSchema extends Schema {
    val datetimeAsProperty = table[JodaDateTimeAsProperty]
    val datetimeAsId = table[JodaDateTimeAsId]
    val datetimeAsForeignKey = table[JodaDateTimeAsForeignKey]

    val datetimeOneToMany = oneToManyRelation(datetimeAsId, datetimeAsForeignKey).via(_.id === _.foreignJodaDateTime)

    override def drop = {
      Session.cleanupResources
      super.drop
    }
  }

}

abstract class JodaDateTimeTests extends SchemaTester with RunTestsInsideTransaction{
  import JodaDateTimeTests._

  final def schema = TestSchema

  test("JodaDateTimeAsProperty") {
    import TestSchema._

    val testObject = new JodaDateTimeAsProperty
    testObject.save

    testObject.datetime should equal(datetimeAsProperty.where(_.id === testObject.id).single.datetime)

    // TODO
    //testObject.datetime should equal(datetimeAsProperty.where(_.datetime in List(testObject.datetime)).single.datetime)
  }

  test("JodaDateTimeAsId") {
    import TestSchema._

    val testObject = new JodaDateTimeAsId

    testObject.save

    testObject.id should equal(datetimeAsId.where(_.id === testObject.id).single.id)

    // TODO
    //testObject.id should equal(datetimeAsId.where(_.id in List(testObject.id)).single.id)

    val lookup = datetimeAsId.lookup(testObject.id)
    lookup.get.id should equal(testObject.id)
  }

  test("JodaDateTimeAsForeignKey") {
    import TestSchema._

    val primaryObject = new JodaDateTimeAsId
    primaryObject.save

    val secondaryObject = new JodaDateTimeAsForeignKey(primaryObject.id)
    datetimeAsForeignKey.insert(secondaryObject)

    secondaryObject.id should equal(datetimeAsForeignKey.where(_.id === secondaryObject.id).single.id)

    List(secondaryObject.id) should equal(primaryObject.foreigns.map(_.id).toList)
  }
}
